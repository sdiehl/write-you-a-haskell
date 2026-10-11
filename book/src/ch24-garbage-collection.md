# Chapter 24: A Precise Garbage Collector

> **New files**
>
> - `rt/src/copying.rs`
> - `src/liveness.rs`
> - `src/llvm/roots.rs`
>
> **Changed files**
>
> - `src/llvm/control.rs`
> - `src/llvm/values.rs`
> - `rt/src/heap.rs`
> - `rt/src/apply.rs`
> - `src/llvm/mod.rs`
> - `src/llvm/primitives.rs`
> - `src/native.rs`
> - `src/main.rs`
> - `rt/src/lib.rs`
> - `src/lib.rs`

## Motivation

Boehm lets a compiler allocate without describing every live pointer. That was a
useful boundary while we were building the language. Now we can move the boundary.
Our compiler knows which values may be used again, and our runtime knows which
object fields contain values. Together they can identify the live graph exactly.

Moving that graph makes the contract more demanding. Suppose a closure captures
a list and collection moves the list. Updating the closure's field is necessary,
but insufficient if the caller still holds the old address in an LLVM register.
Every reference used after collection must come from a location the collector
can update. This chapter is optional for readers using the Boehm backend, but it
completes the compiler/runtime account of memory [@henderson2002].

## The Language, Now

There is no new Fun syntax. Select the collector when building an executable:

```text
$ cd crates/ch24_gc
$ cargo run -q -- build tests/programs/gc/partial.fun \
    -o ../../build/partial --gc=copying --lint
$ FUN_GC_STRESS=1 ../../build/partial
100
αβγ
```

```fun
{{#include ../../crates/ch24_gc/tests/programs/gc/partial.fun}}
```

The ordinary `--gc=boehm` mode remains the default. Interpreters retain their Rust
representations, and the JIT retains its Boehm roots. Copying collection is a
native executable option. Its heap belongs to one thread, matching Fun's lack of
concurrency. Selecting a collector happens before global initialization.

## Theory

### Copy the Reachable Graph

Split the heap into two regions. Allocation advances a pointer in the active
region, called _from-space_. At collection, copy objects reachable from roots
into _to-space_. Replace each evacuated object's old header with a forwarding
marker and its first payload word with the new address. A second reference to
that object follows the forwarding address, preserving sharing and cycles.

Cheney's algorithm uses the copied objects themselves as a queue [@cheney1970]:

```text
from-space                         to-space
+------------+                     +----------+-------------+--------+
| FORWARD ---+-------------------->| scanned  | copied,     | free   |
| old object |                     | objects  | not scanned | space  |
+------------+                     +----------+-------------+--------+
                                   ^          ^             ^
                                   start      scan          free
```

Everything before `scan` has had its pointer fields updated. Between `scan` and
`free` are copied objects whose fields still need inspection. Scanning one object
can append more objects at `free`. When the pointers meet, the live graph is
complete. Swap the spaces and allocate after `free`. No recursive walk or native
stack growth is involved.

Object kinds tell the collector what to inspect. Constructor payloads contain
values. Closures begin with an untraced code address and arity, followed by
captures. PAPs contain a closure, a raw argument count, and arguments. Doubles
and strings contain no pointers. Nullary constructors and integers have their low
bit set, so the tracer skips them. Static string addresses lie outside from-space
and remain stationary.

### Make Roots Writable

A _shadow stack_ is an explicit stack of root ranges, maintained alongside the
machine stack. Each generated function owns an array of tagged-word slots.
Native integer and floating-point intermediates need no root. A tagged result
that might be a pointer is stored in a slot, and subsequent uses reload it.
Collection changes the slot, so a reload sees the new address.

Arguments need the same care. Boxing a later Double argument may move an earlier
argument. Codegen first saves converted arguments in scratch roots, then reloads
all of them for the call. Constructor allocation protects its fields across the
allocator call before storing their relocated addresses in the new object.
Rust-owned argument buffers use the same registration contract. Their API
returns copied words or raw ABI pointers, never a shared Rust slice into cells
that collection may update.

A returning function removes its frame. A known tail call removes the frame
_before_ entering its callee. There is no allocation between the final argument
loads and the callee's registration of its parameters. Thus tail recursion does
not accumulate shadow frames. Unknown calls still have the eval/apply limitation
from Chapter 21.

### Retain Only Live Values

Keeping every value until return would be safe for addresses, but could retain a
large dead structure inside a loop. Backward liveness determines which bindings
may be read again. Each Flat node contributes its operands. Its successors
contribute their live variables, excluding bindings defined on that edge.
A jump's successor is its join body, with the join parameters removed.

The equations are monotone. Begin with empty sets and revisit predecessors when
a successor changes. The least fixed point handles join back-edges as well as
ordinary branches. Before emitting a computation, codegen clears slots for dead
bindings and clears the scratch range. Liveness is conservative about control
flow, but every nonzero traced slot contains a tagged value, never an unboxed
numeric intermediate.

## Implementation

`liveness.rs` builds its control-flow graph with an explicit worklist. The solver
is another worklist, so source nesting does not become Rust recursion:

```rust
{{#include ../../crates/ch24_gc/src/liveness.rs:live-worklist}}
```

`llvm/roots.rs` assigns root slots and changes pointer-bearing locations from SSA
words into reloadable slots. These addresses remain stable for the activation:

```rust
{{#include ../../crates/ch24_gc/src/llvm/roots.rs:root-binding}}
```

The runtime's evacuation step either preserves a stationary word, follows a
forwarding address, or copies one object. The header's size includes only payload
words, so the allocation also reserves its header:

```rust
{{#include ../../crates/ch24_gc/rt/src/copying.rs:evacuate}}
```

The collector scans roots, then consumes the queue in to-space. The heap grows
when necessary, allocating the destination before moving any object. Existing
addresses are never invalidated by resizing active from-space.

```rust
{{#include ../../crates/ch24_gc/rt/src/copying.rs:cheney}}
```

Tests force collection before every allocation with `FUN_GC_STRESS=1`. Runtime
contracts check relocation, shared references, a cycle, raw Double bits that look
like an address, heap growth, and complete reclamation after removing roots.
The native corpus additionally exercises closures, PAPs, recursive closure
patching, globals, Unicode strings, and values surviving calls.

## Running It

```text
$ cd crates/ch24_gc
$ cargo run -q -- dump --pass=llvm tests/programs/gc/partial.fun --gc=copying
; ... root arrays, rt_roots_push, reloads, rt_roots_pop ...
$ cargo test -p fun-rt-24 --test copying
$ cargo test --test copying
```

The dump shows the root protocol before LLVM optimization. Stress collection is
intentionally expensive: repeatedly copying a growing live list takes quadratic
work. Use it to expose stale addresses, then measure normal collection separately.
Neither backend promises recovery from operating-system allocation failure.

Native copying executables link a separate `copying-only` runtime archive.
That build excludes the Boehm imports and link flag. The ordinary runtime rlib
still supports the JIT's Boehm heap. `just build` prepares both archives and
records each one's native link flags. A test checks the executable's dynamic
dependencies as well as its output under collection stress.

### Heap Growth

Correct tracing is not enough. If almost all of a semispace remains live, each
collection can copy a large graph while recovering only a few words. After a
capacity-triggered collection we grow the space when live data and the pending
allocation would occupy more than half of it. Stress collection still runs on
every allocation, but does not itself force growth. A regression test holds
seven eighths of a space live and bounds the total words copied during churn.
The bound measures collector work without relying on a machine-specific timer.

## Exercises

1. **Warm-up.** Trace two roots that point to the same object. Explain why only
   one copy is made and where each root ends up.
2. **Intermediate.** Draw a closure/PAP graph and mark every payload word the
   collector must skip. Explain why low-bit testing alone is insufficient for
   raw metadata and Double bits.
3. **Intermediate.** Give a join back-edge that needs more than one liveness
   iteration. Show the sets as they grow.
4. **Advanced.** Reuse root slots whose live ranges do not overlap. Preserve the
   existing stress tests and compare generated frame sizes.
5. **Advanced.** Design a generational extension. Identify the write barrier
   required by recursive closure patching before implementing it.

## Further Reading

Cheney supplies the nonrecursive copying algorithm [@cheney1970]. Henderson
explains explicit roots in an otherwise uncooperative compilation environment
[@henderson2002]. The handbook puts copying, generations, and concurrent
collection in context [@gc2023]. Compare the root contract with Boehm and Weiser's
conservative approach [@boehm1988].
