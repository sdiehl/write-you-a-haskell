# Chapter 25: Future Work

We started with a calculator. Now we have a language with inferred types,
algebraic data, records, type classes, and a compiler that produces native code.
We've even built an optimizer and a garbage collector. That's quite a journey!

The best part is that the compiler is now yours to experiment with. You know
where its decisions are made, and you have smaller chapter versions to return
to when you want to try a different approach. Here are a few directions that
might be fun to explore next.

## Where Next?

### Explicit Delay and Laziness

Sometimes we really do want to describe a computation and leave it for later.
I'd explore that through explicit delay, using call-by-push-value to distinguish
values from computations [@levy2001]. A programmer could then see where a
computation is suspended and where it is forced.

Sharing the result is another choice. Call-by-need needs a way to remember that
a suspension has already been evaluated, as well as a way to handle recursive
forcing. Our closure representation is a useful starting point, though adding
a thunk object alone won't do all of that work. An STG-like intermediate
language would give these operations a place to live.

### Modules and Separate Compilation

As your programs grow, you'll probably get tired of putting everything in one
file. Modules would let us group definitions, expose an interface, and hide the
rest. Separate compilation would let us rebuild one part without compiling the
whole program again.

We already have unique names within a compilation. The next step is deciding
which identities and type information must survive between compilations.
Classes make this especially interesting. An imported function may depend on
instances, so we need a clear rule for which instances it sees. A small module
system with explicit interfaces would be a good first experiment.

### Rows, Effects, and Classes

Our records have names and fixed fields. What if a function could accept any
record with an `x` field? Row polymorphism is one way to express that. Effect
rows open another line of exploration, letting types describe which effects a
computation may perform.

These ideas would take us beyond the constraint language we've built, but the
basic habit still applies. Find a small program you'd like to express, write
down its intended type, and work out what information inference needs.

You could also explore multi-parameter classes, functional dependencies, or
type families. Our Haskell 98 restrictions keep instance search manageable
[@haskell2010]. Relaxing one at a time gives you a chance to understand what
it was buying us before the next complication arrives.

### Complete Tail Calls

Known calls already use LLVM's tail calling convention. Unknown calls pass
through the C runtime, where a chain of calls can still accumulate stack frames.
It would be satisfying to close that gap.

One route is to emit apply dispatch in LLVM too. Exact calls, PAP entry, and
oversaturation could then share a tail-call protocol. Oversaturation is the
interesting case. We must keep the leftover arguments somewhere while the first
call returns a function to receive them. The eval/apply model gives us a place
to start [@marlow2004].

### Specialization and Unboxed Values

We can keep arithmetic intermediates in native registers, but calls still pass
uniform words. When the compiler knows the concrete types at a call, it could
create a specialized version with unboxed arguments. Knowing a dictionary could
also turn a method selection into a direct call.

There is plenty of room to experiment here. There is also plenty of room to
make the executable enormous. Specialized copies cost code space, so a useful
experiment should measure both the work saved and the code added. Start with
one small numeric function and inspect the result before building a general
specializer.

### Optimization and Debugging

Inlining can expose a constant, a constructor, or a dictionary method that the
next pass can simplify. It can also duplicate work. The traces and machine-step
counts from Chapter 23 give us a way to investigate that tradeoff. An optimization
should earn its place by helping programs we can actually run.

A source debugger would be another rewarding project. Imagine stepping through
Fun code while the native executable runs underneath. DWARF can connect machine
instructions to source locations, but we'd also need to describe scopes and
variable locations. Start without optimization. Explaining where a variable
went after the optimizer removed it is a problem we can enjoy later.

## Pick a Starting Point

You don't have to rebuild the whole compiler to try one of these ideas. Each
has a natural place to begin.

| Direction        | Where to Begin                                       |
| :--------------- | :--------------------------------------------------- |
| Delay            | Core evaluation and closure entry                    |
| Modules          | Renamed identities and compilation-unit metadata     |
| Rows or effects  | Types, constraints, and elaboration                  |
| Class extensions | Instance checking and constraint solving             |
| Unknown-call TCO | Flat calls, slow entries, and runtime apply          |
| Specialization   | Type evidence before erasure and calling conventions |
| Debugging        | Source spans through Flat and LLVM emission          |

The [IR Reference](appendix-c-ir.md) and [Runtime ABI](appendix-d-runtime.md)
are there when you need to look up the details.

Congratulations on making it through! Building a compiler brings together a
remarkable range of ideas, and you've followed them from source text all the
way to a running program. I hope some of the machinery that once looked
intimidating now looks like something you could sit down and build.

Functional programming has advanced a great deal since Haskell 2010, and we've
only scratched the surface. There are beautiful ideas left to discover in its
languages, type systems, and implementations. Pick one that makes you curious
and try it. This is only the beginning of your compiler journey.
