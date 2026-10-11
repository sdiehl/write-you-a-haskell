# Chapter 25: Future Work

We started with arithmetic expressions and ended with a language that compiles to
native code. Along the way, we built type inference, pattern matching,
dictionaries, closures, an optimizer, and a garbage collector. Each piece is
small enough to study, and together they make a working compiler. That is a lot
to have built.

You now have a foundation for trying out your own ideas. Perhaps you want a new
language feature, a faster runtime, or better tools for understanding a program.
Here are a few directions you could take Fun next.

## Where Next?

### Explicit Delay and Laziness

A lazy implementation would need closures for suspended computations, rules for
forcing them, and an update protocol if repeated forcing must share the result.
It would also need to account for retained environments and black holes during
recursive evaluation. Adding a thunk constructor alone does not provide
call-by-need.

An STG-like intermediate language would make those closures and entry protocols
explicit. For a language design experiment, I would instead start with opt-in
delay and a call-by-push-value account of values and computations [@levy2001].
That gives delayed evaluation a visible boundary. Fun's strict semantics remain
the reference, rather than changing silently beneath existing programs.

### Modules and Separate Compilation

Our globals belong to one compilation unit. Modules need exported signatures,
qualified identities, an interface format, and a linker-visible naming policy.
Type classes add a coherence question: which instance environment does an
imported binding assume? Native objects alone do not answer it. Start with
explicit interfaces and a fixed rule for instance visibility, then test whether
separately compiled and whole-program builds agree.

### Rows, Effects, and Classes

Nominal records make each projection a known field index. Row polymorphism would
replace that assumption with extensible record types and evidence for field
access. Effect rows would change the account of purity and generalization too.
Both deserve their own constraint language and solver, not extra cases hidden in
ordinary type unification.

Multi-parameter classes, functional dependencies, and type families likewise
change inference and termination obligations. Our Haskell 98 restrictions keep
instance resolution predictable [@haskell2010]. Relax one restriction at a time,
state a termination or coverage condition, and keep ambiguity errors meaningful.

### Complete Tail Calls

Known calls use LLVM's tail calling convention. Unknown calls cross into the C
runtime's eval/apply routine, which must resume after calling a slow entry. That
boundary can accumulate stack frames.

Emitting apply dispatch in LLVM could put exact calls, PAP entry, and
oversaturation continuations under one tail-call protocol. The difficult case
is oversaturation: the remaining arguments must survive while the first call
returns a function. The eval/apply model is the place to begin [@marlow2004].
Test constant stack on unknown recursive calls, not only direct self recursion.

### Specialization and Unboxed Values

Chapter 23 keeps arithmetic intermediates native within a function. Its calling
convention still transports words. Specializing a function at concrete argument
types could remove dictionary calls and introduce unboxed entry points. It also
creates more code and more interfaces to maintain.

Monomorphization makes a different tradeoff by producing type-specific copies.
Either approach needs a budget for code growth, a treatment of recursive
specializations, and a clear fallback to the uniform ABI. Allocation counts and
work-preservation tests should accompany throughput measurements.

### Optimization and Debugging

Inlining exposes constants, known constructors, and dictionary methods. It can
also duplicate work, inflate code, and destroy a useful source location.
Chapter 23's traces and machine counts are a starting point for testing a
cost-aware inliner. Add one rewrite at a time and keep its legality separate
from its profitability.

DWARF would let a debugger relate native instructions to Fun source. Our spans
already identify failures, but useful debugging also needs lexical scopes,
variable locations, and a policy for optimized-away values. Begin at O0 with
source stepping and stack traces. Do not promise that every source binding has
a physical location after optimization.

## Pick a Starting Point

Each of these projects has a place to begin in the compiler you already know:

| Direction        | First boundary to revisit                         |
| :--------------- | :------------------------------------------------ |
| Delay            | Core evaluation and closure entry                 |
| Modules          | Renamed identities and compilation-unit metadata  |
| Rows or effects  | Type representation, constraints, and elaboration |
| Class extensions | Instance validity and context reduction           |
| Unknown-call TCO | Flat calls, slow entries, and runtime apply       |
| Specialization   | Typed evidence before erasure and ABI selection   |
| Debugging        | Source spans through Flat and LLVM emission       |

The [IR Reference](appendix-c-ir.md) and [Runtime ABI](appendix-d-runtime.md)
record the contracts an extension would need to change.

Congratulations on making it through. Building a compiler brings together a
remarkable range of ideas, and you have followed them all the way from source
text to a running program.

Functional programming has advanced considerably since Haskell 2010. We have
only scratched the surface of its languages, type systems, and implementation
techniques. There is plenty left to explore, and now you have a compiler of your
own to experiment with. This is only the beginning of your compiler journey.
