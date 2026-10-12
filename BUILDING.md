# Building

Toolchain, build and REPL notes for the compiler and the book. The reader-facing
build instructions are in Chapter 1.

## Nix Development Shell

`nix develop` provides the pinned Rust toolchain, LLVM 22, Boehm GC, and build
tools. It supports Apple Silicon macOS and x86-64/AArch64 Linux. Enable Nix's
`nix-command` and `flakes` features, or pass
`--extra-experimental-features 'nix-command flakes'` to the command.
Run `just bootstrap` inside the shell to install the pinned book toolchain.

## Build

Rust 1.99.0, edition 2024, is pinned in `rust-toolchain.toml`.

```sh
just build     # builds the static runtimes first
cargo test --workspace
cargo clippy --workspace --all-targets -- -D warnings
cargo fmt --all -- --check
just bootstrap  # installs pinned book tools into .tools, not the global PATH
just book
just pdf       # build/write-you-a-haskell.pdf
```

`just` adds `.tools/bin` to PATH. The book toolchain is mdBook 0.5.3,
mdbook-katex 0.10.0, mdbook-bib 0.5.3, the author's pinned mdbook-include-rs
fork, pandoc 3.12.1 and Typst 0.15.1. Fonts and licenses are vendored.
Apple Silicon macOS and Linux on x86-64 or AArch64 are supported. A C compiler is needed
for Rust dependencies. Chapter 20 requires Boehm: install `libgc-dev` on
Debian/Ubuntu or `bdw-gc` with Homebrew before building the workspace. Set
`GC_LIB_DIR` for a nonstandard library location. Chapter 21 pins Inkwell 0.10.0
and LLVM 22. Install `llvm-22-dev` from [apt.llvm.org](https://apt.llvm.org/)
on Ubuntu, or `brew install llvm@22` on macOS. Set the prefix before Cargo commands:

```sh
export LLVM_SYS_221_PREFIX=/usr/lib/llvm-22       # Linux
# macOS: export LLVM_SYS_221_PREFIX="$(brew --prefix llvm@22)"
just build
```

Earlier chapter packages and the book can still be built without LLVM.
CI shards the chapters across Ubuntu jobs and runs chapter 24 on macOS as well.

## Current compiler

Chapter 24 adds an optional precise copying collector to the native compiler.
The O1 JIT and persistent REPL use Boehm. Core,
ANF and Flat remain independent execution engines; Flat remains the default
for `run`, while the REPL defaults to JIT. Native compilation uses `build`,
with `-O0` through `-O3` (default `-O2`). Both `fun` and `fun-ch24` use the same driver.
Use `build --gc=copying` for the optional precise collector. Set
`FUN_GC_STRESS=1` when running that executable to collect at every allocation.

```sh
cd crates/ch24_gc
cargo run -q -- run tests/programs/native/acceptance.fun --engine=jit --lint
cargo run -q -- repl --lint
cargo run -q -- build tests/programs/native/acceptance.fun -o ../../build/acceptance --lint
../../build/acceptance
cargo run -q -- dump --pass=llvm tests/programs/native/acceptance.fun
cargo test -p ch24_gc -p fun-rt-24
```

`just build` builds the chapter runtimes before the compilers. For Chapter 24
alone, first run `python3 scripts/build_runtime.py 24`. It records rustc's host
link flags in `build/native-static-libs-24.txt`. Native linking finds
`libfun_rt_24.a` beside the executable; `FUN_RT` overrides the archive path and
`CC` selects the linker. The script also builds `libfun_rt_24_copying.a` with
the `copying-only` feature and records its separate link flags. `--gc=copying`
selects that archive, which has no Boehm dependency. `FUN_RT_COPYING` overrides
its location. The JIT links the default runtime rlib directly and needs no
archive or linker at execution time. For release builds, run
`python3 scripts/build_runtime.py 24 --release`, then
`cargo build --release -p ch24_gc`.

REPL commands are `:t`, `:k`, `:core`, `:anf`, `:llvm`, `:load`, `:reset`,
`:engine`, and `:quit`. Enter declarations on one line or in an explicit-brace
block; use `:load FILE` for multiline source. Loading replaces definitions after
successful checking and initialization; it does not run the loaded `main`.
Evaluate `main` explicitly to execute that action. Duplicate user definitions
are rejected; Prelude overrides belong in the first block or a loaded file.
Imported types and syntactic arities cannot change; `:reset` releases the session. Compiler errors preserve accepted
bindings. Native runtime errors and `exitWith` terminate a JIT session, as they
do a native program. No unwinding crosses generated frames. JIT setup failure
falls back to Flat in the REPL; explicit `run --engine=jit` reports the failure.
`--fuel` and the test-only `--print-value` are interpreter facilities.

Each REPL input rechecks accepted source and emits a fresh module. Engines and
registered global roots survive until reset, preserving closures and initialized
values. This deliberately trades compilation speed and session memory for a
small, inspectable lifetime model; there is no incremental type checker.

Known saturated tail calls use LLVM `tailcc`; unknown calls through the runtime's
C eval/apply boundary do not guarantee tail-call elimination. LLVM emission,
Flat lint, Flat printing and owned Flat IR destruction use heap worklists,
validated on a 512 KiB stack. From Chapter 16, Core execution and value
reclamation use explicit worklists, tested on a 256 KiB stack. Application and
join spines also have bounded-stack compiler regressions. Other syntax, type,
pattern, and ANF traversals still recurse, so there is no end-to-end stack bound.

The generated HTML is `book/build/html/index.html`, and the PDF is
`build/write-you-a-haskell.pdf`. Compiler tests include inherited golden programs,
1,000 generated differential cases, bounded-stack recursion, and direct runtime
checks with forced garbage collections.

## Chapter workflow

```sh
cargo xtask new-chapter 08 layout
cargo xtask check-inherit 08
cargo xtask book-diff 08
cargo xtask pdf-collect
```

Each snapshot owns its compiler source and inherited corpus. Earlier snapshots
are edited only for bug fixes applied forward. `new-chapter` refuses to overwrite
an existing chapter. `check-inherit` checks sources and expectation sidecars,
requires reasons in `BREAKING.md` for changed fixtures, then runs the chapter's
tests under cargo-nextest when it is installed. A program inherited unchanged runs
on the default engine only (Flat where available); the chapter that introduces it
and chapter 24 run every engine. Pass `--full` to run every engine in any chapter.
The foundations (2–6) are separate languages, not inherited snapshots.

The HTML and PDF share preprocessed Markdown; PDF builds reject unresolved includes and TeX conversion
warnings. HTML citations use mdbook-bib; the bibliography page uses the pinned
Pandoc citeproc to retain emphasis, sorting, and clickable source links.

Code: MIT, see `LICENSE`. Prose: CC BY-SA 4.0, see `LICENSE-PROSE`.
