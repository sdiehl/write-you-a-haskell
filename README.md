<p align="center">
  <a href="https://sdiehl.github.io/write-you-a-haskell/">
    <img src="https://github.com/sdiehl/write-you-a-haskell/raw/main/img/Haskell-Logo.png"/>
  </a>
</p>

<p align="center">
  <a href="https://sdiehl.github.io/write-you-a-haskell/">
    <img src="https://github.com/sdiehl/write-you-a-haskell/raw/main/img/cover.png"/>
  </a>
  <br/>
  <em>Building a modern functional compiler from first principles.</em>
</p>

<p align="center">
  <a href="https://twitter.com/smdiehl">Stephen Diehl</a>
</p>

[![Build Status](https://github.com/sdiehl/write-you-a-haskell/actions/workflows/ci.yml/badge.svg)](https://github.com/sdiehl/write-you-a-haskell/actions/workflows/ci.yml)
[![MIT License](http://img.shields.io/badge/license-mit-blue.svg)](https://github.com/sdiehl/write-you-a-haskell/blob/main/LICENSE)

Read Online:

* [**HTML**](https://sdiehl.github.io/write-you-a-haskell/)
* [**PDF**](https://sdiehl.github.io/write-you-a-haskell/write-you-a-haskell.pdf)

Releases
--------

* [Chapter 1: Introduction](https://sdiehl.github.io/write-you-a-haskell/ch01-introduction.html)
* [Chapter 2: Parsing](https://sdiehl.github.io/write-you-a-haskell/ch02-parsing.html)
* [Chapter 3: Lambda Calculus](https://sdiehl.github.io/write-you-a-haskell/ch03-lambda.html)
* [Chapter 4: Simply Typed Lambda Calculus](https://sdiehl.github.io/write-you-a-haskell/ch04-stlc.html)
* [Chapter 5: Evaluation](https://sdiehl.github.io/write-you-a-haskell/ch05-evaluation.html)
* [Chapter 6: Hindley-Milner Inference](https://sdiehl.github.io/write-you-a-haskell/ch06-hindley-milner.html)
* [Chapter 7: Design of Fun](https://sdiehl.github.io/write-you-a-haskell/ch07-fun.html)
* [Chapter 8: Layout and the Extended Parser](https://sdiehl.github.io/write-you-a-haskell/ch08-layout.html)
* [Chapter 9: The Renamer](https://sdiehl.github.io/write-you-a-haskell/ch09-renamer.html)
* [Chapter 10: Typed Core and Core Lint](https://sdiehl.github.io/write-you-a-haskell/ch10-typed-core.html)
* [Chapter 11: Datatypes and Pattern Matching](https://sdiehl.github.io/write-you-a-haskell/ch11-datatypes.html)
* [Chapter 12: Records](https://sdiehl.github.io/write-you-a-haskell/ch12-records.html)
* [Chapter 13: Kinds and Type Constructors](https://sdiehl.github.io/write-you-a-haskell/ch13-kinds.html)
* [Chapter 14: Type Classes](https://sdiehl.github.io/write-you-a-haskell/ch14-type-classes.html)
* [Chapter 15: Monads, Do Notation and IO](https://sdiehl.github.io/write-you-a-haskell/ch15-io.html)
* [Chapter 16: Pattern-Match Compilation](https://sdiehl.github.io/write-you-a-haskell/ch16-pattern-compilation.html)
* [Chapter 17: The Prelude and Deriving](https://sdiehl.github.io/write-you-a-haskell/ch17-prelude.html)
* [Chapter 18: A-Normal Form and Join Points](https://sdiehl.github.io/write-you-a-haskell/ch18-anf.html)
* [Chapter 19: Closure Conversion](https://sdiehl.github.io/write-you-a-haskell/ch19-closures.html)
* [Chapter 20: Data Representation and the Runtime](https://sdiehl.github.io/write-you-a-haskell/ch20-runtime.html)
* [Chapter 21: LLVM Code Generation](https://sdiehl.github.io/write-you-a-haskell/ch21-llvm.html)
* [Chapter 22: JIT and the REPL](https://sdiehl.github.io/write-you-a-haskell/ch22-jit.html)
* [Chapter 23: Optimization](https://sdiehl.github.io/write-you-a-haskell/ch23-optimization.html)
* [Chapter 24: A Precise Garbage Collector](https://sdiehl.github.io/write-you-a-haskell/ch24-garbage-collection.html)
* [Chapter 25: Future Work](https://sdiehl.github.io/write-you-a-haskell/ch25-future-work.html)

Building
--------

To build the chapters and the book, install Rust, `just`, Boehm GC and LLVM 22.
The pinned book tools are installed locally by `just bootstrap`.

```bash
$ just bootstrap
$ just build
$ just book
```

To generate the pdf:

```bash
$ just pdf
```

Contributing
------------

Any and all contributions are always welcome.  As always, I rely on the
perpetual kindness and goodwill of Haskellers (like you!) to help correct
grammar, clarify, and fix errors.

* [Contributing](https://github.com/sdiehl/write-you-a-haskell/blob/main/.github/CONTRIBUTING.md)

License
-------

<img src="http://mirrors.creativecommons.org/presskit/buttons/88x31/png/by-sa.png" width="140" alt="Artwork CC BY SA" />

This written work is licensed under a <a rel="license"
href="http://creativecommons.org/licenses/by-sa/4.0/">Creative Commons
Attribution-ShareAlike 4.0 International License</a>. You may
reproduce and edit this work with attribution.

The included source is released under the terms of the [MIT License](http://opensource.org/licenses/MIT).
