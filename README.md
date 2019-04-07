<p align="center">
  <a href="http://dev.stephendiehl.com/fun/">
    <img src="https://github.com/sdiehl/write-you-a-haskell/raw/master/img/Haskell-Logo.png"/>
  </a>
</p>

<p align="center">
  <a href="http://dev.stephendiehl.com/fun/">
    <img src="https://github.com/sdiehl/write-you-a-haskell/raw/master/img/cover.png"/>
  </a>
  <br/>
  <em>Building a modern functional compiler from first principles.</em>
</p>

<p align="center">
  <a href="https://twitter.com/smdiehl">Stephen Diehl</a>
</p>

[![Build Status](https://travis-ci.org/sdiehl/write-you-a-haskell.svg)](https://travis-ci.org/sdiehl/write-you-a-haskell)
[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/sdiehl/write-you-a-haskell?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=body_badge)
[![MIT License](http://img.shields.io/badge/license-mit-blue.svg)](https://github.com/sdiehl/write-you-a-haskell/blob/master/LICENSE)

Read Online:

* [**HTML**](http://dev.stephendiehl.com/fun/)
* [**PDF**](http://dev.stephendiehl.com/fun/WYAH.pdf)

Releases
--------

This is a work in progress.

* [Chapter 1: Introduction](http://dev.stephendiehl.com/fun/000_introduction.html)
* [Chapter 2: Haskell Basics](http://dev.stephendiehl.com/fun/001_basics.html)
* [Chapter 3: Parsing](http://dev.stephendiehl.com/fun/002_parsers.html)
* [Chapter 4: Lambda Calculus](http://dev.stephendiehl.com/fun/003_lambda_calculus.html)
* [Chapter 5: Type Systems](http://dev.stephendiehl.com/fun/004_type_systems.html)
* [Chapter 6: Evaluation](http://dev.stephendiehl.com/fun/005_evaluation.html)
* [Chapter 7: Hindley-Milner Inference](http://dev.stephendiehl.com/fun/006_hindley_milner.html)
* [Chapter 8: Design of ProtoHaskell](http://dev.stephendiehl.com/fun/007_path.html)
* [Chapter 9: Extended Parser](http://dev.stephendiehl.com/fun/008_extended_parser.html)
* [Chapter 10: Custom Datatypes](http://dev.stephendiehl.com/fun/009_datatypes.html)
* [Chapter 11: Renamer](http://dev.stephendiehl.com/fun/010_renamer.html)
* Chapter 12: Pattern Matching & Desugaring
* Chapter 13: System-F
* Chapter 14: Type Classes
* Chapter 15: Core Language
* Chapter 16: Kinds
* Chapter 17: Haskell Type Checker
* Chapter 18: Core Interpreter
* Chapter 19: Prelude
* Chapter 20: Design of Lazy Evaluation
* Chapter 21: STG
* Chapter 22: Compilation
* Chapter 23: Design of the Runtime
* Chapter 24: Imp
* Chapter 25: Code Generation ( C )
* Chapter 26: Code Generation ( LLVM )
* Chapter 27: Row Polymorphism & Effect Typing
* Chapter 28: Future Work

Building
--------

To generate the build scripts provision a cabal sandbox with pandoc in it. This
is done by the ``write-you-a-haskell.cabal`` and  ``stack.yaml`` files.

**Stack**

```bash
$ stack exec make
```

To generate the pdf, the LaTeX packages must be installed on the
system.

```bash
$ sudo apt-get install texlive-xetex texlive-latex-extra
$ sudo apt-get install xzdec
$ sudo tlmgr install zapfding
$ stack exec make pdf
```

Generation of the epub is also supported.

```bash
$ stack exec make epub
```


**Cabal**

```bash
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal exec bash
$ make
```

Contributing
------------

Any and all contributions are always welcome.  As always, I rely on the
perpetual kindness and goodwill of Haskellers (like you!) to help correct
grammar, clarify, and fix errors.

* [Contributing](http://dev.stephendiehl.com/fun/contributing.html)

License
-------

<img src="http://mirrors.creativecommons.org/presskit/buttons/88x31/png/by-nc-sa.png" width="140" alt="Artwork CC BY NC SA" />

This written work is licensed under a <a rel="license"
href="http://creativecommons.org/licenses/by-nc-sa/4.0/">Creative Commons
Attribution-NonCommercial-ShareAlike 4.0 International License</a>. You may
reproduce and edit this work with attribution for all non-commercial purposes.

The included source is released under the terms of the [MIT License](http://opensource.org/licenses/MIT).
