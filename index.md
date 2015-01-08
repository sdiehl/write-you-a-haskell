<!--
<p class="logo">
![](img/cover_image.jpg)
</p>
-->

<p class="halfbreak">
</p>

<p class="logo">
![](img/Haskell-Logo.png)
</p>

<p class="logo">
![](img/cover.png)
</p>

<p class="center tagline">
Building a modern functional compiler from first principles.
</p>

<p class="center author">
[Stephen Diehl](https://twitter.com/smdiehl)
</p>

<p class="halfbreak">
</p>

<p class="break">
</p>

In 2014 I wrote a [short tutorial](http://www.stephendiehl.com/llvm/) about
building a small imperative language in Haskell that compiled into LLVM. I was
extremely happy with the effect the tutorial seemed to have, and the warm
response I got from so many people was very encouraging.

I've done a great bit of thinking about what the most impactful topic I could
write about in 2015 could be; and decided throughout this year I will follow up
with an large endeavor for another project-based tutorial on *building a simple
functional programming language from first principles*.

This is a nontrivial topic and is unfortunately very much underserved, the
knowledge to build such a modern functional language is not widely disseminated
among many programmers.  The available resources most often discuss language
theory in depth while completely glossing over the engineering details. I wished
to write a project-based tutorial that included the engineering details and left
the reader with a fully functional toy language at the end that could be
extended for further projects.

We will build a small functional language called *Fun* which is a partial
Haskell 2010 toy language; complete with a parser, type inference, datatypes,
pattern matching, desugaring, typeclasses, higher-kinded types, monadic IO,
arbitrary-rank polymorphism, records, Core language, STG intermediate language,
lazy evaluation, interpreter, native code generator, a runtime, and several
optimization passes.

As with most of my writing, this is the pre-edited rough cut version, which I
will refine over time. The compiler itself is fully complete as of December
2014, but each of the chapters (28 in total) will be staggered on a
month-by-month basis to allow me to progressively refine the text with feedback
from readers and testers.  The later chapters on runtime and code generation are
somewhat involved and much longer.

December
--------

* [Chapter 1: Introduction](000_introduction.html)
* [Chapter 2: Haskell Basics](001_basics.html)
* [Chapter 3: Parsing](002_parsers.html)
* [Chapter 4: Lambda Calculus](003_lambda_calculus.html)

January
-------

* [Chapter 5: Type Systems](004_type_systems.html)
* [Chapter 6: Evaluation](005_evaluation.html)
* [Chapter 7: Hindley-Milner Inference](006_hindley_milner.html)
* [Chapter 8: Design of ProtoHaskell](007_path.html)

February 
--------

* Chapter 9: Extended Parser
* Chapter 10: Custom Datatypes
* Chapter 11: Renamer
* Chapter 12: Pattern Matching & Desugaring

March 
-----

* Chapter 13: System-F
* Chapter 14: Type Classes
* Chapter 15: Core Language


April 
-----

* Chapter 16: Kinds
* Chapter 17: Haskell Type Checker
* Chapter 18: Core Interpreter
* Chapter 19: Prelude

May 
----

* Chapter 20: Design of Lazy Evaluation
* Chapter 21: STG

June 
----

* Chapter 22: Compilation
* Chapter 23: Design of the Runtime

July 
----

* Chapter 24: Imp
* Chapter 25: Code Generation ( C )
* Chapter 26: Code Generation ( LLVM )

August 
------

* Chapter 27: Row Polymorphism & Effect Typing
* Chapter 28: Future Work

***

Addendum
--------

* [Contributing](contributing.html)
* [Credits](credits.html)

License
-------

This written work is licensed under a <a rel="license"
href="http://creativecommons.org/licenses/by-nc-sa/4.0/">Creative Commons
Attribution-NonCommercial-ShareAlike 4.0 International License</a>. You may
reproduce and edit this work with attribution for all non-commercial purposes.

The included source is released under the terms of the [MIT License](http://opensource.org/licenses/MIT).
