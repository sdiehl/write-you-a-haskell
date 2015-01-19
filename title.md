% Write You a Haskell
% Stephen Diehl
% 1/2/2015

<!--

Forward
=======

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

\begin{flushright}
\textit{Stephen Diehl}
\end{flushright}

\clearpage
-->
