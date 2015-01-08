As always, I rely on the perpetual kindness and goodwill of Haskellers (like
you!) to help correct grammar, clarify, and fix errors.

I am also very keen to the idea of having multiple language translations, if you
would like to translate the text into your native language please get in touch.

Git Repo
--------

```bash
$ git clone https://github.com/sdiehl/write-you-a-haskell.git
$ make
```

Pandoc
------

The text is written in the Markdown language and handled with the Pandoc
processing library, which is itself written in Haskell!

The tutorial uses a custom pandoc preprocessor contained in *includes.hs*.  This
allows us to include and slice fragments of code from files in the src directory
.

*LaTeX Macros*

```perl
~~~~ {literal="latex_macros"}
~~~~
```

*Including Whole Source Files*

```perl
~~~~ {.haskell include="includes.hs"}
~~~~
```

*Including Partial Source Files*

Will slice the lines 5-15 inclusively from the file *parsec.hs* with the Haskell
syntax highlighting.

```perl
~~~~ {.haskell slice="src/parsers/parsec.hs" lower=5 upper=15}
~~~~
```

*Math Typesetting*

```latex
$$
\begin{aligned}
e :=\ & x            & \text{Var} \\
     & \lambda x. e  & \text{Lam} \\
     & e\ e          & \text{App} \\
\end{aligned}
$$
```

Will generate:

$$
\begin{aligned}
e :=\ & x            & \text{Var} \\
     & \lambda x. e  & \text{Lam} \\
     & e\ e          & \text{App} \\
\end{aligned}
$$

Several type theory macros are also included on many pages:

```latex
$$
\infrule{ 0 : \t{nat}}{\infrule{\t{succ}(0) : \t{nat}}{ \t{succ}(\t{succ}(0)) : \t{nat} }}
$$
```

Will generate:

$$
\infrule{ 0 : \t{nat}}{\infrule{\t{succ}(0) : \t{nat}}{ \t{succ}(\t{succ}(0)) : \t{nat} }}
$$

Typography
----------

* Body is the system sans serif default preferring *Helvetica Neue Regular* or
  *Arial* if available.
* Subtitles are in *Signika*.
* Titles are in *Helvetica Neue Light*.
* Code is typeset in *Inconsolata* or *Monaco*.

Images
------

The images are drawn in SVG using Inkscape.

Graphs and blocks diagrams are generated using graphviz.

Preprocessor
------------

The source for the preprocessor is a simple bottom-up traversal replacement of
these custom markdown extensions:

~~~~ {.haskell include="includes.hs"}
~~~~

Reference Code
--------------

The subject of this text is largely build around the mini Haskell compiler.
Several other files are included for earlier chapters.
