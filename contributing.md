Contributing
============

As always, I rely on the perpetual kindness and goodwill of Haskellers (like
you!) to help correct grammar, clarify, and fix errors.

Git Repo
--------

*Simple Fixes*

For most fixes you can simply edit the Markdown files at the toplevel of the Git
repo and then submit a pull request on Github. There should be no need to
compile the text locally. I will try to merge the changes quickly and rebuild
the text daily.

If you would like to add your name to
[CONTRIBUTORS.md](https://github.com/sdiehl/write-you-a-haskell/blob/master/CONTRIBUTORS.md)
submit this along with your pull request.

*Complex Fixes*

If you'd like to submit a change to the publishing software around the text,
then clone the repo. You will need a local copy of Pandoc library to use the
build system.

```bash
$ git clone https://github.com/sdiehl/write-you-a-haskell.git
$ make        # Makes all html files
$ make pdf    # Makes a pdf containing all the chapters
```

Pandoc
------

The text is written in the Markdown language and handled with the Pandoc
processing library, which is itself written in Haskell!

The tutorial uses a custom pandoc preprocessor contained in *includes.hs*.  This
allows us to include and slice fragments of code from files in the src
directory.

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

Equations can be included in display form:

```latex
$$
\int_\Omega \mathrm{d}\omega = \oint_{\partial \Omega} \omega
$$
```

$$
\int_\Omega \mathrm{d}\omega = \oint_{\partial \Omega} \omega
$$

Or in inline form (like $a^2 + b^2 = c^2$) with single dollar signs. Specially
there must be no spaces around the dollar signs otherwise Pandoc will not parse
it properly.

```latex
$a^2 + b^2 = c^2$
```

For most definitions, the ``aligned`` block is used:

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

LaTeX
-----

The LaTeX styling is sourced from the ``template.latex`` file, which is an
extension of Pandoc's default template with some custom modifications.

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
