Contributing
============

As always, I rely on the perpetual kindness and goodwill of other compiler nerds (like
you!) to help correct grammar, clarify, and fix errors.

Git Repo
--------

*Simple Fixes*

For most fixes you can simply edit the Markdown files under `book/src` and then
submit a pull request on Github. There should be no need to compile the text
locally.

If you would like to add your name to
[CONTRIBUTORS.md](https://github.com/sdiehl/write-you-a-haskell/blob/main/CONTRIBUTORS.md)
submit this along with your pull request.

*Complex Fixes*

If you'd like to change the compiler or the publishing tooling around the
text, then clone the repo. You will need Rust, `just`, Boehm GC and LLVM 22;
the pinned book tools are installed locally by `just bootstrap`. See
[BUILDING.md](https://github.com/sdiehl/write-you-a-haskell/blob/main/BUILDING.md)
for the details.

```bash
$ git clone https://github.com/sdiehl/write-you-a-haskell.git
$ just bootstrap
$ just build   # Builds every chapter's compiler
$ just book    # Makes the html edition
$ just pdf     # Makes a pdf containing all the chapters
```

Pre-commit hooks run `rustfmt`, `clippy` and `dprint`; install them with
`pre-commit install` so your pull request passes CI.

Chapters
--------

Each chapter's compiler lives in its own crate under `crates`. A chapter
inherits the previous chapter's test corpus; `cargo xtask check-inherit NN`
checks that the inherited programs still behave as expected.

Text
----

The text is written in Markdown and built with mdBook for the html edition and
with Pandoc and Typst for the pdf. Code listings are included from the chapter
crates with named anchors, never line ranges, so that they stay in sync with
the compiler:

```markdown
{{#include ../../crates/ch03_lambda/src/eval.rs:whnf}}
```

Math is written in LaTeX and rendered with KaTeX in the html edition:

```latex
$$
\begin{aligned}
e :=\ & x            & \text{Var} \\
     & \lambda x. e  & \text{Lam} \\
     & e\ e          & \text{App} \\
\end{aligned}
$$
```

Typography
----------

* Body is set in *Libertinus Serif*.
* Headings are in *Libertinus Sans*.
* Math is in *Libertinus Math*.
* Code is typeset in *JetBrains Mono*.

The fonts are vendored under `pdf/fonts`.
