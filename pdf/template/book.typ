// Book typography. No network packages are required.
#let chapter-title = state("chapter-title", "")
#let section-title = state("section-title", "")
#let opener = state("opener", false)
#let blank-verso(p) = {
  let ended = query(metadata.where(value: "chapter-end")).any(m => (
    m.location().page() == p - 1
  ))
  let next = query(heading.where(level: 1)).any(h => (
    h.location().page() == p + 1
  ))
  let next-part = query(metadata.where(value: "part-page")).any(m => (
    m.location().page() == p + 1
  ))
  calc.even(p) and ended and (next or next-part)
}
#let book(body) = {
  set document(
    title: "Write You a Haskell",
    author: "Stephen Diehl",
  )
  set page(
    width: 7.5in,
    height: 9.25in,
    binding: left,
    margin: (inside: 0.9in, outside: 0.7in, top: 0.8in, bottom: 0.9in),
    header: context {
      let p = counter(page).get().first()
      let heads = query(heading.where(level: 1)).filter(h => (
        h.location().page() == p
      ))
      let parts = query(metadata.where(value: "part-page")).map(m => m
        .location()
        .page())
      if (
        heads.len() == 0
          and chapter-title.get() != ""
          and not parts.contains(p)
          and not parts.contains(p - 1)
          and not blank-verso(p)
      ) {
        set text(font: "Libertinus Sans", size: 8pt, fill: luma(90))
        if calc.odd(p) { align(right, section-title.get()) } else {
          align(left, chapter-title.get())
        }
      }
    },
    footer: context {
      let p = counter(page).get().first()
      let parts = query(metadata.where(value: "part-page")).map(m => m
        .location()
        .page())
      if (
        not parts.contains(p)
          and not parts.contains(p - 1)
          and not blank-verso(p)
      ) {
        if calc.odd(p) { align(right, counter(page).display("1")) } else {
          align(left, counter(page).display("1"))
        }
      }
    },
  )
  set text(
    font: "Libertinus Serif",
    size: 10.5pt,
    lang: "en",
    hyphenate: true,
    costs: (hyphenation: 60%),
  )
  set par(justify: true, leading: 0.68em, first-line-indent: 1.2em)
  show math.equation: set text(font: "Libertinus Math")
  show heading: set text(font: "Libertinus Sans", hyphenate: false)
  show heading: set par(justify: false, first-line-indent: 0pt)
  show heading.where(level: 1): h => {
    pagebreak(to: "odd", weak: true)
    chapter-title.update(h.body)
    section-title.update("")
    v(0.5in)
    if h.numbering != none {
      context text(
        size: 48pt,
        fill: luma(150),
        counter(heading).display(h.numbering),
      )
      linebreak()
    }
    text(font: "Libertinus Sans", size: 27pt, weight: "bold", h.body)
    v(10pt)
    line(length: 100%, stroke: 0.7pt + luma(140))
    v(18pt)
  }
  show heading.where(level: 2): h => {
    section-title.update(h.body)
    block(above: 1.4em, below: .6em, text(
      font: "Libertinus Sans",
      size: 14pt,
      weight: "bold",
      h.body,
    ))
  }
  set raw(syntaxes: ("../fun.sublime-syntax",))
  show raw: set text(font: "JetBrains Mono", size: 8.6pt, hyphenate: false)
  show raw.where(block: true): code => {
    // Keep excerpts that fit on one page intact; longer listings may split.
    let rows = code
      .text
      .split("\n")
      .map(line => calc.max(1, calc.ceil(line.len() / 78)))
      .sum()
    block(
      fill: luma(246),
      inset: 10pt,
      radius: 3pt,
      width: 100%,
      breakable: rows > 48,
      code,
    )
  }
  set table(
    stroke: (x, y) => if y == 0 { (top: 0.6pt, bottom: 0.3pt) } else { none },
    inset: 6pt,
  )
  body
}
#let callout(kind, body) = block(
  width: 100%,
  inset: (left: 12pt, top: 6pt, bottom: 6pt),
  stroke: (left: 1pt + luma(140)),
)[#text(font: "Libertinus Sans", smallcaps(kind)) #body]
#let part(body) = {
  pagebreak(to: "odd", weak: true)
  chapter-title.update("")
  metadata("part-page")
  v(2in)
  align(center, text(font: "Libertinus Sans", size: 29pt, body))
  pagebreak(to: "odd")
}
#let chapter(n, title, id) = {
  counter(heading).update((n - 1,))
  [#heading(level: 1, numbering: "1", supplement: [Chapter], title)#label(id)]
}

#let appendix(n, title, id) = {
  counter(heading).update((n - 1,))
  [#heading(level: 1, numbering: "A", supplement: [Appendix], title)#label(id)]
}
