#import "template/book.typ": book, callout, chapter, part
#show: book
#page(header: none, footer: none)[
  #set align(center)
  #v(1.2in)
  #image("img/haskell-logo.svg", width: 1.9in)
  #v(0.5in)
  #text(
    font: "Libertinus Sans",
    size: 36pt,
    weight: "bold",
  )[Write You a Haskell]
  #v(0.5in)
  #text(size: 13pt, style: "italic")[
    Building a modern functional compiler from first principles.
  ]
  #v(0.9in)
  #text(size: 16pt)[Stephen Diehl]
]
#page(header: none, footer: none)[
  #v(2in)
  #text(size: 24pt)[Write You a Haskell]
]
#page(header: none, footer: none)[
  #v(1fr)
  Copyright © 2014-2026 Stephen Diehl.

  Prose: Creative Commons Attribution-ShareAlike 4.0 International.
  Code: MIT License. Fonts: SIL Open Font License.
]
#outline(depth: 2, title: [Contents])
#include "../build/pdf/body.typ"
#pagebreak(to: "odd")
#bibliography("../book/refs.bib", style: "chicago-author-date", full: true)
