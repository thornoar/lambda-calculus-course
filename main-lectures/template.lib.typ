#import "@preview/quick-maths:0.1.0": shorthands
// #import "@preview/ouset:0.2.0": *

// Shortcut definitions

#let l = math.lambda
#let be = $beta eta$
#let L = math.Lambda
#let impl = $#h(5pt) => #h(5pt)$
#let conv = math.tilde.eq
#let proves = $#l tack.r$
#let TV(expr) = $"TV"(#expr)$
#let FV(expr) = $"FV"(#expr)$
#let BV(expr) = $"BV"(#expr)$
#let Sub(expr) = $"Sub"(#expr)$
#let Con(expr) = $"Con"(#expr)$
#let CR(expr) = $"CR"(#expr)$
#let Gr(expr) = $"Gr"(#expr)$
#let empty = math.diameter
#let acongr = $eq^alpha$
#let inc = math.op("#")
#let rel = math.subset.sq
#let rrel = math.supset.sq

#let combinator = it => math.bold(math.upright(math.sans(it)))
#let I = combinator([I])
#let K = combinator([K])
#let KK = combinator($K_*$)
#let S = combinator([S])
#let Y = combinator([Y])
#let O = combinator(math.Omega)

#let formatting = doc => {
  set page(
    "a4",
    margin: (x: 0.5in, y: 0.5in),
    numbering: none
  )
  set text(12pt, lang: "ru")
  show "л-": name => $lambda"-"$
  show "ал-": name => $alpha"-"$
  show "бэ-": name => $be"-"$
  show "б-": name => $beta"-"$
  show "эт-": name => $eta"-"$

  set heading(numbering: "1.1.")
  show heading.where(level:1): it => {
    counter(math.equation).update(0)
    it
  }
  show: shorthands.with(
    ($>=$, math.gt.eq.slant),
    ($<=$, math.lt.eq.slant),
    ($==>$, $#h(5pt) arrow.r.double #h(5pt)$),
    ($<==$, $#h(5pt) arrow.l.double #h(5pt)$),
    ($,,$, $,#h(5pt)$),
    ($>$, math.lambda),
    ($~~$, math.tilde),
    ($==$, math.equiv),
    ($!==$, math.equiv.not),
    ($,.$, $,#h(.5pt)$),
    ($..$, $.#h(3pt)$)
  )

  show outline.entry.where(level: 1): it => {
    v(1em, weak: true)
    strong(it)
  }

  set outline(indent: auto, fill: repeat([.#h(3pt)]))
  set figure(gap: 1.5em)

  doc
}

#let head(str) = align(center)[
  #text(18pt)[*#str*]\
  #text(14pt)[_$lambda$-исчисление, 2024_]
]

#let problemlist(num, title) = doc => {
  show: formatting
  set page(background: image("pictures/troubles-faded.jpg", width: 100%, height: 100%, fit: "stretch"))
  set enum(numbering: n => [ *Задача #num.#n.* ])

  head([ Лист №#num. #title ])
  doc
}

// Routines for drawing reduction diagrams

#import "@preview/fletcher:0.4.5" as fletcher: diagram, node, edge

#let lnode(A, radius: 0.06cm) = node(A, shape: circle, fill: black, radius: radius)
#let ledge(A,B,bend) = edge(A, B, marks: "stealth-stealth", bend: bend)
#let add((a,b),(c,d)) = (a+c, b+d)
#let mult(r, (a,b)) = (r*a, r*b)
#let self(A, angle: -90deg, bend: -160deg) = edge(A, add(A, mult(0.001,(calc.cos(angle+90deg), calc.sin(angle+90deg)))), marks: "stealth-stealth", bend: bend)

#let lambda-diagram(spacing, block) = {
  diagram(
    axes: (ltr,btt),
    spacing: spacing,
    node-stroke: none,
    edge-stroke: 0.7pt,
    node-outset: 3pt,
    block
  )
}
