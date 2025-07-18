#import "@preview/quick-maths:0.2.1": shorthands
#import "@preview/ouset:0.2.0": *

// Shortcut definitions

#let l = math.lambda
#let be = $beta eta$
#let L = math.Lambda
#let impl = $#h(5pt) => #h(5pt)$
#let proves = $#l tack.r$
#let TV(expr) = $"TV"(#expr)$
#let FV(expr) = $"FV"(#expr)$
#let BV(expr) = $"BV"(#expr)$
#let Sub(expr) = $"Sub"(#expr)$
#let Con(expr) = $"Con"(#expr)$
#let CR(expr) = $"CR"(#expr)$
#let SN(expr) = $"SN"(#expr)$
#let Gr(expr) = $"Gr"(#expr)$
#let Trans(expr) = $"Trans"lr((#expr), size: #0.5cm)$
#let Equiv(expr) = $"Equiv"lr((#expr), size: #0.5cm)$
#let Refl(expr) = $"Refl"lr((#expr), size: #0.5cm)$
#let Preord(expr) = $"Preord"lr((#expr), size: #0.5cm)$
#let empty = math.diameter
#let acongr = $eq^alpha$
#let inc = math.op("#")
#let rrel = math.subset.sq
#let rrel1 = math.attach(rrel, br: [1])
#let rrel2 = math.attach(rrel, br: [2])
#let rel = math.supset.sq
#let rel1 = math.attach(rel, br: [1])
#let rel2 = math.attach(rel, br: [2])
#let rel3 = math.attach(rel, br: [3])
#let rel4 = math.attach(rel, br: [4])
#let ul = symbol("\u{231c}")
#let ur = symbol("\u{231d}")
#let num(expr) = $ul #expr ur $
#let nums(expr) = $#h(3pt) ul #expr ur #h(3pt)$
#let ite(b, m, n) = $underline("if") #h(5pt) #b #h(5pt) underline("then") #h(5pt) #m #h(5pt) underline("else") #h(5pt) #n$
#let A = $cal(A)$
#let B = $cal(B)$
#let arr = math.arrow.r.squiggly

#let combinator = it => math.bold(math.upright(math.sans(it)))
#let Ap = combinator($A_+$)
#let Am = combinator($A_*$)
#let Ae = combinator($A_("exp")$)
#let I = combinator([I])
#let K = combinator([K])
#let T = combinator([T])
#let KK = combinator($K_*$)
#let F = combinator([F])
#let S = combinator([S])
#let Y = combinator([Y])
#let Th = combinator(math.Theta)
#let O = combinator(math.Omega)
#let P = combinator([P])
#let Sp = combinator($S^+$)
#let Pm = combinator($P^-$)
#let Zero = combinator([Zero])

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
    ($<<$, math.angle.l),
    ($>>$, math.angle.r),
    ($==>$, $#h(5pt) arrow.r.double #h(5pt)$),
    ($<==$, $#h(5pt) arrow.l.double #h(5pt)$),
    ($,,$, $,#h(5pt)$),
    ($@$, math.lambda),
    ($~~$, math.tilde),
    ($==$, math.equiv),
    ($!==$, math.equiv.not),
    ($%$, $,#h(.5pt)$),
    ($..$, $.#h(3pt)$)
  )

  show outline.entry.where(level: 1): it => {
    v(1em, weak: true)
    strong(it)
  }

  set outline(indent: auto)
  set figure(gap: 1.5em)

  // show math.equation: box

  doc
}

#let head(str) = align(center)[
  #text(18pt)[*#str*]\
  #text(14pt)[_$lambda$-исчисление, 2024_]
]

#let problemlist(num, title) = doc => {
  show: formatting
  set page(background: image("pictures/troubles-faded.jpg", width: 100%, height: 100%, fit: "stretch"))
  set enum(numbering: n => [ #num.#n. ])

  head([ Лист №#num. #title ])
  doc
}

// Routines for drawing reduction diagrams

#import "@preview/fletcher:0.5.7" as fletcher: diagram, node, edge

#let lnode(A, radius: 0.06cm) = node(A, shape: circle, fill: black, radius: radius, outset: 2*radius)
#let ledge(A, B, bend: 0deg, marks:"-stealth") = edge(A, B, marks: marks, bend: bend)
#let add((a,b),(c,d)) = (a+c, b+d)
#let mult(r, (a,b)) = (r*a, r*b)
#let self(A, angle: -90deg, bend: -160deg, marks: "stealth-") = edge(A, add(A, mult(0.001,(calc.cos(angle+90deg), calc.sin(angle+90deg)))), marks: marks, bend: bend)

#let lambda-diagram(spacing: 1cm, block) = {
  diagram(
    axes: (ltr,btt),
    spacing: spacing,
    node-stroke: none,
    edge-stroke: 0.7pt,
    node-outset: 0pt,
    block
  )
}

#let diamond-diagram(prepad: -15pt, spacing: 1cm, r1, r2, r3, r4) = {
  v(prepad)
  align(center, lambda-diagram(
    spacing: spacing,
    {
      edge((0,0), (2,0), "-", label: r1, label-side: left)
      edge((0,0), (0,-1), "-", label: r2, label-side: right)
      edge((0,-1), (2,-1), "--", label: r3, label-side: right)
      edge((2,0), (2,-1), "--", label: r4, label-side: left)
    }
  ))
}
