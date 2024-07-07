#import "@preview/quick-maths:0.1.0": shorthands

// shortcut definitions
#let l = math.lambda
#let L = math.Lambda
#let impl = $#h(5pt) => #h(5pt)$

#let formatting(bg: true) = doc => {
  let setbg = none
  if (bg) {
    setbg = image("pictures/troubles-faded.jpg", width: 100%, height: 100%, fit: "stretch")
  }
  set page(
    "a4",
    margin: (x: 0.5in, y: 0.5in),
    numbering: none,
    background: setbg
  )
  set text(12pt, lang: "ru")
  show "л-": name => $lambda"-"$

  set heading(numbering: "1.1.")
  show heading.where(level:1): it => {
    counter(math.equation).update(0)
    it
  }
  // set math.equation(numbering: n => {
  //   let h1 = counter(heading).get().first()
  //   numbering("(1.1.1)", h1, n)
  // }, supplement: "Equation")
  show: shorthands.with(
    // ($>=$, math.gt.eq.slant),
    ($==>$, $#h(5pt) arrow.r.double #h(5pt)$),
    ($..$, $.#h(3pt)$),
    ($\\$, $lambda$)
    // ($<=$, math.lt.eq.slant)
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
  show: formatting(bg: true)
  set enum(numbering: n => [ *Задача #num.#n.* ])

  head([ Лист №#num. #title ])
  doc
}

// Routines for drawing reduction diagrams

#import "@preview/fletcher:0.4.5" as fletcher: diagram, node, edge

#let lnode(A) = node(A, shape: circle, fill: black, radius: 0.1cm)
// #let ledge(A,B,bnd) = edge(A, B, marks: (none, "straight"), bend: bnd)
// #let sedge(A) = edge(A, A, marks: ("straight", none), bend: -160deg)
#let ledge(A,B,bnd) = edge(A, B, marks: "->", bend: bnd)
#let sedge(A) = edge(A, A, marks: "->", bend: -160deg)

#let lambda-diagram(space,block) = {
  diagram(
    axes: (ltr,btt),
    spacing: space,
    node-stroke: none,
    edge-stroke: 0.7pt,
    node-outset: 5pt,
    block
  )
}

// #lambda-diagram(3cm, {
//   let (A,B,C) = ((0,0),(1,0),(2,0))
//   let bnd = 40deg
//   for x in (A,B,C) { lnode(x) }
//   sedge(A)
//   ledge(A, B, bnd)
//   sedge(B)
//   ledge(B, C, bnd)
// })
//
// #lambda-diagram(2cm, {
//   let arr = ((-1,0),(0,1),(1,0),(0,-1))
//   let bnd = -10deg
//   let epsilon = 0.00001
//   let mult(num, (a,b)) = (a*num, b*num)
//   let sum((a,b),(c,d)) = (a+c, b+d)
//   for i in (0,1,2,3) {
//     let x = arr.at(i)
//     lnode(x)
//     ledge(arr.at(i), arr.at(calc.rem(i+1,4)), bnd)
//     ledge(x, sum(x, mult(epsilon, arr.at(calc.rem(i+1,4)))), 150deg)
//   }
// })
