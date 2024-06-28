#let formatting = doc => {
  set page(
    "a4",
    margin: (x: 0.5in, y: 0.5in),
    numbering: none,
    background: image("pictures/troubles-faded.jpg", width: 100%, height: 100%, fit: "stretch")
  )
  set text(12pt, lang: "ru")
  show "л-": name => $lambda"-"$

  doc
}

#let head(str) = align(center)[
  = #str
  #text(14pt)[_$lambda$-исчисление, 2024_]
]

#let problemlist(num, title) = doc => {
  show: formatting
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
