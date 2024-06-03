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
