#import "@local/common:0.0.0": *
#import "@local/theorem:0.0.0": *
#show: theorem
#import "./template.lib.typ": *
#show: formatting
// #set page("a5", flipped: false, margin: (x: 0.5in, y: 0.5in)) 
#set heading(numbering: none)
#set text(size: 14pt)

#head([ Результаты итоговой работы ])

#show: body => align(center, body)

#table(
  columns: (6cm, 2cm),
  inset: 5pt,
  align: (center, center),
  stroke: (x,y) => {
    if ((y == 0 and x != 0 and x != 2) or (x == 0 and y == 0)) { none }
    else { 1pt }
  },
  table.header([ _Фамилия_ ], [ _Оценка_ ]),
  [ Афанасьев И. ],    [ 2 ],
  [ Винников С. ],     [ 4 ],
  [ Клименко М. ],     [ 5 ],
  [ Коско А. ],        [ 4 ],
  [ Мело Лапшин Б. ],  [ 4 ],
  [ Немчинова А. ],    [ 3 ],
  [ Поздняков Г. ],    [ 3 ],
  [ Рыбаков И. ],      [ 2 ],
  [ Тараканов Г. ],    [ 3 ],
  [ Титов О. ],        [ 2 ],
  [ Харченко М. ],     [ 2 ],
  [ Чурилин Ф. ],      [ 3 ],
  [ Шутова Д. ],       [ 4 ],
  [ Григорьев Ф. ],    [ 4 ]
)
