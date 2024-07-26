#import "template.lib.typ": *
#import "@local/common:0.0.0": *
#show: problemlist($(3 \/ 2)$, [ Редукционные графы ])

+ Нарисуйте редукционные графы выражений:\
  [a]#hs $H #I H$, где $H == @x%y.. x(\z.. y z y)x$;\
  [b]#hs $L L #I$, где $L == @x%y.. x(y y)x$;\
  [c]#hs $P Q$, где $P == @u.. u #I u$, $Q == @x%y.. x y #I (x y)$.

+ Постройте л-выражения с редукционными графами:
  #v(.7cm)
  #grid(
    columns: (6.5cm, 6.5cm),
    align: (center, center),
    gutter: 1.7cm,
    lambda-diagram(
      spacing: 2cm,
      {
        let (A,B,C) = ((0,0),(1,0),(2,0))
        lnode(A)
        lnode(B)
        lnode(C)
        ledge(A,B, bend: 15deg)
        ledge(B,C, bend: 15deg)
        self(A, angle: -90deg)
        self(B, angle: -90deg)
      }
    ),
    lambda-diagram(
      spacing: 2cm,
      {
        let (A,B,C) = ((0,0),(1,0),(2,0))
        lnode(A)
        lnode(B)
        lnode(C)
        ledge(A,B, bend: 15deg)
        ledge(B,C, bend: 15deg)
        self(B, angle: -90deg)
        self(C, angle: -90deg)
      }
    ),
    grid.cell(
      colspan: 2,
      lambda-diagram(
        spacing: 1cm,
        {
          let (A1,B1,C1) = ((0,1),(1,0),(0,-1))
          let (A2,B2,C2) = ((4,1),(5,0),(4,-1))
          let (A3,B3,C3) = ((8,1),(9,0),(8,-1))
          let (A4,B4,C4) = ((9,1),(10,0),(9,-1))
          lnode(A1)
          lnode(B1)
          lnode(C1)
          lnode(A2)
          lnode(B2)
          lnode(C2)
          lnode(A3)
          lnode(B3)
          lnode(C3)
          ledge(A1, A2)
          ledge(B1, B2)
          ledge(C1, C2)
          ledge(A2, A3)
          ledge(B2, B3)
          ledge(C2, C3)
          ledge(A1, B1)
          ledge(B1, C1)
          ledge(C1, A1)
          ledge(A2, B2)
          ledge(B2, C2)
          ledge(C2, A2)
          ledge(A3, B3)
          ledge(B3, C3)
          ledge(C3, A3)
          ledge(A3, A4)
          ledge(B3, B4)
          ledge(C3, C4)
        }
      )
    )
  )
  #v(.7cm)

+ Покажите, что *ни одно* л-выражение не имеет редукционный граф
  #align(center, lambda-diagram(
    spacing: 1.8cm,
    {
      let (A,B1,B2,B3,C) = ((0,1),(-1,0),(0,0),(1,0),(0,-1))
      lnode(A)
      lnode(B1)
      lnode(B2)
      lnode(B3)
      lnode(C)
      ledge(A,B1)
      ledge(A,B2)
      ledge(A,B3)
      ledge(B1,C)
      ledge(B2,C)
      ledge(B3,C)
    }
  ))

+ Найдите л-выражение $M_0$ с редукционным путём
  $
    M_0 ->>_beta M_1 ->_eta M_2 ->>_beta M_3 ->_eta M_4 ->>_beta dots.h.c
  $

+ Пусть $M_1 == (@x.. b x (b c))c$, $M_2 == (@x.. x x)(b c)$. Докажите, что *не* существует такого выражения $M$, что $M ->> M_1$ и $M ->> M_2$.
