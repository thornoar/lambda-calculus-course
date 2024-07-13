#import "template.lib.typ": *
#import "@local/common:0.0.0": *
#show: problemlist(1, [ Конверсия и редукция ])

+
  - Перепишите в формальной нотации: $y(@x..x y (@z%w..y z))$
  - Перепишите в упрощённом виде: $@v'(@v''((((@v v)v')v'')((v''(@v'''(v'v''')))v'')))$

+ Положим $X == #S#I$. Покажите, что $X X X X = X(X(X X))$. Правда ли, что $X^n X = X X^(~~ n)$ справедливо для всех $n in NN_0$?

+ Покажите, что выражение имеет нормальную форму:\
  [a]#hs $(@y.. y y y)((@a%b.. a)#I (#S#S))$, #h(1fr)
  [b]#hs $#S#S#S#S$, #h(1fr)
  [c]\*#hs $#S (#S#S)(#S#S)#S$. #h(1fr)

+ Найдите л-выражение $M$, такое, что $forall N in #L: hs M N = M M$.

+ Докажите, что *не* существует такого $F in #L$, что $forall M, N in #L: hs F (M N) = M$.

+ Пусть $A == #S#K#K#K$. Постройте такое л-выражение $M$, чтобы выполнялась конверсия $#S#I M #K A = #S M #S #K A$.

+ Докажите, что правило эт-конверсии ($@x.. M x = M, hs forall M,x: x in.not TV(M)$) эквивалентно тому, что "функции равны, если равны их значения":
  #v(-5pt)
  $
    M x = N x ==> M = N, #h(15pt) forall M, N, x : x in.not TV(M N).
  $
  #v(-5pt)

+ 
  - Докажите, что: #h(1fr)
    [a]#hs $#I inc #K$, #h(1fr)
    [b]#hs $#I inc #S$, #h(1fr)
    [c]\*#hs $x y inc x x$. #h(5fr)
  - Постройте последовательность $M_0, M_1, ...$, такую, что $M_i inc N_j$, если $i eq.not j$.

+ Докажите, что $P inc Q hs <==> hs (#l + (P = Q)) tack.r #K = #KK$

+ Постройте последовательность л-выражений $M_0, M_2, ...$ так, чтобы $M_0 = v$ и для любого\ $n in NN_0$ выполнялось $M_(n+1) &= M_(n+2) M_n.$

+ Докажите, что $forall M in #L: hs exists N in #L: hs N #I ->>_beta M$, причём $N$ в б-нормальной форме.

+ Обозначим через $M arrow.t N$ условие $exists L: hs (L ->> M) and (L ->> N)$. Покажите, что:\
  [a]#hs $(@x.. a x)b arrow.t (@y.. y b)a$, #h(1fr)
  [b]#hs $(@x.. x c)c arrow.t (@x.. x x)c$, #h(1fr)
  [c]#hs $(@x.. b x)c arrow.t (@x..x)b c$ #h(1fr)

+ Постройте л-выражения со следующими редукционными графами:\
  #v(.3cm)
  #grid(
    columns: (4.5cm, 3.5cm, 5.5cm),
    align: (center, center, center),
    gutter: 1.6cm,
    lambda-diagram(
      spacing: 0.7cm,
      {
        let (A,B,C) = ((-1.5,0),(1.5,0),(0,-3))
        lnode(A)
        lnode(B)
        lnode(C)
        ledge(A,B, bend: 30deg)
        ledge(B,A, bend: 30deg)
        ledge(B,C, bend: 10deg)
        ledge(A,C, bend: -10deg)
      }
    ),
    grid.cell(
      rowspan: 2,
      lambda-diagram(
        spacing: 1.8cm,
        {
          import calc: *
          let (A,B,C) = (
            (cos(180deg), sin(180deg)),
            (cos(60deg), sin(60deg)),
            (cos(-60deg), sin(-60deg))
          )
          lnode(A); self(A, angle: 180deg)
          lnode(B); self(B, angle: 60deg)
          lnode(C); self(C, angle: -60deg)
          ledge(A,B)
          ledge(B,C)
          ledge(C,A)
        }
      )
    ),
    grid.cell(
      rowspan: 2,
      lambda-diagram(
        spacing: 1.3cm,
        {
          let (A,B,C,D) = ((0,0),(0,-1),(0,-2),(0,-3))
          let L = (1,0)
          lnode(A)
          lnode(B)
          lnode(C)
          lnode(L)
          node(D, move($dots.v$, dy: 0.15cm), radius: 0.06cm, inset: 0cm, outset: 0cm)
          ledge(A,B, bend: 20deg)
          ledge(B,C, bend: 20deg)
          ledge(C,D, bend: 20deg)
          ledge(A,L, bend: 10deg)
          ledge(B,L, bend: -10deg)
          ledge(C,L, bend: -40deg)
          self(A, angle: 180deg)
          self(B, angle: 180deg)
          self(C, angle: 180deg)
          self(L, angle: 40deg)
        }
      )
    ),
    lambda-diagram(
      spacing: 1.1cm,
      {
        let (A,B,C) = ((-2,0),(0,0),(2,0))
        lnode(A)
        lnode(B)
        ledge(A,B, bend: 40deg)
        ledge(B,A, bend: 40deg)
        self(A, angle: 180deg)
        self(B, angle: 0deg)
      }
    ),
  )
  #v(.5cm)

+ Нарисуйте редукционные графы следующих л-выражений:\
  [a]#hs $(@x.. #I x x)(@x.. #I x x)$, #h(1fr)
  [b]#hs $(@x.. #I (x x))(@x.. #I (x x))$ #h(1fr)

+ Пусть $M == A A x$, где $A == @a%x%z.. z (a a x)$. Докажите, что редукционный граф $Gr(M)$ содержит $n$-мерный куб при всех $n in NN_0$. 

+ Покажите, что концептуально существует только одно л-выражение (а именно $#O$), имеющее следующий редукционный граф:
  #align(center, lambda-diagram(
    spacing: 1cm,
    {
      lnode((0,0))
      self((0,0), angle: 0deg)
    }
  ))

+ Расширим множество л-выражений двумя константами $delta, epsilon$. Также добавим новое правило редукции: $delta M M -> epsilon$ для любого $M in #L union {delta, epsilon}$. Докажите, что в получившейся системе *не* выполняется теорема Чёрча-Россера.
  #box(
    stroke: 0.5pt,
    width: 100%,
    inset: 0.25cm,
    radius: 0.2cm,
    [
      Подсказка: найдите выражения $C$. $D$ такие, что
      $
        C x -> delta x (C x),\
        D -> C D.
      $
      Докажите, что $D ->> epsilon$ и $D ->> C epsilon$, но у $epsilon$ и $C epsilon$ нет общего редукта.
    ]
  )

+ Пусть $rel1$ и $rel2$ --- коммутирующие отношения на множестве $X$. Покажите, что $Trans(rel1)$ и $Trans(rel2)$ также коммутируют.

+ л-выражение $M$ _сильно нормализуется_ #h(2pt) (нотация $SN(M)$), если *не* существует бесконечного редукционного пути, начинающегося в $M$. Докажите, что:\
  [a]#hs $SN(M) ==> M$ имеет нормальную форму;\
  [b]#hs $SN(M) ==> Gr(M)$ конечен. Верно ли обратное?

+ Рассмотрим
  $
    "SN"_0 &:= { M in #L | SN(M) },\
    "SN"_1 &:= { M in #L | forall N_1, N_2, ..., N_k in "SN"_0: hs M N_1 N_2 ... N_k in "SN"_0 }.
  $
  Докажите, что $"SN"_1 subset.not "SN"_0$.
