#import "template.lib.typ": *
#import "@local/common:0.0.0": *
#show: formatting
#let Mult = combinator([Mult])
#let Fac = combinator([Fac])
#set heading(numbering: none)

#head([ Задачи ])

= Конверсия и редукция

+
  - Перепишите в формальной нотации: $y(@x..x y (@z%w..y z))$
  - Перепишите в упрощённом виде: $(@v'(@v''((((@v v)v')v'')((v''(@v'''(v'v''')))v''))))$

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
  - Постройте последовательность $M_0, M_1, ...$, такую, что $M_i inc M_j$, если $i eq.not j$.

+ Докажите, что $P inc Q hs <==> hs (#l + (P = Q)) tack.r #K = #KK$

+ Постройте последовательность л-выражений $M_0, M_1, ...$ так, чтобы $M_0 = v$ и для любого\ $n in NN_0$ выполнялось $M_(n+1) &= M_(n+2) M_n.$

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
        C x ->> delta x (C x),\
        D ->> C D.
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
    "SN"_(n+1) &:= { M in #L | forall N_1, N_2, ..., N_k in "SN"_n: hs M N_1 N_2 ... N_k in "SN"_n }.
  $
  Докажите, что\
  [a]#hs $"SN"_1 subset "SN"_0$, но $"SN"_1 eq.not "SN"_0$.\
  [b]#hs $"SN"_1 = "SN"_2 = "SN"_3 = ...$

+ Нарисуйте редукционные графы выражений:\
  [a]#hs $H #I H$, где $H == @x%y.. x(\z.. y z y)x$;\
  [b]#hs $L L #I$, где $L == @x%y.. x(y y)x$;\
  [c]#hs $P Q$, где $P == @u.. u #I u$, $Q == @x%y.. x y #I (x y)$.

+ Постройте л-выражения с редукционными графами:
  #v(.7cm)
  #grid(
    columns: (6.5cm, 6.5cm),
    align: (center, center),
    gutter: 1.9cm,
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

= л-представимость

+ Пусть $M_1, M_2, ..., M_k$ и $N_1, N_2, ..., N_k$ --- два набора л-выражений. Покажите, что
  $
    << M_1, M_2, ..., M_k >> = << N_1, N_2, ..., N_k >> hs <==> hs M_1 = N_1, M_2 = N_2, ..., M_k = N_k
  $

+ Постройте л-выражения $A, B in #L$ таким образом, чтобы $A x = A$ и $B x = x B$.

+ Постройте выражения $F, pi in #L^0$, такие, что:
  - $forall n in NN: hs F nums(n) x y = x y^(~~ n)$
  - $forall n in NN, hs forall i <= n: hs pi nums(n) num(i) = pi^n_i $

+
  - Постройте л-выражение $Mult$, такое, что $Mult num(n) #h(3pt) num(m) = num(m n)$ для любых $m,n in NN_0$.
  - Постройте л-выражение $Fac$, такое, что $Fac num(n) = num(n!)$ для любого $n in NN_0$.

+ _Элементарная функция Аккермана_ $phi$ определяется следующими соотношениями:
  $
    phi(0, n) &= n+1,\
    phi(m+1, 0) &= phi(m, 1),\
    phi(m+1, n+1) &= phi(m, phi(m+1, n)).
  $
  Покажите, что $phi$ рекурсивна, и найдите л-выражение, которое её л-представляет.

+ Постройте функцию предшествующего элемента для чисел Чёрча: $#P _c^-$ такое, что $#P _c^- c_(n+1) = c_n$ при всех $n in NN_0$.

+ Допустим, что каждый символ в упрощённой записи л-выражения (переменная, скобка, точка, запятая, лямбда) занимает 0.5см пространства на бумаге. Найдите л-выражение длиной менее 25см, имеющее нормальную форму длиной не менее $10^10^150$ световых лет (скорость света составляет $3 dot 10^10$ см/сек.)

#let p = math.pound
+ Пусть
  $
    pound &= @a%b%c%d%e%f%g%h%i%j%k%l%m%n%o%p%q%s%t%u%v%w%x%y%z%r.. r(t h i s i s a f i x e d p o i n t c o m b i n a t o r),\
    dollar &= #p#p#p#p#p#p#p#p#p#p#p#p#p#p#p#p#p#p#p#p#p#p#p#p#p#p.
  $
  Покажите, что $dollar$ --- комбинатор неподвижной точки.

+ Докажите, что $M in #L$ --- комбинатор неподвижной точки $<==>$ $M = (#S #I) M$.

+ Пусть $f$, $g$ --- л-выражения. Положим $X == #Th (f circ g)$. Докажите, что $g(X)$ --- неподвижная точка выражения $g circ f$.

+ Положим $#Y _M == @f.. W W M$, где $W == @x%z.. f(x x z)$. Докажите, что $#Y _M$ --- комбинатор неподвижной точки для любого $M in #L$.

+ Докажите, что $#Y _M = #Y _N ==> M = N$. ($#Y _M$ и $#Y _N$ определены как в предыдущей задаче)

+
  - Пусть $f : NN_0^2 -> NN_0$ --- рекурсивная функция. Постройте последовательность $X_0, X_1, ...$ л-выражений, такую, что при всех $n in NN_0$ выполняется $X_n X_m = X_(f(n,m))$.
  - Пусть $X = { x_1, x_2, ..., x_n }$, и пусть $times$ --- бинарная операция на $X$. Постройте л-выражения $X_1, X_2, ..., X_n$ таким образом, чтобы выполнялось $X_i X_j = X_k hs <==> hs x_i times x_j = x_k$ при всех $i, j, k$.

+ Пусть $d$ --- числовая система. Докажите, что $d$ адекватна тогда и только тогда, когда
  $
    exists F, F^(-1) in #L: hs hs forall n in NN_0: hs hs (F nums(n) = d_n) and (F^(-1) d_n = num(n)).
  $

#let C = combinator([C])
+ Пусть $d_0, d_1, ...$ --- адекватная числовая система. Положим $d'_n == #Y#C d_n$, где $#C == @x%y%z.. x(z y)$. Покажите, что все рекурсивные функции одного аргумента $phi : NN_0 -> NN_0$ л-представляются с помощью $d'$.\ (подсказка: рассмотрите $F' == @x..x F$)

+ Пусть $f_0 == @x%y%z.. y$ и $#S^+_f == @x.. <<x>>$. Покажите, что функции $#P^-_f == <<I>>$ и $Zero _f == @x%y%z.. x (@x'%y'%z'.. z')y z$ превращают $(f_0, #S^+_f)$ в адекватную числовую систему.

+ Рассмотрим последовательность $a_n == #K^n #I$. Покажите, что $a$ --- *не* числовая система.

+ Покажите, что множество ${ M in #L | M = #I }$ --- *не* рекурсивное.

+ Докажите, что существует л-выражение $M$, такое, что $M = num(M)$.\ (подсказка: обратите внимание на доказательство теоремы Скотта-Карри о неразрешимости)

+ Докажите _вторую теорему о неподвижной точке:_ $forall F in #L: hs exists X in #L: hs F nums(X) = X$.
