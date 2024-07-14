#import "template.lib.typ": *
#import "@local/common:0.0.0": *
#show: problemlist(2, [ л-Представимость и неразрешимость ])
#let Mult = combinator([Mult])
#let Fac = combinator([Fac])

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