#import "@local/common:0.0.0": *
#import "@local/theorem:0.0.0": *
#show: theorem
#import "./template.lib.typ": *
#show: formatting

#import "@preview/cetz:0.2.2" as cz

// #set list(indent: 0.1in)
// #set enum(numbering: n => strong([#n.]))
#set enum(numbering: "(1.a)")
#set page(numbering: "1")

#let myplainstyle(ident, head, ..args) = thmstyle(ident, head, titlefmt: it => strong(underline(it)), ..args)
#let mystatestyle(ident, head, ..args) = thmstyle(ident, head, bodyfmt: emph, titlefmt: it => strong(underline(it)), ..args)

#let def = myplainstyle("definition", "Определение")
#let agr = myplainstyle("agreement", "Договорённость", numbering: none)
#let note = myplainstyle("note", "Замечание")
#let nota = myplainstyle("notation", "Нотация", numbering: none)
#let exam = myplainstyle("example", "Пример")
#let exer = myplainstyle("exercise", "Упражнение", numbering: none)
#let th = mystatestyle("theorem", "Теорема")
#let lm = mystatestyle("lemma", "Лемма")
#let state = mystatestyle("statement", "Утверждение")
#let cor = mystatestyle("corollary", "Следствие", base: "theorem")
#let prb = mystatestyle("problem", "Задача")
#let pf = proofstyle("proof", "Доказательство", titlefmt: it => underline(emph(it)))

#head([ МАТЕРИАЛ КУРСА ])

#outline()
#pagebreak()

= Конверсия и редукция

== Основные понятия

#def[
  Рассмотрим счётное множество $V = {v, v', v'', ...}$. Элементы этого множества будут называться _переменными._ Множество л-выражений, #L, --- это наименьшее множество, удовлетворяющее следующим условиям: #v(2pt)
  - $x in V ==> x in #L$;
  - $x in V,, M in #L==> (::x M) in #L$; #h(1fr) (абстракция, морально: определение функции)
  - $M in #L,, N in #L ==> (M N) in #L$. #h(1fr) (комбинация, морально: применение функции к аргументу)
]

#exam[
  л-выражения в формальной нотации:
  $
    v';\
    (v v');\
    (::v (v' v));\
    ((::v (v' v))v'');\
    (((::v (::v' (v' v))) v'')v''');
  $
]

#nota[
  - $x$, $y$, $z$, ... обозначают произвольные переменные из множества $V$.
  - $M$, $N$, $K$, ... обозначают произвольные л-выражения из #L.
  - Внешние скобки опускаются: $(::x(y z)) -> ::x (y z)$.
  - Многократная абстракция сокращается:
  $
    ::x_1 (::x_2 (::... (::x_n M)...)) -> ::x_1,.x_2,. ... ,.x_n.. M -> ::arrow(x).. M
  $
  - Многократная комбинация сокращается:
  $
    ((...((M_1 M_2)M_3)...)M_n)N -> M_1M_2...M_n N -> arrow(M)N
  $
  // - $norm(M)$ обозначает количество символов в выражении $M$.
  - Комбинация берёт приоритет над абстракцией: $::x.. y z = #l x.. (y z)$
]

#set table(
  inset: (y: 7pt),
  fill: (_, y) => {
    if (y == 0) { gray.lighten(80%) }
  },
  stroke: (_, y) => {
    if (y == 0) { (bottom: 1pt, x: 1pt) } else { (y:none, x: 1pt) }
  }
)

#def[
  Пусть $M$ --- л-выражение. Множества $TV(M),, FV(M),, BV(M) subset V$ определяются индуктивно:
  #{
    show: it => align(center, it)
    table(
      columns: (10%, 20%, 20%, 20%),
      align: left,
      table.header($M$,$TV(M)$,$FV(M)$,$BV(M)$),
      $x in V$,       ${x}$,                  ${x}$,                  $empty$,
      $::x.. N$,       ${x} union TV(N)$,      $FV(N) \\ {x}$,         ${x} union BV(N)$,
      $N K$,          $TV(N) union TV(K)$,    $FV(N) union FV(K)$,    $BV(N) union BV(K)$
    )
  }
]

#note[
  В данный момент существуют не вполне осмысленные л-выражения. Так, в выражении $(::x.. x y)x$ переменная $x$ выступает одновременно связанной и свободной, а в выражении $::x.. ::x.. x x$ переменная $x$ связывается дважды. Обе этих проблемы можно исправить _заменой связанных переменных:_ $(::x.. x y)x -> (::u.. u y)x$, $::x.. ::x.. x x -> ::x.. ::u.. u u$. Сейчас мы формализуем эту идею.
]

#def[
  Пусть $rrel$ --- бинарное отношение на множестве #L. Тогда $rrel$ называется _совместимым с операциями,_ если:
  $
    M &rrel N ==> ::x.. M rrel ::x.. N,\
    M &rrel N ==> Z M rrel Z N,\
    M &rrel N ==> M Z rrel N Z.
  $
]

#def[
  _Тождественное равенство_ ($equiv$) обозначает полностью идентичный состав символов: $::x.. x y equiv.not ::u.. u y$.
]

#def[
  Отношение _ал-конгруэнтности_ ($acongr$) на #L --- это наименьшее подмножество $#L times #L$, удовлетворяющее следующим условиям:
  - $M acongr M$;
  - $::x.. M acongr ::y.. (M[x -> y])$, при условии что $y in.not TV(M)$;
  - $acongr$ совместимо с операциями.
]

#def[
  Пусть $M$ --- л-выражение. $M$ называется _корректным_ в следующих случаях:
  + $M equiv x in V$;
  + $M equiv ::x.. N$, причём $N$ корректно, а также $x in.not BV(N)$;
  + $M equiv N K$, причём $N,K$ корректны, а также $BV(N) sect FV(K) = empty$ и $FV(N) sect BV(K) = empty$.
]

#exer[
  Доказать, что если $M$ корректно, то $FV(M) sect BV(M) = empty$, $FV(M) union BV(M) = TV(M)$.
]

#exer[
  Пусть $M$ --- л-выражение. Доказать, что существует корректное л-выражение $N$, такое, что $M acongr N$.
]

#agr("Правило переменных")[
  Пусть л-выражения $M_1, M_2, ..., M_n$ выступают с едином контексте. Тогда мы будем предполагать, что выражение $M_1M_2...M_n$ --- корректное.
]

#def[
  л-выражение $M$ называется _замкнутым_ (или _комбинатором_), если $FV(M) = empty$. $#L^0$ обозначает множество всех замкнутых л-выражений.
]

#def[
  $M$ является _подвыражением_ $N$ ($M subset N$), если $M$ лежит во множестве $Sub(N)$:
  #align(center, table(
    columns: (10%, 35%),
    align: left,
    table.header($N$, $Sub(N)$),
    $x in V$, ${x}$,
    $::x.. K$, ${::x.. K} union Sub(K)$,
    $K_1K_2$, $Sub(K_1) union Sub(K_2) union {K_1K_2}$
  ))
]

#def[
  Пусть $F, M in #L$. Тогда
  - $F^0 M equiv M$; #hs $F^(n+1)M equiv F(F^n M)$
  - $F M^(~~ 0) equiv F$; #hs $F M^(~~ n+1) equiv (F M^(~~ n))M$
]

== Оператор подстановки и бэ-конверсия

#def[
  Пусть $M in #L$, $x in.not BV(M)$. Пусть также $N in #L$. _Результат подстановки $N$ вместо $x$_, $M[x := N]$, определяется индуктивно:
  $
    x[x := N] &equiv N;\
    y[x := N] &equiv y,, "если" y equiv.not x;\
    (::y.. M')[x := N] &equiv ::y.. (M'[x := N]);\
    (M_1M_2)[x := N] &equiv (M_1[x := N])(M_2[x := N]).
  $
]

#note[
  Рассмотрим $M equiv ::y.. x,, N equiv y y$. Тогда по предыдущему определению мы получаем $M[x := N] = ::y.. y y$, что настораживает, ведь $M equiv ::y.. x acongr ::u.. x equiv M'$, тогда как $ M[x := N] = ::y.. y y eq.not^alpha ::u.. y y = M'[x := N]. $ Однако заметим, что такая ситуация некорректна, ведь $BV(M) sect FV(N) = {y} eq.not empty$.
]

#exer[
  Доказать, что оператор подстановки уважает ал-конгруэнтность, если рассматриваемые выражения соблюдают правило переменных. Иначе говоря,
  $
    cases(
      reverse: #true,
      // delim: "[" // ]
      M acongr M',
      N acongr N'
    ) ==> M[x := N] acongr M'[x := N'].
  $
]

#lm("о подстановке")[
  Пусть $M, N, L in #L$. Тогда если $x equiv.not y$ и $x in.not FV(L)$, то
  $
    (M[x := N])[y := L] equiv (M[y := L])[x := N[y := L]]
  $
]
#pf[
  Индукция по структуре л-выражения $M$.
  + База: $M equiv u in V$. Тогда рассмотрим три случая:
    - $u equiv x$. Тогда обе части тождественно равны $N[y := L]$, так как $x equiv.not y$.
    - $u equiv y$. Тогда обе части равны $L$, так как $L[x := ...] = L$, ведь $x in.not FV(L)$.
    - $u equiv.not x,y$. Тогда обе части равны $u$.
  + Переход.
    - $M equiv ::z.. M'$. По правилу переменых и определению оператора подстановки мы имеем $z in.not FV(N L)$ и $z equiv.not x,y$. Тогда по предположению индукции
    $
      (::z.. M')[x := N][y := L] &== ::z.. M'[x := N][y := L]\
      &== ::z.. M'[y := L][x := N[y := L]]\
      &== (::z.. M')[y := L][x := N[y := L]].
    $
    - $M == M_1M_2$. Доказательство аналогично.
  q.e.d.
]

#def("бэ-конверсия")[
  Отношение бэ-конверсии ($=$) --- это наименьшее подмножество $#L times #L$, удовлетворяющее следующим условиям:
  - $(::x.. M)N = M[x := N]$; #h(1fr) ($beta$-конверсия)
  - $::x.. M x = M$, при условии что $x in.not TV(M)$; #h(1fr) ($eta$-конверсия)
  - $=$ --- отношение эквивалентности;
  - $=$ совместимо с операциями.
  Если $M = M$, мы говорим, что "$M$ равно $N$", или "$M$ конвертируется в $N$". Запись "$proves M = N$" означает, что конверсию $M = N$ можно вывести из вышеуказанных правил.
]

#th("о неподвижной точке")[
  $forall F in #L: hs exists X in #L: hs F X = X$.
]
#pf[
  Пусть $W == ::x.. F(x x)$ и $X == W W$. Тогда имеем $ X == W W == (::x.. F(x x))W = F(x x)[x := W] == F(W W) == F X, $ q.e.d.
]

#state("fallacy")[
  $forall M,N in #L: hs proves M = N$
]
#pf[
  Рассмотрим $F == ::x,.y.. y x$. Тогда для любых $M,N$ имеем
  $
    F M N == ((::x.. (::y.. y x))M)N = (::y.. y M)N = N M.
  $
  В частности, $F y x = x y$. Однако
  $
    F y x == ((::x.. (::y.. y x))y)x = (::y.. y y)x = x x.
  $
  Тогда $x y = x x$, а значит $F_1 == ::x,.y.. x y = ::x,.y.. x x == F_2$. Теперь для любого $M in #L$ имеем
  $
    M = (::x.. x)M = F_1 (::x.. x) M = F_2 (::x.. x) M = (::x.. x)(::x.. x) = (::x.. x),
  $
  и по транзитивности $M = (::x.. x) = N$ для любых $M, N in #L$. В чём ошибка?
]

#lm[
  Оператор подстановки уважает конверсию. Иначе говоря, если $M = M',, N = N'$, то $M[x := N] = M'[x := N']$.
]
#pf[
  Упражнение.
]

== Комбинаторы и согласованность

#def[
  - $#I == ::x.. x$
  - $#K == ::x,.y.. x$
  - $#KK == ::x,.y.. y$
  - $#S == ::x,.y,.z.. x z(y z)$
  - $#Y == ::f.. (::x.. f(x x))(::x.. f(x x))$ --- комбинатор неподвижной точки: $forall F in #L: hs F(#Y F) = #Y F$.\
    Этот комбинатор позволяет моделировать простую рекурсию. РАссмотрим л-выражение $M$, определённое рекуррентной формулой:
    $
      M x == F x M.
    $
    Определим $G == ::y.. ::x.. F x y$. Тогда $M$ приобретает явную форму: $M == #Y G$ (упражнение).
]

#def[
  - Выражение вида $M = N$ называется _равенством;_
  - Равенство $M = N$ называется _замкнутым,_ если $M,N in #L^0$;
  - Пусть $cal(T)$ --- формальная теория, т.е. набор правил, с помощью которых можно выводить равенства (наподобие л-теории). Тогда $cal(T)$ называется _согласованной_ (нотация $Con(cal(T))$) если $cal(T)$ *не* доказывает все замкнутые равенства. В противном случае $cal(T)$ называется _противоречивой._
  - Если $cal(T)$ --- это набор равенств, то $#l + cal(T)$ обозначает теорию, полученную добавлением равенств из $cal(T)$ к стандартному списку аксиом бэ-конверсии.
]

#def[
  Пусть $M,N in #L$. Тогда $M$ и $N$ называются _несовместимыми_ (нотация $M inc N$), если теория $#l + (M = N)$ противоречива.
]

#exam[
  $#I inc #K$
]
#pf[
  Имеем $#I M N = #K M N$ для любых $M,N in #L$. По определению комбинаторов $#I$ и $#K$, имеем $M N = M$. Подставляя $M == #I$, получаем $N = #I hs forall N in #L$.
]

== Нормальные формы

#def[
  - л-выражение $M$ называется _бэ-нормальной формой,_ если оно *не* имеет подвыражений вида $(::x.. M)N$ или $::y.. (M y)$ (где $y in.not TV(M)$).
  - $M$ _имеет нормальную форму $N$,_ если $M = N$ и $N$ --- нормальная форма.
]

#exam[
  - $#I$ находится в нормальной форме;
  - $#K#I$ имеет нормальную форму $::y.. #I$;
  - Комбинатор $#O = (::x.. x x)(::x.. x x)$ не имеет нормальной формы (доказательство позже).
]

#underline(strong([ Воспоминания о будущем. ]))
- Если $M$ и $N$ --- различные бэ-нф, то $M inc N$
- $M$ может иметь максимум одну нормальную форму;
- $#O = (::x.. x x)(::x.. x x)$ не имеет нормальной формы;
- $#l$ --- согласованная теория.
// - Если $M$ и $N$ --- различные бэ-нф, то $#l tack.r.not M = N$;
// - Следствие: пусть $M$ и $N$ имеют нормальную форму. Тогда либо $M = N$, либо $M inc N$;

== Редукция

#note[
  В правилах конверсии есть определённая асимметрия. Так, о конверсии $ (::x.. x^2 + 1)3 = 10 $ можно сказать, что "10 является результатом _упрощения_ выражения $(::x.. x^2 + 1)3$", но никак не в обратную сторону. Сейчас мы формализуем эту асимметрию. 
]

#def[
  + Отношение $->$ (_редукция за один шаг_) --- это наименьшее подмножество $#L times #L$, такое что:
    - $(::x.. M)N -> M[x := N]$;
    - $::x.. M x -> M$, если $x in.not TV(M)$;
    - $->$ совместимо с операциями.
  + Отношение $->>$ (_редукция_) --- это замыкание $->$ до предпорядка: $->> #h(3pt) = Preord(->)$;
  + Отношение $=$ (_конгруэнтность_ или _эквивалентность_) --- это замыкание $->>$ до отношения эквивалентности: $= #h(3pt)= Equiv(->>)$
]

#def[
  - л-выражения вида $(::x.. M)N$ называются _$beta$-редексами;_ соотв. отношения: $->_beta$, $->>_beta$, $=_beta$
  - л-выражения вида $::x.. M x$ называются _$eta$-редексами._ соотв. отношения: $->_eta$, $->>_eta$, $=_eta$
  - $M$ --- _нормальная форма_ (или _в нормальной форме_), если $M$ не содержит редексов.
  - Пусть $Delta$ --- редекс в выражении $M$. Запись $M ->^Delta N$ означает, что $N$ получается из $M$ сокращением редекса $Delta$: #hs $N == M[Delta -> Delta']$
  - _Редукционный путь_ --- это последовательность (конечная или бесконечная) вида
    $
      M_0 ->^(Delta_0) M_1 ->^(Delta_1) M_2 -> ...
    $
]

#exam[
  - Определим $omega_3 == ::x.. x x x$. Это выражение порождает бесконечный редукционный путь:
    $
      omega_3 omega_3 ->^(omega_3 omega_3) omega_3 omega_3 omega_3 ->^(omega_3 omega_3) omega_3 omega_3 omega_3 omega_3 ->^(omega_3 omega_3) ...
    $
  - Редекс не всегда однозначно задаётся редукцией:
    $
      #I (#I x) ->^(#I x) #I x, hs hs #I (#I x) ->^(#I (#I x)) #I x
    $
]

#state[
  Пусть $M$ --- нормальная форма. Тогда:
  + $exists.not N: hs M -> N$;
  + $M ->> N ==> M == N$.
] <uniquenf>
#pf[
  + Очевидно.
  + По определению $->>$, условие $M ->> N$ влечёт два случая:
    - $M -> K_1 -> K_2 -> ... -> N$ --- невозможно по (1);
    - $M == N$ --- искомый.
  q.e.d.
]

#def[
  _Редукционный граф_ выражения $M$ (нотация $Gr(M)$) --- это граф, в котором:
  $
    V = { N in #L | M ->> N }, hs hs E = { (N,K) in V^2 | N -> K }
  $
]

#def[
  Пусть $rel$ --- произвольное отношение на множестве $X$. $rel$ _обладает свойством Чёрча-Россера_ (нотация $CR(rel)$), если
  $
    forall x, x_1, x_2 in X: hs (x rel x_1) and (x rel x_2), hs hs exists z in X: hs (x_1 rel z) and (x_2 rel z).
  $
] <crdef>

#th[
  Пусть $rel$ рефлексивно и обладает свойством Чёрча-Россера. Тогда для отношения $~~ #h(3pt)= Equiv(rel)$ справедливо:
  $
    x ~~ y ==> exists z: hs (x rel z) and (y rel z)
  $
] <bottom>
#pf[
  Индукция по определению отношения $~~$. Пусть $x ~~ y$. Тогда возникают три случая:
  - $x ~~ y <== x rel y$. Тогда положим $z == y$.
  - $x ~~ y <== y ~~ x$. Тогда возьмём $z$ по предположению индукции.
  - $x ~~ y <== (x ~~ L) and (L ~~ y)$. Тогда рассмотрим $z_1, z_2 in #L: hs (z_1 rrel x,L) and (z_2 rrel L,y)$. Поскольку $CR(rel)$, найдётся л-выражение $z$, такое, что $(z_1 rel z) and (z_2 rel z)$. Оно искомое.
  q.e.d.
]

== Теорема Чёрча-Россера

Сначала мы докажем, что отношение $->_beta$ обладает свойством Чёрча-Россера.

#lm[
  Пусть $rel$ --- бинарное отношение на множестве $X$ и пусть $rel' = Trans(rel)$ --- его транзитивное замыкание. Тогда #hs
  $CR(rel) ==> CR(rel').$
] <transcr>
#pf[
  Пусть $x rel' x_1,, x rel' x_2$. Тогда для каждого отношения возможны два случая, и все четыре можно представить на диаграмме:
  #v(.3cm)
  #align(center, lambda-diagram(
    spacing: 1cm,
    {
      let x = (0,0)
      let u1 = (1,0)
      let u2 = (2,0)
      let w1 = (0,-1)
      let w2 = (0,-2)
      let z1 = (1,-1)
      let z2 = (2,-1)
      let z3 = (1,-2)
      let z4 = (2,-2)
      node(x, $x$)
      node(u1, $u_1$)
      node(u2, $u_2$)
      node(w1, $w_1$)
      node(w2, $w_2$)
      node(z1, $z_1$)
      node(z2, $z_2$)
      node(z3, $z_3$)
      node(z4, $z_4$)
      edge(x, u1, "-straight")
      edge(u1, u2, "-straight")
      edge(x, w1, "-straight")
      edge(w1, w2, "-straight")
      edge(w1, z1, "--straight")
      edge(u1, z1, "--straight")
      edge(w2, z3, "--straight")
      edge(u2, z2, "--straight")
      edge(z1, z2, "--straight")
      edge(z1, z3, "--straight")
      edge(z2, z4, "--straight")
      edge(z3, z4, "--straight")
    }
  ))
  q.e.d.
]

#let arr = math.arrow.r.squiggly

#def[
  Рассмотрим бинарное отношение $arr$, определённое индуктивно следующим образом:
  - $M arr M$;
  - $M arr M' ==> ::x.. M arr ::x.. M'$;
  - $M arr M',, N arr N' ==> M N arr M'N'$;
  - $M arr M',, N arr N' ==> (::x.. M)N arr M'[x := N']$.
]

#lm[
  Если $M arr M'$ и $N arr N'$, то $M[x := N] arr M'[x := N']$.
] <arrsubs>
#pf[
  Индукция по определению $M arr M'$.
  + $M arr M' <== M arr M$. Тогда требуется доказать, что $M[x := N] arr M[x := N']$. Проведём индукцию по структуре $M$:
    #align(center, table(
      columns: (10%, 20%, 20%, 20%),
      table.header($M$, "Правая часть", "Левая часть", "Комментарий"),
      $x$, $N$, $N'$, "ОК",
      $y$, $y$, $y$, "ОК",
      $P Q$, $P[...]Q[...]$, $P[...']Q[...']$, "предп. инд.",
      $::y.. P$, $::y.. P[...]$, $::y.. P[...']$, "аналогично"
    ))
  + $M arr M' <== ::y.. P arr ::y.. P'$, прямое следствие $P arr P'$. По предположению индукции имеем $P[x := N] arr P'[x := N']$, а тогда $::y.. P[x := N] arr ::y.. P'[x := N']$, что и требовалось доказать.
  + $M arr M' <== P Q arr P'Q'$, где $P arr P'$ и $Q arr Q'$. Тогда имеем
    $
      M[x := N] &== P[x := N]Q[x := N]\
                &arr P'[x := N']Q'[x := N']\
                &== M'[x := N'].
    $
  + $M arr M' <== (::y.. P)Q arr P'[x := Q']$, где $P arr P'$, $Q arr Q'$. Тогда
    $
      M[x := N] &== (::y.. P[x := N])(Q[x := N])\
                &arr P'[x := N'][y := Q'[x := N']]\
                &== P'[y := Q'][x := N']\
                &== M'[x := N'].
    $
  q.e.d.
]

#lm[
  + $::x.. M arr N$ влечёт $N == ::x.. M'$, где $M arr M'$;
  + $M N arr L$ влечёт либо
    - $L == M'N'$, где $M arr M'$ и $N arr N'$, либо
    - $M == ::x.. P,, L == P'[x := N']$, где $P arr P',, N arr N'$.
] <pattern>
#pf[
  Очевидно.
]

#lm[
  $arr$ удовлетворяет свойству Чёрча-Россера.
]
#pf[
  Пусть $M arr M_1,, M arr M_2$. Проводим индукцию по определению $M arr M_1$.
  + $M arr M_1 <== M == M_1$. Тогда положим $Z == M_2$.
  + $M arr M_1 <== (::x.. P)Q arr P'[x := Q']$, где $P arr P'$, $Q arr Q'$. @pattern позволяет рассмотреть два подслучая:
    - $M_2 == (::x.. P'')Q''$, где $P arr P'',, Q arr Q''$. По предположению индукции существуют л-выражения $Z_P$, $Z_Q$, такие, что
    $
      P' arr Z_P,, P'' arr Z_P,, Q' arr Z_Q,, Q'' arr Z_Q.
    $ @arrsubs позволяет взять $Z == Z_P [x := Z_Q]$ в качестве искомого (упражнение).
    - $M_2 == P''[x := Q'']$ --- аналогично.
  + $M arr M_1 <== P Q arr P'Q'$, где $P arr P'$, $Q arr Q'$. Снова два подслучая:
    - $M_2 == P''Q''$, причём $P arr P''$, $Q arr Q''$. Тогда аналогично берём $Z == Z_P [x := Z_Q]$.
    - $P == (::x.. P_1)$, $M_2 == P''_1[x := Q'']$ и $P_1 arr P''_1$, $Q arr Q''$. @pattern гарантирует, что $P' == ::x.. P'_1$, где $P_1 arr P'_1$. Применяя предположение индукции, берём $Z = Z_P_1 [x := Z_Q]$.
  + $M arr M_1 <== ::x.. P arr ::x.. P'$, где $P arr P'$. Тогда $M_2 == ::x,, P''$. По предположению индукции возьмём $Z = ::x.. Z_P$.
  q.e.d.
]

#lm[
  $->_beta$ --- это транзитивное замкание $arr$.
]
#pf[
  Очевидно по определению.
]

#th("Чёрча-Россера")[
  + $->_beta$ удовлетворяет свойству Ч.-Р.;
  + $M =_beta N ==> exists Z: hs (M ->_beta Z) and (N ->_beta Z)$.
]
#pf[
  Упражнение.
]

#cor[
  + Если $M$ имеет б-нормальную форму $N$, то $M ->>_beta N$.
  + $M$ может иметь максимум одну нормальную форму.
]
#pf[
  + Пусть $M =_beta N$, где $N$ --- б-нормальная форма. Тогда существует л-выражение $Z$, такое, что $M ->>_beta Z$ и $N ->>_beta Z$ (@bottom). Однако раз $N$ --- нормальная форма, мы заключаем, что $N == Z$ (@uniquenf), и $M ->>_beta N$.
  + Пусть $N_1, N_2$ --- б-нормальнве формы выражения $M$. Тогда $N_1 ->>_beta Z$ и $N_2 ->>_beta Z$ для некоторого $Z$. Следовательно, $N_1 == Z == N_2$.
  q.e.d.
]

Теперь мы перейдём к эт-редукции.

#def[
  Пусть $rel1, rel2, rel3, rel4$ --- бинарные отношения на множестве $X$. Следующая диаграма,
  #diamond-diagram(
    spacing: 2cm,
    $rel1$,
    $rel2$,
    $rel3$,
    $rel4$
  )
означает "$forall x, x_1, x_2 in X: hs (x rel1 x_1) and (x rel2 x_2), hs hs exists z in X: hs (x_2 rel3 z) and (x_1 rel4 z)$".
]

#note[
  Свойство Чёрча-Россера можно переформулировать в этой нотации.
]

#def[
  Пусть $rel1$ и $rel2$ --- два бинарных отношения на $X$. Мы говорим, что $rel1$ и $rel2$ _коммутируют,_ если
  #diamond-diagram(
    spacing: 2cm,
    $rel1$,
    $rel2$,
    $rel1$,
    $rel2$
  )
]

#note[
  Отношение $rel$ обладает свойством Ч.-Р. $<=>$ $rel$ коммутирует само с собой.
]

#state("лемма Хиндли-Росена")[
  Пусть $rel1$, $rel2 #h(2pt) subset X times X$ таковы, что
  + $CR(rel1), CR(rel2)$;
  + $rel1$ и $rel2$ коммутируют.
  Тогда #h(2pt) $Trans(rel1 union rel2)$ также обладает свойством Чёрча-Россера.
]
#pf[
  Упражнение.
]

#lm[
  Пусть $rel1, rel2$ --- бинарные отношения на множестве $X$. Допустим также, что
  #diamond-diagram(
    spacing: 2cm,
    $rel1$,
    $rel2$,
    $Refl(rel1)$,
    $Preord(rel2)$
  )
  Тогда отношения $Preord(rel1)$ и $Preord(rel2)$ коммутируют.
] <commute>
#pf[
  Диаграммный поиск (лень рисовать).
]

#lm[
  $->>_eta$ удовлетворяет свойству Чёрча-Россера.
]
#pf[
  Так как $->>_eta hs = Preord(->_eta) = Trans(Refl(->_eta))$, достаточно доказать утверждение для отношения $Refl(->_eta) =: hs (arr)$ (@transcr). Предположим теперь, что $M arr M_1$ и $M arr M_2$. Без ограничения общности, допустим, что все три выражения $M, M_1, M_2$ различны (иначе очевидно). Индукция по определению $M arr M_1$:
  + $M arr M_1 <== ::x.. P x arr P$. Тогда $M_2 = ::x.. P'x$, где $P arr P'$. Положим $Z == P'$ и дело в шляпе.
  + $M arr M_1 <== K P arr K P'$, где $P arr P'$. Тогда если $M_2 == K' P$, $K arr K'$, то положим $Z == K' P'$. Если же $M_2 == K P''$, $P' arr P''$, то воспользуемся предположением индукции: $exists Z_P : hs P', P'' arr Z_P$. Положим $Z = K Z_P$.
  + $M arr M_1 <== P K arr P' K$, где $P arr P'$. Аналогично с предыдущим случаем.
  + $M arr M_1 <== ::x.. P arr ::x.. P'$, где $P arr P'$.
    + $M_2 == ::x.. P''$, $P arr P''$. Тогда положим $Z == ::x.. Z_P$, где $Z_P$ взято из предположения индукции.
    + $P == P_0 x$, $M_2 == P_0$. Тогда $P' == P'_0 x$, и мы можем положить $Z == P'_0$.
  q.e.d.
]

#lm[
  $->>_beta$ коммутирует с $->>_eta$.
]
#pf[
  @commute сводит доказательство к следующей диаграмме:
  #diamond-diagram(
    prepad: -10pt,
    spacing: 1.8cm,
    $->_beta$,
    $->_eta$,
    $Refl(->_beta)$,
    $->>_eta$
  )
  Упражнение.
]

#th([теорема Чёрча-Россера для бэ-редукции])[
  + $->>$ удовлетворяет свойству Чёрча-Россера;
  + $M = N ==> exists Z: hs (M ->> Z) and (N ->> Z)$.
]
#pf[
  Упражнение.
]

#cor[
  - Если $M$ имеет бэ-нормальную форму $N$, то $M ->> N$;
  - $M$ может иметь максимум одну нормальную форму;
  - Теория $#l beta eta$ согласованна;
  - л-выражение $#O = (::x.. x x)(::x.. x x)$ не имеет нормальной формы.
]
#pf[
  Очевидно, применяя @uniquenf.
]

== Стандартная редукция

#def[
  + л-выражение $M in #L$ называется _внешней нормальной формой,_ если оно имеет форму
    $
      M == ::x_1,. ... ,.x_n.. x M_1 ... M_m,
    $
    где $n,m ::= 0$.
  + Если $M$ имеет форму
    $
      M == ::x_1,. ... ,.x_n.. (::x.. M_0) M_1 ... M_m, hs n ::= 0, m ::= 1,
    $
    то выражение $(::x.. M_0) M_1$ называется _внешним редексом._
  + $->_h$ (соотв. $->>_h$) --- редукция, в которой сокращаются только внешние редексы.
  + Редекс $Delta$ называется _внутренним,_ если он не внешний.
  + $->_i$ (соотв. $->>_i$) --- редукция, в которой сокращаются только внутренние редексы.
]

#def[
  Пусть $M$ --- л-выражение. Редекс $Delta_1$ в $M$ _левее_ редекса $Delta_2$, если первая "$#l$" в $Delta_1$ левее, чем первая "$#l$" в $Delta_2$.
]

#lm[
  Пусть $M, N in #L$ и $M ->> N$. Тогда существует $Z in #L$, такое, что $M ->>_h Z ->>_i N$.
]
#pf("(эскиз)")[
  Ключ в том, что внешняя и внутренняя редукции коммутируют:
  #{
    v(-5pt)
    align(center, lambda-diagram(
      spacing: 2cm,
      {
        edge((0,0), (2,0), "-", label: $h$, label-side: left)
        edge((0,0), (0,-1), "--", label: $i$, label-side: right)
        edge((0,-1), (2,-1), "--", label: $h$, label-side: right)
        edge((2,0), (2,-1), "-", label: $i$, label-side: left)
      }
    ))
    v(-3pt)
  }
  Редукция $M ->> N$ представляется как
  $
    M ->>_h M_1 ->>_i M_2 ->>_h M_3 ->>_i ... ->>_i N.
  $
  Переставляя редукции, получаем искомое разбиение.
]

#def[
  Пусть $sigma$ --- это редукционная последовательность, то есть
  $
    sigma : M_0 ->^(Delta_0) M_1 ->^(Delta_1) M_2 ->^(Delta_2) dots.h.c.
  $
  $sigma$ называется _стандартной,_ если $forall i$, $forall j < i$: $Delta_i$ --- не результат сокращения редекса, находящегося левее $Delta_j$. Стандартная редукция обозначается $M ->>_s N$.
]

#th[
  Пусть $M, N in #L$ и $M ->> N$. Тогда $M ->>_s N$.
]
#pf[
  Имеем $M ->>_h Z ->>_i N$ для какого-то $Z in #L$. Индукция по длине выражения $N$.
  + $N = x in V$. Тогда $Z == x$ и доказательство завершено.
  + $N == ::x_1,. ... ,.x_n.. N_0 N_1 ... N_m$, где $n + m > 0$. Тогда $Z$ должно иметь форму
  $
    ::x_1,. ... ,.x_n.. Z_0 Z_1 ... Z_m,
  $
  где $Z_i ->> N_i$ при $0 <= i < m$. По предположению индукции имеем $Z_i ->>_s N_i$. Тогда $Z ->>_s N$ и доказательство завершено.
]

== Редукционные стратегии

#def("редукционная стратегия")[
  Отображение $F : #L -> #L$ называется _редукционной стратегией,_ если для любого $M in #L$ выполняется редукция $ M ->> F(M). $
]

#def[
  + Пусть $F$ --- редукционная стратегия. _$F$-редукционный путь_ выражения $M$ --- это последовательность
    #v(-5pt)
    $
      M,, F(M),, F^2(M),, ...
    $
  + $F$ называется _нормализующей,_ если лдя любого $M in #L$, имеющего нф, $F^n (M)$ находится в нормальной форме для некоторого $n in NN$.
]

#def[
  _Крайняя левая редукционная стратегия,_ $F_l$, определяется следующим образом:
  - $F_l (M) = M$, если $M$ в нормальной форме.
  - $F_l (M) = M'$, если $M ->^Delta M'$, где $Delta$ --- крайний левый редекс в $M$.
]

#th("о нормализации")[
  $F_l$ --- нормализующая стратегия.
]
#pf[
  Пусть выражение $M$ имеет нормальную форму $N$. Тогда по теореме Чёрча-Россера имеем $M ->> N$. Тогда по предыдущей теореме есть стандартная редукционная последовательность
  $
    sigma : M == M_0 ->^(Delta_0) M_1 ->^(Delta_1) -> dots.h.c ->^(Delta_(n-1)) M_n == N.
  $
  Утверждается, что $sigma$ --- это редукционный путь стратегии $F_l$. Допустим противное. Тогда на каком-то шагу редекс $Delta_i$ --- не крайний левый, а значит он уже не сможет сократиться в дальнейшем. Тогда $N$ --- не нормальная форма. Противоречие.
]

= л-представимость

== Основные понятия

#def[
  Пусть $A == ::x,.y.. y(x x y)$. Комбинатор $Th == A A$ называется _комбинатором Тьюринга._
]

#exer[
  Доказать, что $Th$ --- комбинатор фиксированной точки, то есть $Th F ->> F (Th F)$ для любого $F in #L$.
]

#def[
  + $underline("true") == #T == ::x,. y.. x$
  + $underline("false") == #F == ::x,. y.. y$
  + Пусть $B in #L$. Тогда запись
  $
    ite(B, M, N)
  $
  обозначает л-выражение $B M N$.
]

#def[
  Пусть $M,N in #L$. _Уподядоченная пара_ $[M, N]$ определяется как
  $
    [M, N] == ::z.. z M N.
  $
  Определим также $(P)_0 == P #T, hs (P)_1 == P #F$.
]

#exer[
  Показать, что $([M,N])_0 ->> M, hs ([M,N])_1 ->> N$. Правда ли, что $[(P)_0, (P)_1] = P$?
]

#def("конечные кортежи")[
  $
    [M] == M, hs hs hs hs hs [M_0, M_1, ..., M_(n+1)] == [M_0, [M_1, ..., M_(n+1)]], \
    << M_0, M_1, ..., M_n >> == ::z.. z M_0 M_1 ... M_n
  $
]

#def[
  + $ pi^n_i &== ::z.. z #F^(~~ i) #T, hs hs 0 <= i < n, \ pi^n_n &== ::z.. z #F^(~~ n) $
  + $ #P^n_i == ::z.. z(::x_1,. x_2,. ...,. x_n.. x_i), hs hs 0 <= i <= n $
]

#exer[
  Показать, что $ pi^n_i [M_0, M_1, ..., M_n] ->> M_i, #h(15pt) #P^n_i <<M_0, M_1, ..., M_n>> ->> M_i $
]

#th("обобщённая теорема о неподвижной точке")[
  Пусть $F_1, F_2, ..., F_n in #L$. Тогда существуют выражения $X_1, X_2, ..., X_n in #L$, такие, что
  $
    X_1 &= F_1 X_1 X_2 ... X_n,\
    X_2 &= F_2 X_1 X_2 ... X_n,\
        &dots.v\
    X_n &= F_n X_1 X_2 ... X_n.
  $
]
#pf[
  Определим выражения
  $
    M &== ::f,.x.. f (pi^n_1 x) (pi^n_2 x) ... (pi^n_n x),\
    F &== ::x.. <<M F_1 x, M F_2 x, ..., M F_n x>>.
  $
  Тогда по теореме о неподвижной точке найдётся выражение $X in #L: hs X = F X$. Наконец, положим $X_i == pi^n_i X$. Действительно,
  $
    X_i == pi^n_i X = M F_i X = F_i X_1 X_2 ... X_n,
  $
  q.e.d.
]

#def[
  Пусть $M, N in #L$. _Композиция_ $M circ N$ определяется как $::x.. M(N x)$, где $x in.not FV(M) union FV(N)$.
]

#def[
  + _Числа Барендрегта_ (или просто л-числа) --- это следующая последовательность л-выражений:
    $
      num(0) == #I, #h(20pt) num(n+1) == [#F, num(n)]
    $
    Заметим, что все л-числа --- различные нормальные формы.
  + Определим $ Sp == ::z.. [F, z], #h(1cm) Pm == ::z.. z#F, #h(1cm) Zero == ::z.. z#T $
]

#exer[
  $Sp (num(n)) = num(n+1), hs hs Pm (num(n+1)) = num(n), hs hs Zero (num(0)) = #T, hs hs Zero (num(n+1)) = #F$
]

#def[
  Пусть $P : NN_0 -> {"true", "false"}$ --- предикат на натуральных числах. Запись
  $
    mu m [ P(m) ]
  $
обозначает наименьшее число $m$, такое, что выполняется $P(m)$, если такое число существует. В противном случае $mu m [ P(m) ]$ неопределено.
]

== Рекурсивные функции

#def[
  + _Числовая функция_ --- это отображение $NN_0^p -> NN_0$, для некоторого $p in NN$.
  + Числовая функция $phi : NN_0^p -> NN_0$ называется л-представимой, если существует выражение $F in #L$, такое, что
    $
      forall n_1, n_2, ..., n_p in NN_0: #h(1cm) F nums(n_1) nums(n_2) ... nums(n_p) = num(phi(n_1, n_2, ..., n_p))
    $
  + Если $arrow(n) = n_1, n_2, ..., n_p$, то положим $ num(arrow(n)) = num(n_1), num(n_2), ..., num(n_p). $
]

#def("первичные функции")[
  Функции $U^p_i,, S^+,, Z$ называются _первичными:_
  $
    U^p_i (n_0, n_1, ..., n_p) = n_i, hs hs 0 <= i <= p,\
    S^+ (n) = n+1, #h(1cm) Z(n) = 0.
  $
]

#def[
  #let A = $cal(A)$
  Пусть #A --- некий класс числовых функций.
  + #A называется _замкнутым относительно суперпозиции,_ если для любых $chi, psi_1, psi_2, ..., psi_m in #A$, функция 
    $
      phi (arrow(n)) = chi (psi_1(arrow(n)), psi_2(arrow(n)), ..., psi_m(arrow(n)))
    $
    лежит в #A.
  + #A называется _замкнутым относительно примитивной рекурсии,_ если лдя любых $chi, psi in #A$, функция
    $
      phi (0, arrow(n)) = chi (arrow(n)),\
      phi (k+1, arrow(n)) = psi (phi (k, arrow(n)), k, arrow(n))
    $
    лежит в #A.
  + #A называется _замкнутым относительно минимизации,_ если для любой функции $chi in #A: hs forall arrow(n) hs exists m hs hs chi (arrow(n), m) = 0$, функция
    $
      phi (arrow(n)) = mu m [ chi (arrow(n), m) = 0 ]
    $
    лежит в #A.
  + Класс $cal(R)$ _рекурсивных функций_ -- это наименьший класс числовых функций, который содержит все первичные функции, а также замкнут относительно суперпозиции, примитивной рекурсии и минимизации. 
]

#lm[
  Все первичные функции л-представимы.
]
#pf[
  Очевидно.
]

#lm[
  л-представимые функции замкнуты относительно суперпозиции.
]
#pf[
  Упражнение.
]

#lm[
  л-представимые функции замкнуты относительно примитивной рекурсии.
]
#pf[
  Пусть функция $phi$ задаётся соотношениями
  $
    phi (0, arrow(n)) = chi (arrow(n)),\
    phi (k+1, arrow(n)) = psi (phi (k, arrow(n)), k, arrow(n)),
  $
  где $chi$ и $psi$ л-представлены выражениями $G$ и $H$ соответственно. Рассмотрим выражение
  $
    X == ::f.. ::x,. arrow(y).. (ite(
      Zero x,
      G arrow(y),
      H #h(2pt) (f (Pm x) arrow(y)) #h(2pt) (Pm x) #h(2pt) arrow(y)
    )).
  $
  л-выражение $F == #Y X$ представляет функцию $phi$ (упражнение).
]

#def[
  Пусть $P in #L$. Определим
  $
    H_P &== Th (::h,.z.. ite(P z, z, h (Sp z))),\
    mu P &== H_P num(0).
  $
]
#state[
  Пусть $P in #L$ таково, что при всех $n in NN_0$ либо $P num(n) = #T$, либо $P num(n) = #F$. Тогда:
  + $H_P z ->> ite(P z, z, H_P (Sp z))$;
  + $mu P = num(mu n [ P num(n) = #T ])$ (если минимум существует).
]
#pf[
  + Упражнение.
  + Допустим, что $mu n [ P num(n) = #T ] = m$. Тогда имеем
    $
      H_P num(m) &= num(m),\
      forall n < m: hs hs H_P num(n) &= H_P num(n+1) = H_P num(n+2) = ... = H_P num(m) = num(m).
    $
    Отсюда получаем, что $mu P == H_P num(0) = num(m)$,
  q.e.d.
]

#lm[
  л-представимые функции замкнуты относительно минимизации.
]
#pf[
  Пусть
  $
    phi (arrow(n)) = mu m [ chi (arrow(n), m) = 0 ],
  $
  где $G in #L$ представляет функцию $chi$. Определим $F in #L$ как
  $
    F arrow(x) = mu (::y.. Zero (G arrow(x) y)).
  $
  По предыдущему утверждению, $F$ представляет функцию $phi$.
]

#[
  #let cor = mystatestyle("corollary", "Следствие", base: "theorem", numbering: none)
  #cor[
    Все рекурсивные функции л-представимы.
  ]
]

#lm[
  Пусть $phi$ л-представляется выражением $F$. Тогда для всех $arrow(n), m in NN_0$
  $
    phi (arrow(n)) = m hs <=> hs F num(arrow(n)) = num(n)
  $
]
#pf[
  / ($==>$): Очевидно по определению.
  / ($<==$): Предположим, что $F num(arrow(n)) = num(m)$. Тогда $num(phi (arrow(n))) = num(m)$. Так как л-числа --- это различные нормальные формы, по теореме Чёрча-Россера имеем $phi (arrow(n)) = m$,
  q.e.d.
]

#th("Клини")[
  Функция $phi : NN_0^p -> NN_0$ рекурсивна $<==>$ $phi$ л-представима.
]
#pf("(эскиз)")[
  / ($==>$): Очевидно.
  / ($<==$): 
]
