#import "@local/common:0.0.0": *
#import "@local/theorem:0.0.0": *
#show: theorem
#import "./template.lib.typ": *
#show: formatting

#import "@preview/cetz:0.2.2" as cz

// #set list(indent: 0.1in)
#set enum(numbering: n => strong([#n.]))
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

= Конверсия и Редукция

== Основные понятия

#def[
  Рассмотрим счётное множество $V = {v, v', v'', ...}$. Элементы этого множества будут называться _переменными._ Множество л-выражений, #L, --- это наименьшее множество, удовлетворяющее следующим условиям: #v(2pt)
  - $x in V ==> x in #L$;
  - $x in V,, M in #L==> (>x M) in #L$; #h(1fr) (абстракция, морально: определение функции)
  - $M in #L,, N in #L ==> (M N) in #L$. #h(1fr) (комбинация, морально: применение функции к аргументу)
]

#exam[
  л-выражения в формальной нотации:
  $
    v';\
    (v v');\
    (>v (v' v));\
    ((>v (v' v))v'');\
    (((>v (>v' (v' v))) v'')v''');
  $
]

#nota[
  - $x$, $y$, $z$, ... обозначают произвольные переменные из множества $V$.
  - $M$, $N$, $K$, ... обозначают произвольные л-выражения из #L.
  - Внешние скобки опускаются: $(>x(y z)) -> >x (y z)$.
  - Многократная абстракция сокращается:
  $
    >x_1 (>x_2 (>... (>x_n M)...)) -> >x_1,.x_2,. ... ,.x_n.. M -> >arrow(x).. M
  $
  - Многократная комбинация сокращается:
  $
    ((...((M_1 M_2)M_3)...)M_n)N -> M_1M_2...M_n N -> arrow(M)N
  $
  // - $norm(M)$ обозначает количество символов в выражении $M$.
  - Комбинация берёт приоритет над абстракцией: $>x.. y z = #l x.. (y z)$
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
      $>x.. N$,       ${x} union TV(N)$,      $FV(N) > {x}$,         ${x} union BV(N)$,
      $N K$,          $TV(N) union TV(K)$,    $FV(N) union FV(K)$,    $BV(N) union BV(K)$
    )
  }
]

#note[
  В данный момент существуют не вполне осмысленные л-выражения. Так, в выражении $(>x.. x y)x$ переменная $x$ выступает одновременно связанной и свободной, а в выражении $>x.. >x.. x x$ переменная $x$ связывается дважды. Обе этих проблемы можно исправить _заменой связанных переменных:_ $(>x.. x y)x -> (>u.. u y)x$, $>x.. >x.. x x -> >x.. >u.. u u$. Сейчас мы формализуем эту идею.
]

#def[
  Пусть $rel$ --- бинарное отношение на множестве #L. Тогда $rel$ называется _совместимым с операциями,_ если:
  $
    M &rel N ==> >x.. M rel >x.. N,\
    M &rel N ==> Z M rel Z N,\
    M &rel N ==> M Z rel N Z.
  $
]

#def[
  _Тождественное равенство_ ($equiv$) обозначает полностью идентичный состав символов: $>x.. x y equiv.not >u.. u y$.
]

#def[
  Отношение _ал-конгруэнтности_ ($acongr$) на #L --- это наименьшее подмножество $#L times #L$, удовлетворяющее следующим условиям:
  - $M acongr M$;
  - $>x.. M acongr >y.. (M[x -> y])$, при условии что $y in.not TV(M)$;
  - $acongr$ совместимо с операциями.
]

#def[
  Пусть $M$ --- л-выражение. $M$ называется _корректным_ в следующих случаях:
  + $M equiv x in V$;
  + $M equiv >x.. N$, причём $N$ корректно, а также $x in.not BV(N)$;
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
    $>x.. K$, ${>x.. K} union Sub(K)$,
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
    (>y.. M')[x := N] &equiv >y.. (M'[x := N]);\
    (M_1M_2)[x := N] &equiv (M_1[x := N])(M_2[x := N]).
  $
]

#note[
  Рассмотрим $M equiv >y.. x,, N equiv y y$. Тогда по предыдущему определению мы получаем $M[x := N] = >y.. y y$, что настораживает, ведь $M equiv >y.. x acongr >u.. x equiv M'$, тогда как $ M[x := N] = >y.. y y eq.not^alpha >u.. y y = M'[x := N]. $ Однако заметим, что такая ситуация некорректна, ведь $BV(M) sect FV(N) = {y} eq.not empty$.
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
    - $M equiv >z.. M'$. По правилу переменых и определению оператора подстановки мы имеем $z in.not FV(N L)$ и $z equiv.not x,y$. Тогда по предположению индукции
    $
      (>z.. M')[x := N][y := L] &== >z.. M'[x := N][y := L]\
      &== >z.. M'[y := L][x := N[y := L]]\
      &== (>z.. M')[y := L][x := N[y := L]].
    $
    - $M == M_1M_2$. Доказательство аналогично.
  q.e.d.
]

#def("бэ-конверсия")[
  Отношение бэ-конверсии ($conv$) --- это наименьшее подмножество $#L times #L$, удовлетворяющее следующим условиям:
  - $(>x.. M)N = M[x := N]$; #h(1fr) ($beta$-конверсия)
  - $>x.. M x = M$, при условии что $x in.not TV(M)$; #h(1fr) ($eta$-конверсия)
  - $conv$ --- отношение эквивалентности;
  - $conv$ совместимо с операциями.
  Если $M conv M$, мы говорим, что "$M$ равно $N$", или "$M$ конвертируется в $N$". Запись "$proves M conv N$" означает, что конверсию $M conv N$ можно вывести из вышеуказанных правил.
]

#th("о неподвижной точке")[
  $forall F in #L: hs exists X in #L: hs F X = X$.
]
#pf[
  Пусть $W == >x.. F(x x)$ и $X == W W$. Тогда имеем $ X == W W == (>x.. F(x x))W conv F(x x)[x := W] == F(W W) == F X, $ q.e.d.
]

#state[
  $forall M,N in #L: hs proves M conv N$
]
#pf[
  Рассмотрим $F == >x,.y.. y x$. Тогда для любых $M,N$ имеем
  $
    F M N == ((>x.. (>y.. y x))M)N conv (>y.. y M)N conv N M.
  $
  В частности, $F y x conv x y$. Однако
  $
    F y x == ((>x.. (>y.. y x))y)x conv (>y.. y y)x = x x.
  $
  Тогда $x y conv x x$, а значит $F_1 == >x,.y.. x y conv >x,.y.. x x == F_2$. Теперь для любого $M in #L$ имеем
  $
    M conv (>x.. x)M conv F_1 (>x.. x) M conv F_2 (>x.. x) M conv (>x.. x)(>x.. x) conv (>x.. x),
  $
  и по транзитивности $M conv (>x.. x) conv N$ для любых $M, N in #L$. В чём ошибка?
]

#lm[
  Оператор подстановки уважает конверсию. Иначе говоря, если $M conv M',, N conv N'$, то $M[x := N] conv M'[x := N']$.
]
#pf[
  Упражнение.
  // Действительно,
  // $
  //   M conv M' &==> >x.. M conv >x.. M'\
  //             &==> (>x.. M)N conv (>x.. M')N conv (>x.. M')N'\
  //             &==> M[x := N] conv M'[x := N'],
  // $
  // q.e.d.
]

== Комбинаторы и согласованность

#def[
  - $#I == >x.. x$
  - $#K == >x,.y.. x$
  - $#KK == >x,.y.. y$
  - $#S == >x,.y,.z.. x z(y z)$
  - $#Y == >f.. (>x.. f(x x))(>x.. f(x x))$ --- комбинатор неподвижной точки: $forall F in #L: hs F(#Y F) = #Y F$.\
    Этот комбинатор позволяет моделировать простую рекурсию. РАссмотрим л-выражение $M$, определённое рекуррентной формулой:
    $
      M x == F x M.
    $
    Определим $G == >y.. >x.. F x y$. Тогда $M$ приобретает явную форму: $M == #Y G$ (упражнение).
]

#def[
  - Выражение вида $M conv N$ называется _равенством;_
  - Равенство $M conv N$ называется _замкнутым,_ если $M,N in #L^0$;
  - Пусть $cal(T)$ --- формальная теория, т.е. набор правил, с помощью которых можно выводить равенства (наподобие л-теории). Тогда $cal(T)$ называется _согласованной_ (нотация $Con(cal(T))$) если $cal(T)$ *не* доказывает все замкнутые равенства. В противном случае $cal(T)$ называется _противоречивой._
  - Если $cal(T)$ --- это набор равенств, то $#l + cal(T)$ обозначает теорию, полученную добавлением равенств из $cal(T)$ к стандартному списку аксиом бэ-конверсии.
]

#def[
  Пусть $M,N in #L$. Тогда $M$ и $N$ называются _несовместимыми_ (нотация $M inc N$), если теория $#l + (M conv N)$ противоречива.
]

#exam[
  $#I inc #K$
]
#pf[
  Имеем $#I M N conv #K M N$ для любых $M,N in #L$. По определению комбинаторов $#I$ и $#K$, имеем $M N conv M$. Подставляя $M == #I$, получаем $N conv #I hs forall N in #L$.
]

== Нормальные формы

#def[
  - л-выражение $M$ называется _бэ-нормальной формой,_ если оно *не* имеет подвыражений вида $(>x.. M)N$ или $>y.. (M y)$ (где $y in.not TV(M)$).
  - $M$ _имеет нормальную форму $N$,_ если $M conv N$ и $N$ --- нормальная форма.
]

#exam[
  - $#I$ находится в нормальной форме;
  - $#K#I$ имеет нормальную форму $>y.. #I$;
  - Комбинатор $#O = (>x.. x x)(>x.. x x)$ не имеет нормальной формы (доказательство позже).
]

#underline(strong([ Воспоминания о будущем. ]))
- $M$ имеет бэ-нормальную форму тогда и только тогда, когда имеет $beta$-нормальную форму;
- Если $M$ и $N$ --- различные бэ-нф, то $#l tack.r.not M conv N$;
- Следствие: $#l$ --- согласованная теория (упражнение);
- Если $M$ и $N$ --- различные бэ-нф, то $M inc N$
- Следствие: пусть $M$ и $N$ имеют нормальную форму. Тогда либо $M conv N$, либо $M inc N$;

== Редукция

#note[
  В правилах конверсии есть определённая асимметрия. Так, о конверсии $ (>x.. x^2 + 1)3 conv 10 $ можно сказать, что "10 является результатом _упрощения_ выражения $(>x.. x^2 + 1)3$", но никак не в обратную сторону. Сейчас мы формализуем эту асимметрию. 
]

#def[
  + Отношение $->$ (_редукция за один шаг_) --- это наименьшее подмножество $#L times #L$, такое что:
    - $(>x.. M)N -> M[x := N]$;// #h(1fr) ($->_beta$)
    - $>x.. M x -> M$, если $x in.not TV(M)$;// #h(1fr) ($->_eta$)
    - $->$ совместимо с операциями.
  + Отношение $->>$ (_редукция_) --- это замыкание $->$ до предпорядка: $->> #h(3pt) = "Preord"(->)$;
  + Отношение $conv$ (_конгруэнтность_ или _эквивалентность_) --- это замыкание $->>$ до отношения эквивалентности: $conv #h(3pt)= "Equiv"(->>)$
]

#def[
  + л-выражения вида $(>x.. M)N$ называются _$beta$-редексами;_ соотв. отношения: $->_beta$, $->>_beta$, $conv_beta$
  + л-выражения вида $>x.. M x$ называются _$eta$-редексами._ соотв. отношения: $->_eta$, $->>_eta$, $conv_eta$
  + $M$ --- _нормальная форма_ (или _в нормальной форме_), если $M$ не содержит редексов.
  + Пусть $Delta$ --- редекс в выражении $M$. Запись $M ->^Delta N$ означает, что $N$ получается из $M$ сокращением редекса $Delta$: #hs $N == M[Delta -> Delta']$
  + _Редукционный путь_ --- это последовательность (конечная или бесконечная) вида
    $
      M_0 ->^(Delta_0) M_1 ->^(Delta_1) M_2 -> ...
    $
]

#exam[
  - Определим $omega_3 = >x.. x x x$. Это выражение порождает бесконечный редукционный путь:
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
]
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
  Пусть $rrel$ --- рефлексивное отношение на множестве #L. $rrel$ _обладает свойством Чёрча-Россера_ (нотация $CR(rrel)$), если
  $
    forall M, M_1, M_2 in #L: hs (M rrel M_1) and (M rrel M_2), hs hs exists Z in #L: hs (M_1 rrel Z) and (M_2 rrel Z).
  $
]

#th[
  Пусть $rrel$ обладает свойством Чёрча-Россера. Тогда для отношения $~~ #h(3pt)= "Equiv"(rrel)$ справедливо:
  $
    M ~~ N ==> exists Z: hs (M rrel Z) and (N rrel Z)
  $
]
#pf[
  Индукция по определению отношения $~~$. Пусть $M ~~ N$. Тогда возникают три случая:
  - $M rrel N ==> M ~~ N$. Тогда положим $Z == N$.
  - $N ~~ M ==> M ~~ N$. Тогда возьмём $Z$ по предположению индукции.
  - $M ~~ L and L ~~ N ==> M ~~ N$. Тогда рассмотрим $Z_1, Z_2 in #L: hs (Z_1 rel M,L) and (Z_2 rel L,N)$. Поскольку $CR(rrel)$, найдётся л-выражение $Z$, такое, что $(Z_1 rrel Z) and (Z_2 rrel Z)$. Оно искомое.
  q.e.d.
]

== Теорема Чёрча-Россера для б- и бэ-редукции

Сначала мы докажем, что отношение $->_beta$ обладает свойством Чёрча-Россера.

#lm[
  Пусть $rrel$ --- бинарное отношение на #L и пусть $rrel'$ --- его транзитивное замыкание. Тогда #hs
  $CR(rrel) ==> CR(rrel').$
]
#pf[
  Пусть $M rrel' M_1,, M rrel' M_2$. Тогда для каждого отношения возможны два случая, и все четыре можно представить на диаграмме:
  #v(.3cm)
  #align(center, lambda-diagram(
    1cm,
    {
      let M = (0,0)
      let N1 = (1,0)
      let N2 = (2,0)
      let K1 = (0,-1)
      let K2 = (0,-2)
      let Z1 = (1,-1)
      let Z2 = (2,-1)
      let Z3 = (1,-2)
      let Z4 = (2,-2)
      node(M, $M$)
      node(N1, $N_1$)
      node(N2, $N_2$)
      node(K1, $K_1$)
      node(K2, $K_2$)
      node(Z1, $Z_1$)
      node(Z2, $Z_2$)
      node(Z3, $Z_3$)
      node(Z4, $Z_4$)
      edge(M, N1, "-straight")
      edge(N1, N2, "-straight")
      edge(M, K1, "-straight")
      edge(K1, K2, "-straight")
      edge(K1, Z1, "--straight")
      edge(N1, Z1, "--straight")
      edge(K2, Z3, "--straight")
      edge(N2, Z2, "--straight")
      edge(Z1, Z2, "--straight")
      edge(Z1, Z3, "--straight")
      edge(Z2, Z4, "--straight")
      edge(Z3, Z4, "--straight")
    }
  ))
  q.e.d.
]

#let arr = math.arrow.r.squiggly

#def[
  Рассмотрим бинарное отношение $arr$, определённое индуктивно следующим образом:
  - $M arr M$;
  - $M arr M' ==> >x.. M arr >x.. M'$;
  - $M arr M',, N arr N' ==> M N arr M'N'$;
  - $M arr M',, N arr N' ==> (>x.. M)N arr M'[x := N']$.
]

#lm[
  Если $M arr M'$ и $N arr N'$, то $M[x := N] arr M'[x := N']$.
]
#pf[
  Индукция по определению $M arr M'$.
  + $M arr M' <== M arr M$. Тогда требуется доказать, что $M[x := N] arr M[x := N']$. Проведём индукцию по структуре $M$:
    #align(center, table(
      columns: (10%, 20%, 20%, 20%),
      table.header($M$, "Правая часть", "Левая часть", "Комментарий"),
      $x$, $N$, $N'$, "ОК",
      $y$, $y$, $y$, "ОК",
      $P Q$, $P[...]Q[...]$, $P[...']Q[...']$, "предп. инд.",
      $>y.. P$, $>y.. P[...]$, $>y.. P[...']$, "аналогично"
    ))
  + $M arr M' <== >y.. P arr >y.. P'$, прямое следствие $P arr P'$. По предположению индукции имеем $P[x := N] arr P'[x := N']$, а тогда $>y.. P[x := N] arr >y.. P'[x := N']$, что и требовалось доказать.
  + $M arr M' <== P Q arr P'Q'$, где $P arr P'$ и $Q arr Q'$. Тогда имеем
    $
      M[x := N] &== P[x := N]Q[x := N]\
                &arr P'[x := N']Q'[x := N']\
                &== M'[x := N'].
    $
  + $M arr M' <== (>y.. P)Q arr P'[x := Q']$, где $P arr P'$, $Q arr Q'$. Тогда
    $
      M[x := N] &== (>y.. P[x := N])(Q[x := N])\
                &arr P'[x := N'][y := Q'[x := N']]\
                &== P'[y := Q'][x := N']\
                &== M'[x := N'].
    $
  q.e.d.
]

#lm[
  + $>x.. M arr N$ влечёт $N == >x.. M'$, где $M arr M'$;
  + $M N arr L$ влечёт либо
    - $L == M'N'$, где $M arr M'$ и $N arr N'$, либо
    - $M == >x.. P,, L == P'[x := N']$, где $P arr P',, N arr N'$.
]
