#import "@local/common:0.0.0": *
#import "@local/theorem:0.0.0": *
#show: theorem
#import "./template.lib.typ": *
#show: formatting
#set heading(numbering: none)
#set text(size: 13pt)
#set page(background: image("pictures/troubles-faded.jpg", width: 100%, height: 100%, fit: "stretch"))

#head([ Итоговая работа, I вариант ])

== На 3:

+ Дайте определение множеств всех, свободных и связанных переменных для л-выражения $M$ ($TV(M), FV(M), BV(M)$). Правда ли, что
  $ TV(M) = FV(M) union BV(M) $
  для любого $M in #L$?

+ Определите _результат подстановки,_ $M[x := N]$, для л-выражений $M, N$ и переменной $x in.not BV(M)$.

+ Определите комбинаторы $#I$, $#K$, $#KK$, $#S$. Покажите, что $#K#I = #KK$ и $#S#K#K = #I$.

+ Дайте определение _нормальной формы._ Покажите, что, если $M$ --- нормальная форма, то для любого $N in #L$ редукция $M ->> N$ влечёт $M equiv N$.

== На 4:

+ Докажите _лемму о подстановке:_ если $M, N, L in #L$, $x equiv.not y$ и $x in.not FV(L)$, то тогда #h(1fr)
  $
    (M[x := N])[y := L] equiv (M[y := L])[x := N[y := L]]
  $

+ Докажите _теорему о неподвижной точке:_ $forall F in #L: hs exists X in #L: hs X = F X$. Дайте определение _комбинатора неподвижной точки._

+ Рассмотрим бинарное отношение $arr$, заданное рекурсивно следующими соотношениями:
  + $M arr M$;
  + $M arr M' ==> @x.. M arr @x.. M'$;
  + $M arr M',, N arr N' ==> M N arr M'N'$;
  + $M arr M',, N arr N' ==> (@x.. M)N arr M'[x := N']$.

  Покажите, что $Refl(->_beta) subset arr$. Докажите, если $M arr M'$ и $N arr N'$, то\ $M[x := N] arr M'[x := N']$.

== На 5:

+ Правда ли, что всякая произвольная комбинация выражений $#S$ и $#K$ (например, $#S (#K#S#K) #S #K$) будет иметь нормальную форму?

+ Докажите, что
  $
    (forall N in #L: hs N =_(beta eta) M ==> x in FV(N)) <==> (forall N in #L: hs M ->>_(beta eta) N ==> x in FV(N)).
  $

+ Покажите, что для любого $M in #L$ существует нормальная форма $N in #L$, такая, что $N #I ->> M$.
