#import "@local/common:0.0.0": *
#import "@local/theorem:0.0.0": *
#show: theorem
#import "./template.lib.typ": *
#show: formatting(bg: false)
#set list(indent: 0.5cm)

#let myplainstyle(ident, head, ..args) = thmstyle(ident, head, base: none, titlefmt: it => strong(underline(it)), ..args)
#let mystatestyle(ident, head, ..args) = thmstyle(ident, head, base: none, bodyfmt: emph, ..args)

#let def = myplainstyle("definition", "Определение")
#let nota = myplainstyle("notation", "Нотация", numbering: none)
#let exam = myplainstyle("example", "Пример")
#let exer = myplainstyle("example", "Упражнение")
#let th = mystatestyle("theorem", "Теорема")
#let lm = mystatestyle("lemma", "Лемма")
#let prop = mystatestyle("proposition", "Предложение")
#let cor = mystatestyle("corollary", "Следствие")
#let prb = mystatestyle("problem", "Задача")
#let pf = proofstyle("proof", "Доказательство")

#head([ Материал курса ])

#outline()
#pagebreak()

= Конверсия и Редукция

== Основные понятия

#def[
  Рассмотрим счётное множество $V = {v, v', v'', ...}$. Элементы этого множества будут называться _переменными._ Множество л-выражений, #L, --- это наименьшее множество, удовлетворяющее следующим условиям: #v(2pt)
  - $x in V ==> x in #L$;
  - $x in V, hs M in #L==> (\\x M) in #L$ (абстракция, морально: определение функции);
  - $M in #L, hs N in #L ==> (M N) in #L$ (комбинация, морально: применение функции к аргументу).
]

#exam[
  л-выражения в формальной нотации:
  // #set math.equation(numbering: none)
  $
    v';\
    (v v');\
    (#l v (v' v));\
    ((#l v (v' v))v'');\
    (((#l v (#l v' (v' v))) v'')v''');
  $
]

#nota[
  - $x$, $y$, $z$, ... обозначают произвольные переменные из множества $V$.
  - $M$, $N$, $K$, ... обозначают произвольные л-выражения из #L.
  - Внешние скобки опускаются: $(\\x(y z)) -> \\x (y z)$.
  - Многократная абстракция сокращается:
  $
    \\x_1 (\\x_2 (\\... (\\x_n M)...)) -> \\x_1,x_2,...,x_n.. M -> \\arrow(x).. M
  $
  - Многократная комбинация сокращается:
  $
    ((...((M_1 M_2)M_3)...)M_n)N -> M_1M_2...M_n N -> arrow(M)N
  $
  - $norm(M)$ обозначает количество символов в выражении $M$.
  - Комбинация берёт приоритет над абстракцией: $\\x.. y z = #l x.. (y z)$
]


