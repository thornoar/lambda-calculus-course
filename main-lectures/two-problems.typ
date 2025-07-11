#import "template.lib.typ": *
#import "@local/common:0.0.0": *
#show: formatting
#set page(background: image(
  "pictures/troubles-normal.jpg",
  width: 100%,
  height: 65%,
  fit: "stretch"
))
#set enum(numbering: n => [ #n. ])

#head([ Две задачи ])

+ Рассмотрим последовательность $a_n == #K^n #I$. Покажите, что $a$ --- *не* числовая система.

+ Пусть $M_1 == (@x.. b x (b c))c$, $M_2 == (@x.. x x)(b c)$. Докажите, что *не* существует такого выражения $M$, что $M ->> M_1$ и $M ->> M_2$.
