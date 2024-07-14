settings.outformat = "svg";

size(12cm);
fontsize(12pt);
defaultpen(.8pt);
draw((-6,0)--(2,0), arrow = Arrow(SimpleHead));
draw((0,-1)--(0,3), arrow = Arrow(SimpleHead));

draw((-5,0)--(-1,0)--(1,2), red+2pt);

dot((-1,0), L = Label("$-1$", align = 1.5*S));
// dot((1,0), L = Label("$1$", align = 1.5*S));
dot((0,1), L = Label("$1$", align = S+E));
