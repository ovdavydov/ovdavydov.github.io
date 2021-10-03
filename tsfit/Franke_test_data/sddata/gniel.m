function f = gniel(x,y)
%
%  function f = gniel(x,y)
%
%  Evaluates Greg Nielson' curved valleys.  IJNME 15(1978)308
%
f=.5*y.*cos(4*(x.*x+y-1)).^4;
