function f=f1(x,y)
%  Evaluates the Franke famous exponential
%  02/07/92
%
arg=((9*x-2).^2+(9*y-2).^2)/4; f=exp(-arg)*.75;
arg=(9*x+1).^2/49+(9*y+1)/10;f=f+exp(-arg)*.75;
arg=((9*x-7).^2+(9*y-3).^2)/4;f=f+.5*exp(-arg);
arg=(9*x-4).^2+(9*y-7).^2;f=f-.2*exp(-arg);
