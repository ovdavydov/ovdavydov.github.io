function f=sigmoid(x,y)
%
%  function f=sigmoid(x,y)
%  evaluates the sigmoid function (see Utreras in Topics in
%  Multivariate Approx.
%
e=-27*sqrt(x.^2+y.^2)+22.1;
f=ones(x)./sqrt(1+2*exp(e));
