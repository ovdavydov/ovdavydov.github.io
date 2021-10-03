function z=kes2(xx,yy)
%  function z=kes2(xx,yy)
%
%  inspired by Kes
%
%  xx,yy - points in plane
%  z   - output value of function
%
[m,n]=size(xx);
N=m*n;
x=reshape(xx,1,m*n);
y=reshape(yy,1,m*n);
z=1+y-.3-3*x;
iz=find(z<0);
niz=length(iz);
z(iz)=zeros(1,niz);
iq=find(y-.3>3*x);
niq=length(iq);
z(iq)=ones(1,niq);
r=(3*x-y-2.2).^2+(x-y-.6).^2;
z=z+ones(1,N)./(1+80*r);
z=reshape(z,m,n);
