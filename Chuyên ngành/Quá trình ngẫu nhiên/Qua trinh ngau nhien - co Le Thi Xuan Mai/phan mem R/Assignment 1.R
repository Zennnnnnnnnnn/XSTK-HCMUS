#--------------- Ex1 -----------------
par(mfrow=c(1,1))
n=10
p=0.3
X=rbinom(n,1,p);X
plot(X,xlab="n",ylab="X")




















#--------------- Ex2 -----------------
z=c(1,-1)
p=0.3
n=10
Z=c(0,sample(z,n,replace=T,prob=c(p,1-p)));Z
Xn=cumsum(Z)
par(mfrow=c(2,1))
plot(0:10,Xn,xlab="n",ylab="Xn",xlim=c(0,10),pch=16)
plot(stepfun(1:n,Xn),do.points = F,xlab="t",ylab="X(t)",xlim=c(0,10),main="")












#--------------- Ex3 -----------------
par(mfrow=c(1,1))
t=seq(0,30,0.1)
Y=runif(1,0,1);Y
omega=0.5
Xt=Y*cos(omega*t)
plot(t,Xt,type="l")











