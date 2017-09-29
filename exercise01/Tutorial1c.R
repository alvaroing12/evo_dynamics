
## ------------------------------------------------------------------------
steps<-100
xs<-rep(0,steps)
xs[1]<-0.95 # initial condition
for(ii in 2:steps){
  xs[ii]<-5*xs[ii-1]^2*(1-xs[ii-1])
}

## ------------------------------------------------------------------------
par(mar=c(4,4,0.5,0.5))
par(mgp=c(2.5,1,0))
par(cex.lab=1.25)
plot(xs,xlab="step",ylab="x",main="",col="dodgerblue",pch=20,ylim=c(0,1))
abline(h=0, col="#ff8c00", lwd=2)

## ------------------------------------------------------------------------
xs[1]<-0.9 # initial condition
for(ii in 2:steps){
  xs[ii]<-5*xs[ii-1]^2*(1-xs[ii-1])
}

## ------------------------------------------------------------------------
plot(xs,xlab="step",ylab="x",main="",col="dodgerblue",pch=20,ylim=c(0,1))
abline(h=1/2+sqrt(5)/10, col="#ff8c00", lwd=2)

## ------------------------------------------------------------------------
xstm1<-xs[-length(xs)]
xst<-xs[-1]
plot(xstm1,xst,xlab=expression(x[t-1]),ylab=expression(x[t]),main="",col="dodgerblue",pch=20,xlim=c(0,1),ylim=c(0,1))
lines(xstm1,xst,col="dodgerblue",lwd=0.5)
abline(b=1,a=0, col="#ff8c00", lwd=2)

## ------------------------------------------------------------------------
library(deSolve)
parms<-c()
my.atol <- c(1e-6)
times<-c(0:100)/25
sdiffeqns <- function(t, s, parms)
{
  sd1 <- 3*s[1]*(s[1]-1)*(s[1]-2)
  list(c(sd1))
}

## ------------------------------------------------------------------------
initconds<-c(0-1e-6) # just below 0
out0m <- lsoda(initconds,times,sdiffeqns, rtol=1e-10, atol= my.atol)
initconds<-c(0+1e-6) # just above 0
out0p <- lsoda(initconds,times,sdiffeqns, rtol=1e-10, atol= my.atol)
initconds<-c(1-1e-6) # just below 1
out1m <- lsoda(initconds,times,sdiffeqns, rtol=1e-10, atol= my.atol)
initconds<-c(1+1e-6) # just above 1
out1p <- lsoda(initconds,times,sdiffeqns, rtol=1e-10, atol= my.atol)
initconds<-c(2-1e-6) # just below 2
out2m <- lsoda(initconds,times,sdiffeqns, rtol=1e-10, atol= my.atol)
initconds<-c(2+1e-6) # just above 2
out2p <- lsoda(initconds,times,sdiffeqns, rtol=1e-10, atol= my.atol)

## ------------------------------------------------------------------------
plot(out0p,xlab="time",ylab="x",main="",col="dodgerblue",lty=1,lwd=2,ylim=c(-2,4),xlim=c(0,4))
lines(out0m,col="#ff8c00",lty=3,lwd=3)
lines(out2m,col="dodgerblue",lty=1,lwd=2)
lines(out2p,col="#ff8c00",lty=3,lwd=3)
lines(out1m,col="#68228b",lty=2,lwd=2)
lines(out1p,col="#cd2626",lty=3,lwd=3)

