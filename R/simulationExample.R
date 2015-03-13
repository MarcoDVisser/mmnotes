 set.seed(100)

## groups
N=20
Ncols <- rainbow(20)

## Unrelated on the group level
GroupX <- sort(runif(20,0,4))
GroupY <- (runif(20))

## 5 highly numerous groups and 15 sparse

Nnum <- 2000
Nspa <- 10
groups <- c(rep(1:5,each=Nnum),rep(6:20,each=Nspa))
dat <- data.frame(groups,X=numeric(length(groups)),
                  Y=numeric(length(groups)),col=numeric(length(groups)))
         
for(i in 1:5){
 dat$X[groups==i] <- rnorm(Nnum,GroupX[i],0.04)
 dat$Y[groups==i] <- rnorm(1,0,1)+dat$X[groups==i]*rnorm(1,.25,0.01)
# dat$Y[groups==i] <- dat$Y[groups==i] - GroupY[i]
 dat$col[groups==i] <- Ncols[i]
}

for(i in 6:20){
dat$X[groups==i] <- rnorm(Nspa,GroupX[i],0.04)
dat$Y[groups==i] <- rnorm(1,0,1)+dat$X[groups==i]*rnorm(1,.25,0.01)
#dat$Y[groups==i] <- dat$Y[groups==i] - GroupY[i]
 dat$col[groups==i] <- Ncols[i]
}


require(gam)
require(lme4)


M0 <- gam(Y ~ lo(X, span = 1.3),data=dat)
M1 <- lmer(Y~X+(1+X|groups),data=dat)

ylimz <- range(dat$Y,na.rm=T)
xlimz <- range(dat$X,na.rm=T)

 par(mfrow=c(1,2),mar=c(4,4,1,1))

plot(tapply(dat$X,dat$group,mean),tapply(dat$Y,dat$group,mean)
     ,xlab="group mean X",ylab="group mean Y", main="A")

text(1,0.05,
  paste("r =", round(
                 cor(tapply(dat$X,dat$group,mean),tapply(dat$Y,dat$group,mean)),3)),
     cex=1.4)

plot(dat$X,dat$Y,pch="*",col=dat$col,xlab="X",ylab="Y",main="B")

lines(seq(xlimz[1],xlimz[2],length.out=100),
        predict(M0,newdata=data.frame(X=
        seq(xlimz[1],xlimz[2],length.out=100))),
      col="green",lwd=4, lty=2)

lines(seq(xlimz[1],xlimz[2],length.out=100),
        predict(M1,newdata=data.frame(X=
        seq(xlimz[1],xlimz[2],length.out=100)),re.form=~0),
      col="red",lwd=4, lty=2)
lines(seq(xlimz[1],xlimz[2],length.out=100),
        predict(M1,newdata=data.frame(X=
        seq(xlimz[1],xlimz[2],length.out=100)),re.form=~0),
      col="red",lwd=4, lty=2)

abline(lm(Y~X,data=dat),col="blue",lwd=2,lty=3)

curve(0+0.25*x,lty=1,lwd=2,col=rgb(0.1,.1,.1,alpha=0.4),add=T)


legend("bottomright",legend=
       c("Truth or Generating model",
         "GAM prediction",
         "Classical regression",
         "Multi-level model (lme4)"),
       lty=c(1,2,3,2),
       col=c("grey","green","blue","red"))
         
