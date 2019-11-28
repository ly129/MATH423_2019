## ---- life-cycle-data ----

LifeCycleSavings

str(LifeCycleSavings)

fit1 <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings)
summary(fit1)

par(mfrow=c(2,2))
plot(fit1)

## ---- diagonals-of-hat-matrix ----
inf.diags<-lm.influence(fit1)
data.frame(hat=inf.diags$hat[c(1:7,42:50)])

## ---- deletion-change-in-beta ----
data.frame(signif(inf.diags$coef[c(1:7,42:50),],4))

## ---- influence ----
influence.measures(fit1)


## ---- qqplot ----
#Q-Q n=50

set.seed(54)
n<-50
X<-rnorm(n,5,2)

xvec.qq<-qnorm((c(1:n)-0.5)/n)
yvec.qq<-sort(X)
plot(xvec.qq,yvec.qq,pch=19,cex=0.5,xlim=range(-3,3),ylim=range(0,10),
xlab='Theoretical quantiles',ylab='Sample quantiles')
abline(5,2,col='red',lwd=2);title("Q-Q plot (n=50)")
legend(-3,10,c(expression(mu+sigma*x)),lwd=2,col='red')
 
 
#Q-Q n=500
set.seed(54)
n<-500
X<-rnorm(n,5,2)

xvec.qq<-qnorm((c(1:n)-0.5)/n)
yvec.qq<-sort(X)
plot(xvec.qq,yvec.qq,pch=19,cex=0.5,xlim=range(-3,3),ylim=range(0,10),
xlab='Theoretical quantiles',ylab='Sample quantiles')
abline(5,2,col='red',lwd=2);title("Q-Q plot (n=500)")
legend(-3,10,c(expression(mu+sigma*x)),lwd=2,col='red')


## ---- qqplot-with-CI ----

nreps<-5000
set.seed(2332)

qq.vals50<-matrix(0,nrow=nreps,ncol=50)
qq.vals500<-matrix(0,nrow=nreps,ncol=500)

for(irep in 1:nreps){

	n<-50
	X<-rnorm(n,5,2)

	qq.vals50[irep,]<-sort(X)
	
	n<-500
	X<-rnorm(n,5,2)
	qq.vals500[irep,]<-sort(X)

}

n<-50
xvec.qq.50<-qnorm((c(1:n)-0.5)/n)
n<-500
xvec.qq.500<-qnorm((c(1:n)-0.5)/n)


qq.ci.50<-apply(qq.vals50,2,quantile,probs=c(0.025,0.975))
qq.ci.500<-apply(qq.vals500,2,quantile,probs=c(0.025,0.975))

#Q-Q
xvec.qq<-qnorm((c(1:n)-0.5)/n)
yvec.qq<-sort(X)
plot(xvec.qq.500,qq.ci.500[1,],ylim=range(0,10),xlim=range(-3,3),
     xlab='Theoretical quantiles',ylab='Sample quantiles',type='l',lty=2)
lines(xvec.qq.500,qq.ci.500[2,],lty=2)
points(xvec.qq,yvec.qq,pch=19,cex=0.5)
abline(5,2,col='red',lwd=2);title("Q-Q plot (n=50)")
legend(-3,10,c(expression(mu+sigma*x)),lwd=2,col='red')
 
#Q-Q
xvec.qq<-qnorm((c(1:n)-0.5)/n)
yvec.qq<-sort(X)
plot(xvec.qq.50,qq.ci.50[1,],ylim=range(0,10),xlim=range(-3,3),
     xlab='Theoretical quantiles',ylab='Sample quantiles',type='l',lty=2)
lines(xvec.qq.50,qq.ci.50[2,],lty=2)
points(xvec.qq,yvec.qq,pch=19,cex=0.5)
abline(5,2,col='red',lwd=2);title("Q-Q plot (n=500)")
legend(-3,10,c(expression(mu+sigma*x)),lwd=2,col='red')
 


