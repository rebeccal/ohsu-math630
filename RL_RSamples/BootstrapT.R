n <- 20
r <- 1000
B <- 1000
true.mean <- 3
simple.contains <- numeric(r)
simple.ci <- matrix(NA,nrow=r,ncol=2)
for (i in 1:r) {
  x <- rexp(n,rate=1/true.mean)
  s <- numeric(B)
  for (j in 1:B) {
    boot <- sample(n,replace=TRUE)
    s[j] <- mean(x[boot])
  }
  # this is equivalent to using the 'boot' package:
  #simple.boot <- boot(x,R=B,function(x,i) mean(x[i]))
  #s <- simple.boot$t
  simple.ci[i,] <- quantile(s,c(.025,.975))
}
simple.contains <- (true.mean > simple.ci[,1] & true.mean < simple.ci[,2])
sum(simple.contains)/r
mean(simple.ci[,2] - simple.ci[,1])

plot(0,0,type="n",xlim=c(0,8),ylim=c(0,r),xlab="",ylab="",main="bootstrap conf intervals")
segments(simple.ci[,1],1:r,simple.ci[,2],1:r,col=ifelse(simple.contains,"black","red"))
abline(v=true.mean,col="blue")

pivot.contains <- numeric(r)
pivot.ci <- matrix(NA,nrow=r,ncol=2)
for (i in 1:r) {
  x <- rexp(n,rate=1/true.mean)
  mean.x <- mean(x)
  sd.x <- sd(x)
  z <- numeric(B)
  for (j in 1:B) {
    boot <- sample(n,replace=TRUE)
    z[j] <- (mean(x[boot]) - mean.x)/sd(x[boot])
  }
  pivot.ci[i,] <- mean.x - sd.x*quantile(z,c(.975,.025))
}
pivot.contains <- (true.mean > pivot.ci[,1] & true.mean < pivot.ci[,2])
sum(pivot.contains)/r
mean(pivot.ci[,2] - pivot.ci[,1])

plot(0,0,type="n",xlim=c(0,8),ylim=c(0,r),xlab="",ylab="",main="bootstrap conf intervals")
segments(pivot.ci[,1],1:r,pivot.ci[,2],1:r,col=ifelse(pivot.contains,"black","red"))
abline(v=true.mean,col="blue")