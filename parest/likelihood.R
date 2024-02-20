params <-
list(prefix = "lik")

library(tidyverse)
options(stringsAsFactors=FALSE)
theme_set(theme_bw())
set.seed(1173489184)

p <- 0.3
n <- 50
k <- seq(0,50,by=1)
prob <- dbinom(x=k,size=n,prob=p) 
plot(k,prob,type='h',lwd=5,lend=1,
     ylab="probability")

k1 <- 18
n1 <- 50
p <- seq(0,1,by=0.001)
plot(p,dbinom(x=k1,size=n1,prob=p,log=TRUE),
     ylim=c(-10,-2),ylab="log-likelihood",
     type='l')
abline(v=k1/n1,col='blue')

k2 <- 243
n2 <- 782
p <- seq(0,1,by=0.001)
plot(p,dbinom(x=k2,size=n2,prob=p,log=TRUE),
     ylim=c(-10,-2),ylab="log-likelihood",
     type='l')
abline(v=k2/n2,col='blue')

n <- c(13,484,3200)
k <- c(4,217,1118)
dbinom(x=k,size=n,prob=0.2,log=TRUE)
sum(dbinom(x=k,size=n,prob=0.2,log=TRUE))
ll.fn <- function (p) {
  sum(dbinom(x=k,size=n,prob=p,log=TRUE))
}
p <- seq(0,1,by=0.001)
loglik <- sapply(p,ll.fn)
plot(p,loglik,type='l',ylim=max(loglik)+c(-10,0))
