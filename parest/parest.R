params <-
list(prefix = "parest")

library(tidyverse)
library(pomp)
theme_set(theme_bw())
set.seed(1173439184)

niamey <- read.csv("http://kingaa.github.io/serrapilheira/parest/niamey.csv")
niamey |>
  ggplot(aes(x=biweek,y=measles,color=community))+
  geom_line()+
  geom_point()

niamey |>
  filter(community=="A") |>
  ggplot(aes(x=biweek,y=measles))+
  geom_point()+geom_line()+
  scale_y_log10()+
  labs(y="measles cases")

niamey |>
  filter(community=="A",biweek<=8) |>
  lm(log(measles)~biweek,data=_) -> fit1
summary(fit1)
coef(fit1)
slope <- coef(fit1)[2]; slope

coef(summary(fit1))
slope.se <- coef(summary(fit1))[2,2]
slope.se

fitfn <- function (interval) {
  niamey |>
    filter(community=="A",biweek<=interval) |>
    lm(log(measles)~biweek,data=_) -> fit
  slope <- coef(summary(fit))[2,1]
  slope.se <- coef(summary(fit))[2,2]
  c(interval=interval,R0.hat=slope*1+1,R0.se=slope.se)
}

lapply(2:14,fitfn) |>
  bind_rows() -> ests

ests |>
  ggplot(aes(x=interval,y=R0.hat,
    ymin=R0.hat-2*R0.se,
    ymax=R0.hat+2*R0.se))+
  geom_point()+geom_errorbar(width=0.2)+
  labs(x="length of initial phase",y=expression("estimated"~R[0]))



niamey |>
  filter(community=="A") |>
  select(-community) |>
  pomp(
    times="biweek",t0=0,
    skeleton=vectorfield(
      Csnippet("
      DS = -Beta*S*I/N;
      DI = Beta*S*I/N-gamma*I;
      DR = gamma*I;")),
    rinit=Csnippet("
      S = S_0;
      I = I_0;
      R = N-S_0-I_0;"),
    statenames=c("S","I","R"),
    paramnames=c("Beta","gamma","N","S_0","I_0")) -> niameyA

sse <- function (params) {
  niameyA |>
    trajectory(params=params) |>
    states("I")-obs(niameyA) -> discrep
  sum(discrep^2)
}

f1 <- function (beta) {
  params <- c(Beta=beta,gamma=1,N=50000,S_0=10000,I_0=10)
  sse(params)
}
beta <- seq(from=30,to=40,by=0.5)
SSE <- sapply(beta,f1)

beta.hat <- beta[which.min(SSE)]

plot(beta,SSE,type='l')
abline(v=beta.hat,lty=2)

coef(niameyA) <- c(Beta=beta.hat,gamma=1,N=50000,S_0=10000,I_0=10)
niameyA |>
  trajectory() |>
  as.data.frame() |>
  ggplot(aes(x=biweek))+
  geom_line(aes(y=measles),color='black')+
  geom_line(aes(y=I),color='red')

beta <- seq(from=0,to=40,by=0.5)
SSE <- sapply(beta,f1)

plot(beta,SSE,type='l')
beta.hat <- beta[which.min(SSE)]
abline(v=beta.hat,lty=2)

coef(niameyA,"Beta") <- beta.hat
niameyA |>
  trajectory() |>
  as.data.frame() |>
  ggplot(aes(x=biweek))+
  geom_line(aes(y=measles),color='black')+
  geom_line(aes(y=I),color='red')

expand_grid(
  Beta=seq(from=0,to=20,length=50),
  S_0=seq(from=4000,to=20000,length=50),
  N=50000,
  gamma=1,
  I_0=10
) -> grid

trajectory(niameyA,params=t(grid),format="pomps") |>
  as.data.frame() |>
  group_by(.L1) |>
  summarize(sse=sum((measles-I)^2)) |>
  ungroup() |>
  left_join(
    grid |> rownames_to_column(".L1"),
    by=".L1"
  ) -> grid

grid |>
  ggplot(aes(x=Beta,y=S_0,z=sqrt(sse),fill=sqrt(sse)))+
  geom_tile()+
  geom_contour(bins=30)+
  labs(
    fill=expression(sqrt(SSE)),
    x=expression(beta),
    y=expression(S(0))
  )

## ?optim

f2 <- function (par) {
  params <- c(Beta=par[3],gamma=1,N=50000,S_0=par[1],I_0=par[2])
  sse(params)
}
optim(fn=f2,par=c(10000,10,8)) -> fit2
fit2

niamey |>
  filter(community=="A") |>
  select(-community) |>
  pomp(
    times="biweek",t0=0,
    skeleton=vectorfield(
      Csnippet("
      double incidence;
      incidence = b*S*I;
      DS = -incidence;
      DI = incidence-gamma*I;")),
    rinit=Csnippet("
      S = S_0;
      I = I_0;"),
    paramnames=c("b","gamma","S_0","I_0"),
    statenames=c("S","I")
  ) -> niameyA2

loglik.normal <- function (params) {
  niameyA2 |>
    trajectory(params=params) |>
    states("I") -> x
  sum(dnorm(x=obs(niameyA2),mean=x,
    sd=params["sigma"],log=TRUE))
}

f3 <- function (b) {
  params <- c(S_0=10000,I_0=10,gamma=1,b=b,sigma=1)
  loglik.normal(params)
}

b <- seq(from=0,to=0.001,by=0.00002)
ll <- sapply(b,f3)

plot(b,ll,type='l',ylab=expression(log(L)))
b.hat <- b[which.max(ll)]
abline(v=b.hat,lty=2)

poisson.loglik <- function (params) {
  niameyA2 |>
    trajectory(params=params) |>
    states("I") -> x
  sum(dpois(x=obs(niameyA2),lambda=params["p"]*x,log=TRUE))
}

f4 <- function (par) {
  params <- c(S_0=20000,I_0=1,gamma=1,b=par,p=0.2)
  -poisson.loglik(params)
}

fit4 <- optim(f4,par=c(0.0003),method="Brent",
  lower=0,upper=1); fit4

expand_grid(
  b=seq(8e-5,1.2e-4,length=100)
) |>
  mutate(loglik=-sapply(b,f4)) -> prof.b

maxloglik <- -fit4$value
plot(loglik~b,data=prof.b,type="l")
abline(v=fit4$par)
abline(h=maxloglik)

expand_grid(
  b=seq(9.8e-5,1.01e-4,length=200)
) |>
  mutate(loglik=-sapply(b,f4)) -> prof.b
plot(loglik~b,data=prof.b,type="l",ylim=maxloglik+c(-10,0))
cutoff <- maxloglik-qchisq(p=0.95,df=1)/2
abline(h=c(0,cutoff))
abline(v=range(subset(prof.b,loglik>cutoff)$b),lty=2)

logit <- function (p) log(p/(1-p))    # the logit transform
expit <- function (x) 1/(1+exp(-x))   # inverse logit

f5 <- function (par) {
  params <- c(S_0=20000,I_0=1,gamma=1,b=exp(par[1]),p=expit(par[2]))
  -poisson.loglik(params)
}

fit5 <- optim(f5,par=c(log(0.0001),logit(0.2)))
fit5

mle1 <- c(b=exp(fit5$par[1]),p=expit(fit5$par[2]))
signif(mle1,3)

fitfn <- function (b) {
  fit <- optim(fn=function(p)f5(c(log(b),logit(p))),
    par=mle1[2],method="Brent",lower=0,upper=1)
  data.frame(b=b,p=expit(fit$par),loglik=-fit$value)
}

expand_grid(
  b=seq(9.6e-5,1.01e-4,length=100)
) |>
  rowwise() |>
  reframe(fitfn(b)) -> prof2.b
maxloglik <- max(prof2.b$loglik)
plot(loglik~b,data=prof2.b,type="l",ylim=maxloglik+c(-10,0))
cutoff <- maxloglik-qchisq(p=0.95,df=1)/2
abline(h=c(0,cutoff))
abline(v=range(subset(prof2.b,loglik>cutoff)$b),lty=2)

fitfn <- function (p) {
  fit <- optim(fn=function(b)f5(c(log(b),logit(p))),
    par=mle1[1],method="Brent",lower=9.5e-5,upper=1e-4)
  data.frame(p=p,b=expit(fit$par),loglik=-fit$value)
}

expand_grid(
  p=seq(0.35,0.42,length=100)
) |>
  rowwise() |>
  reframe(fitfn(p)) -> prof2.p
maxloglik <- max(prof2.p$loglik)
plot(loglik~p,data=prof2.p,type="l",ylim=maxloglik+c(-10,0))
cutoff <- maxloglik-qchisq(p=0.95,df=1)/2
abline(h=c(0,cutoff))
abline(v=range(subset(prof2.p,loglik>cutoff)$p),lty=2)





coef(niameyA2) <- c(S_0=20000,I_0=1,gamma=1,mle1)
niameyA2 |>
  trajectory() |>
  states("I") -> model.pred

rpois(n=length(model.pred),lambda=coef(niameyA2,"p")*model.pred) |>
  replicate(n=2000) -> simdat

simdat |>
  apply(1,quantile,probs=c(0.025,0.5,0.975)) -> quantiles

typ <- sample(ncol(simdat),1)

niameyA2 |>
  as.data.frame() |>
  bind_cols(t(quantiles)) |>
  bind_cols(typical=simdat[,typ]) |>
  ggplot(aes(x=biweek))+
  geom_line(aes(y=`50%`),color='red')+
  geom_ribbon(aes(ymin=`2.5%`,ymax=`97.5%`),fill='red',alpha=0.2)+
  geom_line(aes(y=measles),color='black')+
  geom_line(aes(y=typical),color='blue')+
  labs(y="cases",x="biweek")

negbin.loglik <- function (params) {
  niameyA2 |>
    trajectory(params=params) |>
    states("I") -> prediction
  sum(
    dnbinom(x=obs(niameyA2),
      mu=params["p"]*prediction,
      size=1/params["theta"],
      log=TRUE)
  )
}

f7 <- function (par) {
  params <- c(S_0=20000,I_0=1,gamma=1,
    b=exp(par[1]),p=expit(par[2]),theta=exp(par[3]))
  -negbin.loglik(params)
}

guess <- c(log(0.0001),logit(0.4),log(1))
fit7 <- optim(fn=f7,par=guess); fit7

mle3 <- c(b=exp(fit7$par[1]),p=expit(fit7$par[2]),theta=exp(fit7$par[3]))
signif(mle3,3)
