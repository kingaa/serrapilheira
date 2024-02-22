params <-
list(prefix = "odes")

library(tidyverse)
library(pomp)
theme_set(theme_bw())
set.seed(1173489184)

library(pomp)

closed.sir.ode <- Csnippet("
  DS = -Beta*S*I/N;
  DI = Beta*S*I/N-gamma*I;
  DR = gamma*I;
")

init1 <- Csnippet("
  S = N-1;
  I = 1;
  R = 0;
  ")

pomp(
  data=NULL,
  times=1:50, t0=0,
  skeleton=vectorfield(closed.sir.ode),
  rinit=init1,
  statenames=c("S","I","R"),
  paramnames=c("Beta","gamma","N")
) -> closed.sir

params1 <- c(Beta=1,gamma=1/13,N=763)

x <- trajectory(closed.sir,params=params1,format="data.frame")

library(ggplot2)
ggplot(data=x,mapping=aes(x=time,y=I))+geom_line()

expand_grid(
  Beta=c(0.05,1,2),
  gamma=1/c(1,2,4,8),
  N=763
) -> params2

closed.sir |>
  trajectory(params=t(params2),times=seq(0,50),format="d") -> x

x |>
  mutate(.id=as.integer(.id)) |>
  left_join(
    params2 |> mutate(.id=seq_along(Beta)),
    by=".id"
  ) |>
  ggplot(aes(x=time,y=I,group=.id,
    linetype=factor(Beta),color=factor(1/gamma)))+
  geom_line()+
  scale_y_log10(limits=c(1e-3,NA))+
  labs(
    x="time (da)",
    color=expression("IP"==1/gamma),
    linetype=expression(beta)
  )

f <- seq(0,1,length=100)
R0 <- -log(1-f)/f
plot(f~R0,type='l',xlab=expression(R[0]),ylab="fraction infected",bty='l')

open.sir.ode <- Csnippet("
  DS = -Beta*S*I/N+mu*(N-S);
  DI = Beta*S*I/N-gamma*I-mu*I;
  DR = gamma*I-mu*R;
")

init2 <- Csnippet("
  S = S_0;
  I = I_0;
  R = N-S_0-I_0;
")

pomp(
  data=NULL,
  times=seq(0,20,by=1/52),
  t0=-1/52,
  skeleton=vectorfield(open.sir.ode),
  rinit=init2,
  statenames=c("S","I","R"),
  paramnames=c("Beta","gamma","mu","S_0","I_0","N")
) -> open.sir

params3 <- c(
  mu=1/50,
  Beta=400,
  gamma=365/13,
  N=100000,
  S_0=100000/12,
  I_0=100
)

x <- trajectory(open.sir,params=params3,format="d")

ggplot(data=x,mapping=aes(x=time,y=I))+geom_line()
ggplot(data=x,mapping=aes(x=S,y=I))+geom_path()

seasonal.sir.ode <- Csnippet("
  double Beta = beta0*(1+beta1*cos(2*M_PI*t));
  DS = -Beta*S*I/N+mu*(N-S);
  DI = Beta*S*I/N-gamma*I-mu*I;
  DR = gamma*I-mu*R;
")

open.sir |>
  pomp(
    skeleton=vectorfield(seasonal.sir.ode),
    rinit=init2,
    statenames=c("S","I","R"),
    paramnames=c("beta0","beta1","gamma","mu","N","S_0","I_0")
  ) -> seas.sir

params3 <- c(mu=1/50,beta0=400,beta1=0.15,gamma=26,
             N=1e5,S_0=7000,I_0=50)

trajectory(seas.sir,params=params3,format="data.frame") -> x

ggplot(x,mapping=aes(x=time,y=I))+geom_path()
ggplot(x,mapping=aes(x=S,y=I))+geom_path()
