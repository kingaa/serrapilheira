library(tidyverse)
library(ggtree)
library(pomp)
library(cowplot)
library(viridis)
library(phylopomp)
stopifnot(getRversion() >= "4.3")
stopifnot(packageVersion("pomp")>="5.5")
stopifnot(packageVersion("phylopomp")>="0.10.3")
theme_set(theme_bw(base_family="serif"))
options(
  pomp_archive_dir="results",
  dplyr.summarise.inform=FALSE
)
set.seed(1159254136)

library(doFuture)
plan(multisession)
set.seed(2488820)

read_csv("data/covid-variants.csv",comment="#") |>
  rename(date=Day) |>
  filter(
    Code=="USA"|Code=="CAN",
    date < "2023-12-18"
  ) |>
  select(-Entity,-Code) |>
  rename(Other=non_who) |>
  pivot_longer(
    -date,
    names_to="variant",
    values_to="count"
  ) -> dat

stopifnot(
  dat |>
    count(date) |>
    filter(n!=40) |>
    nrow()==0,
  dat |>
    count(date) |>
    mutate(
      date=as_date(date),
      pd=date-lag(date)
    ) |>
    filter(!is.na(pd)) |>
    count(pd) |>
    nrow()==1
)

dat |>
  filter(variant!="Recombinant") |>
  mutate(
    w=count*as.double(date-as_date("2020-01-01"))
  ) |>
  group_by(variant) |>
  summarize(
    mean_time=sum(w)/sum(count)
  ) |>
  ungroup() |>
  arrange(mean_time) |>
  pull(variant) -> varts

dat |>
  filter(!variant%in%c("Recombinant")) |>
  group_by(date,variant) |>
  reframe(count=sum(count)) |>
  ungroup() |>
  group_by(date) |>
  mutate(
    variant=factor(variant,levels=varts),
    tot=sum(count),
    frequency=coalesce(count/sum(count),0),
    frequency=if_else(tot<100,NA_real_,frequency)
  ) |>
  ungroup() -> dat1

dat1 |>
  ggplot(aes(x=date,group=variant,fill=variant))+
  scale_fill_viridis_d(option="E")+
  lims(x=c(as_date("2021-01-01"),NA))+
  theme_bw()+
  theme(legend.background=element_rect(color=NA)) -> pl

pl+geom_area(aes(y=count))+
  labs(
    title="COVID-19 sequenced cases by variant",
    subtitle="US + Canada, biweekly"
  ) -> pl1
pl+geom_area(aes(y=frequency)) -> pl2

plot_grid(
  plot_grid(
    pl1+guides(fill="none")+labs(x=""),
    pl2+guides(fill="none"),
    align="v",axis="b",
    ncol=1,
    rel_heights=c(6,5)
  ),
  get_legend(pl1),
  rel_widths=c(11,2),
  nrow=1
)

freeze(
  seed=522390503,
  simulate(
    "SEIR",
    Beta=1,sigma=0.5,gamma=0.1,psi=0.4,omega=0.1,
    S0=10,E0=1,I0=1,R0=0,
    time=10
  )
) -> x

pal <- c("#00274CFF","#FFCB05FF")

x |>
  plot(
    points=TRUE,
    prune=TRUE,
    obscure=TRUE,
    palette="#B3B3B3FF"
  )+
  labs(x="time")

x |>
  plot(
    points=TRUE,
    prune=TRUE,
    obscure=FALSE,
    palette=pal
  )+
  labs(x="time")

x |>
  plot(
    points=TRUE,
    prune=FALSE,
    obscure=FALSE,
    palette=pal
  )+
  labs(x="time")

freeze(
  seed=522390503,
  simulate(
    "SEIR",
    Beta=1,sigma=0.5,gamma=0.1,psi=0.4,omega=0.1,
    S0=10,E0=1,I0=1,R0=0,
    time=10
  )
) -> x

pal <- c("#00274CFF","#FFCB05FF")

x |>
  plot(points=TRUE,prune=FALSE,obscure=FALSE,palette=pal)+
  geom_vline(xintercept=10,linewidth=0.2,color="black")+
  labs(x="time")

x |>
  plot(points=TRUE,prune=TRUE,obscure=FALSE,palette=pal)+
  geom_vline(xintercept=10,linewidth=0.2,color="black")+
  labs(x="time")

x |>
  plot(points=TRUE,prune=TRUE,obscure=FALSE,palette=pal)+
  geom_vline(xintercept=10,linewidth=0.2,color="black")+
  labs(x="time")

x |>
  plot(points=TRUE,prune=TRUE,obscure=TRUE,palette="#B3B3B3FF")+
  geom_vline(xintercept=10,linewidth=0.2,color="black")+
  labs(x="time")

x |>
  plot(points=TRUE,prune=TRUE,obscure=FALSE,palette=pal)+
  geom_vline(xintercept=10,linewidth=0.2,color="black")+
  labs(x="time")

x |>
  plot(points=TRUE,prune=TRUE,obscure=TRUE,palette="#B3B3B3FF")+
  geom_vline(xintercept=10,linewidth=0.2,color="black")+
  labs(x="time")

x |>
  plot(points=TRUE,prune=TRUE,obscure=TRUE,palette="#B3B3B3FF")+
  geom_vline(xintercept=10,linewidth=0.2,color="black")+
  labs(x="time")

bake(
  file="lbdp1.rds",
  seed=728604304L,
  {
    library(phylopomp)
    runLBDP(lambda=1.5,mu=0.8,psi=1,time=5,n0=1)
  }
) -> x

bake(
  file="lbdp2.rds",
  seed=2119434716L,
  {
    expand_grid(
      rep=1:16,
      lambda=seq(0.7,2.5,by=0.1),
      mu=0.8,
      psi=1,
      times=5,
      n0=1,
      Np=c(1000,10000)
    ) -> params

    library(iterators)
    library(doFuture)
    plan(multicore)

    foreach (
      p=iter(params,"row"),
      .inorder=FALSE,
      .combine=bind_rows,
      .options.future=list(seed=TRUE)
    ) %dofuture% {
      library(pomp)
      library(phylopomp)
      p |> with({
        x |>
          lbdp_exact(lambda=lambda,mu=mu,psi=psi)
      }) -> ll1
      p |> with({
        x |>
          lbdp_pomp(lambda=lambda,mu=mu,psi=psi,n0=n0) |>
          pfilter(Np=Np) |>
          logLik()
      }) -> ll2
      bind_cols(p,exact=ll1,pf=ll2)
    }
  }
) -> params

plot_grid(
  A=x |> plot(points=TRUE),
  B=params |>
    pivot_longer(c(exact,pf)) |>
    unite(name,name,Np) |>
    mutate(
      name=if_else(grepl("exact",name),"exact",name),
      name=gsub("pf_","",name)
    ) |>
    group_by(lambda,mu,psi,times,n0,name) |>
    reframe(
      type=c("logLik","logLik_se"),
      value=logmeanexp(value,se=TRUE)
    ) |>
    ungroup() |>
    pivot_wider(names_from=type) |>
    mutate(
      y=logLik,
      ymax=logLik+2*logLik_se,
      ymin=logLik-2*logLik_se
    ) |>
    filter(logLik>max(logLik)-16) |>
    ggplot(aes(x=lambda,group=name,color=name,
      y=y,ymin=ymin,ymax=ymax))+
    geom_errorbar(
      position="dodge"
    )+
    scale_color_manual(
      labels=c(
        exact="exact",
        `1000`="1000 particles",
        `10000`="10000 particles"
      ),
      values=c(
        exact="black",
        `1000`="blue",
        `10000`="red"
      )
    )+
    labs(
      color=character(0),
      y="log likelihood",
      x=expression(lambda)
    )+
    theme_classic()+
    theme(
      legend.position=c(0.5,0.27)
    ),
  labels="AUTO",
  nrow=1,
  rel_widths=c(1,1)
)

bake(
  file="sirs1.rds",
  seed=328168304L,
  {
    library(phylopomp)
    runSIRS(
      Beta=4,gamma=2,psi=1,omega=1,
      S0=97,I0=3,R0=0,t0=0,time=40
    )
  }
) -> x

bake(
  file="sirs2.rds",
  seed=621400057L,
  {
    expand_grid(
      Beta=4,
      gamma=2,
      psi=1,
      omega=seq(0.5,2,by=0.05),
      S0=97,I0=3,R0=0,
      t0=0,
      rep=1:16,
      Np=5000,
      ) -> params

    library(iterators)
    library(doFuture)
    plan(multicore)

    foreach (
      p=iter(params,"row"),
      .inorder=FALSE,
      .combine=bind_rows,
      .options.future=list(seed=TRUE)
    ) %dofuture% {
      library(pomp)
      library(phylopomp)
      p |> with({
        x |>
          sirs_pomp(
            Beta=Beta,gamma=gamma,psi=psi,omega=omega,
            S0=S0,I0=I0,R0=R0,t0=0
          ) |>
          pfilter(Np=Np) |>
          logLik()
      }) -> ll
      bind_cols(p,logLik=ll)
    } -> params
  }
) -> params

plot_grid(
  A=x |> plot(points=FALSE),
  B=params |>
    group_by(Beta,gamma,psi,omega,S0,I0,R0,Np) |>
    reframe(
      name=c("logLik","logLik_se"),
      value=logmeanexp(logLik,se=TRUE)
    ) |>
    ungroup() |>
    pivot_wider() |>
    mutate(
      y=logLik,
      ymax=logLik+2*logLik_se,
      ymin=logLik-2*logLik_se
    ) |>
    filter(round(Beta,2)==4) |>
    filter(logLik>max(logLik)-16) |>
    ggplot(
      aes(
        x=omega,group=Np,
        y=y,ymin=ymin,ymax=ymax
      )
    )+
    geom_errorbar(
      position="dodge"
    )+
    geom_vline(xintercept=1,color="red")+
    labs(
      color=character(0),
      y="log likelihood",
      x=expression(omega)
    )+
    theme_classic()+
    theme(
      legend.position=c(0.5,0.27)
    ),
  labels="AUTO",
  nrow=1,
  rel_widths=c(1,1)
)

freeze(
  seed=751601556L,
  runSEIR(
    Beta=4,sigma=1,gamma=1,psi=1,omega=1,
    S0=200,E0=3,I0=5,R0=100,
    time=3
  )
) -> G

bake(
  file="seirs1a.rds",
  seed=831282841L,
  {
    library(phylopomp)
    library(circumstance)
    library(doFuture)
    plan(multicore)

    G |>
      seirs_pomp(
        Beta=4,sigma=1,gamma=1,psi=1,omega=1,
        S0=200,E0=3,I0=5,R0=100
      ) -> po

    seq(0.2,2.5,by=0.1) |>
      lapply(
        \(s) {
          x <- po
          coef(x,"sigma") <- s
          x
        }
      ) |>
      concat() |>
      pfilter(Np=20000,Nrep=10)
  }
) -> pfs

left_join(
  coef(pfs) |> melt() |> pivot_wider(),
  pfs |> logLik() |> melt(),
  by=c(".id"="name")
) |>
  rename(logLik=value) -> res
plot_grid(
  A=G |> plot(points=TRUE),
  B=res |>
    filter(is.finite(logLik)) |>
    ggplot(aes(x=sigma,y=logLik))+
    geom_point()+
    geom_smooth()+
    geom_vline(xintercept=1,color="red")+
    lims(y=c(max(res$logLik)-10,NA))+
    labs(
      color=character(0),
      y="log likelihood",
      x=expression(sigma)
    )+
    theme_classic(),
  labels="AUTO",
  nrow=1,
  rel_widths=c(1,1)
)
