if (file.exists("CLUSTER.R")) {
  source("CLUSTER.R")
} else {
  library(doFuture)
  plan(multisession)
}

set.seed(517218506L)
library(tidyverse)
library(phylopomp)
library(gifski)
theme_set(theme_bw())
dir.create("figs")



simulate("Moran",time=0,n=10,psi=0) -> m
m |> plot(prune=FALSE,points=TRUE)+expand_limits(x=5)
m |> simulate("Moran",time=1) -> m
m |> plot(prune=FALSE,points=TRUE)+expand_limits(x=5)
m |> simulate("Moran",time=2) -> m
m |> plot(prune=FALSE,points=TRUE)+expand_limits(x=5)
m |> simulate("Moran",time=3) -> m
m |> plot(prune=FALSE,points=TRUE)+expand_limits(x=5)
m |> simulate("Moran",time=4) -> m
m |> plot(prune=FALSE,points=TRUE)+expand_limits(x=5)
m |> simulate("Moran",time=5) -> m
m |> plot(prune=FALSE,points=TRUE)+expand_limits(x=5)

bake(
  seed=2147460505,
  file="moran1.rds",
  {
    simulate("Moran",t0=0,time=0,n=10,psi=0) -> m
    times <- seq(0,10,by=0.1)
    pl <- vector(mode="list",length=length(times))
    for (i in seq_along(times)) {
      m |> simulate("Moran",time=times[i]) -> m
      m |>
        plot(prune=FALSE,points=TRUE)+
        expand_limits(x=max(times)) -> pl[[i]]
    }
    pl
  }
) -> pl

source("https://kingaa.github.io/serrapilheira/phylo/animator.R")
animator(plots=pl,gif_file="figs/moran1.gif",webm=TRUE,mp4=TRUE)



bake(
  seed=239863857,
  file="lbdp1.rds",
  {
    simulate("LBDP",t0=0,time=0,n=1,lambda=1.8,mu=1,psi=0) -> bd
    times <- seq(0,10,by=0.1)
    pl <- vector(mode="list",length=length(times))
    for (i in seq_along(times)) {
      bd |> simulate("LBDP",time=times[i]) -> bd
      bd |>
        plot(prune=FALSE,points=TRUE)+
        expand_limits(x=max(times)) -> pl[[i]]
    }
    animator(plots=pl,gif_file="figs/lbdp1.gif",webm=TRUE,mp4=TRUE)
  }
)

bake(
  seed=1282680647,
  file="lbdp2.rds",
  {
    simulate("LBDP",t0=0,time=0,n=1,lambda=1.4,mu=1,psi=0.5) -> bd
    times <- seq(0,10,by=0.1)
    pl <- vector(mode="list",length=length(times))
    for (i in seq_along(times)) {
      bd |> simulate("LBDP",time=times[i]) -> bd
      bd |>
        plot(prune=FALSE,points=TRUE)+
        expand_limits(x=max(times)) -> pl[[i]]
    }
    animator(plots=pl,gif_file="figs/lbdp2.gif",webm=TRUE,mp4=TRUE)
  }
)

bake(
  seed=1282680647,
  file="lbdp3.rds",
  {
    simulate("LBDP",t0=0,time=0,n=1,lambda=1.4,mu=1,psi=0.5) -> bd
    times <- seq(0,10,by=0.1)
    pl <- vector(mode="list",length=length(times))
    for (i in seq_along(times)) {
      bd |> simulate("LBDP",time=times[i]) -> bd
      bd |>
        plot(prune=TRUE,points=TRUE)+
        expand_limits(x=max(times)) -> pl[[i]]
    }
    animator(plots=pl,gif_file="figs/lbdp3.gif",webm=TRUE,mp4=TRUE)
  }
)



bake(
  seed=1282680647,
  file="sir1.rds",
  {
    simulate(
      "SIR",t0=0,time=0,
      S0=200,I0=3,R0=0,
      Beta=2,gamma=1,psi=0
    ) -> sir
    times <- seq(0,10,by=0.1)
    pl <- vector(mode="list",length=length(times))
    for (i in seq_along(times)) {
      sir |> simulate("SIR",time=times[i]) -> sir
      sir |>
        plot(prune=FALSE,points=TRUE)+
        expand_limits(x=max(times)) -> pl[[i]]
    }
    animator(plots=pl,gif_file="figs/sir1.gif",webm=TRUE,mp4=TRUE)
  }
)

bake(
  seed=1282680647,
  file="sir2.rds",
  {
    simulate(
      "SIR",t0=0,time=0,
      S0=200,I0=3,R0=0,
      Beta=2,gamma=1,psi=0.2
    ) -> sir
    times <- seq(0,10,by=0.1)
    pl <- vector(mode="list",length=length(times))
    for (i in seq_along(times)) {
      sir |> simulate("SIR",time=times[i]) -> sir
      sir |>
        plot(prune=FALSE,points=TRUE)+
        expand_limits(x=max(times)) -> pl[[i]]
    }
    animator(plots=pl,gif_file="figs/sir2.gif",webm=TRUE,mp4=TRUE)
  }
)

bake(
  seed=1282680647,
  file="sir3.rds",
  {
    simulate(
      "SIR",t0=0,time=0,
      S0=300,I0=3,R0=0,
      Beta=2,gamma=1,psi=0.2
    ) -> sir
    times <- seq(0,12,by=0.1)
    pl <- vector(mode="list",length=length(times))
    for (i in seq_along(times)) {
      sir |> simulate("SIR",time=times[i]) -> sir
      sir |>
        plot(prune=TRUE,points=TRUE)+
        expand_limits(x=max(times)) -> pl[[i]]
    }
    animator(plots=pl,gif_file="figs/sir3.gif",webm=TRUE,mp4=TRUE)
  }
)
