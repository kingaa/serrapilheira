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

simulate("Moran",t0=0,time=0,n=10,psi=0) -> m
times <- seq(0,10,by=0.1)
pl <- vector(mode="list",length=length(times))
for (i in seq_along(times)) {
  m |> simulate("Moran",time=times[i]) -> m
  m |>
    plot(prune=FALSE,points=TRUE)+
    expand_limits(x=10) -> pl[[i]]
}

source("https://kingaa.github.io/serrapilheira/phylo/animator.R")

animator(plots=pl,gif_file="moran1.gif",webm=TRUE,framerate=15)

if (rstudioapi::isAvailable()) {
  file.show("moran1.gif")
}
