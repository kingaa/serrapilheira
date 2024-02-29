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

animator(plots=pl,gif_file="moran1.gif",webm=TRUE,mp4=TRUE,framerate=15)

if (rstudioapi::isAvailable()) {
  file.show("moran1.gif")
}

## \begin{tikzpicture}[scale=1]
##   \usetikzlibrary{arrows.meta,positioning,calc}
##   \definecolor{royalblue}{rgb}{0.263,0.431,0.933}
## 
##   \tikzstyle{box}=[draw=black, text=black, fill=white, very thick, minimum size=3em]
##   \tikzstyle{coordinate}=[inner sep=0pt,outer sep=0pt]
##   \tikzstyle{flow}=[draw=black, very thick, >=stealth]
##   \tikzstyle{obs}=[draw=royalblue, thick, >=Circle]
## 
##   \coordinate (origin) at (0,0);
##   \node [box] (N) at (origin) {$N$};
## 
##   \draw [flow,->] (N.south east) -- ($(N)+(1,-1)$) node[midway,above,sloped] {$\mu$};
##   \draw [flow,->] (N.north) .. controls ($(N)+(90:2)$) and ($(N.west)+(180:2)$) .. (N.west) node[midway,above,sloped] {$\lambda$};
##   \draw [obs,->] (N.east) -- ($(N)+(0:2)$) node[midway,above,sloped] {$\psi$};
## 
## \end{tikzpicture}

## \begin{tikzpicture}[scale=1.3]
##   \usetikzlibrary{arrows.meta,positioning,calc}
##   \definecolor{darkgreen}{rgb}{0,0.392,0}
##   \definecolor{royalblue}{rgb}{0.263,0.431,0.933}
## 
##   \tikzstyle{box}=[draw=black, text=black, fill=white, very thick, minimum size=3em]
##   \tikzstyle{coordinate}=[inner sep=0pt,outer sep=0pt]
##   \tikzstyle{flow}=[draw=black, very thick, >=stealth]
##   \tikzstyle{modulate}=[draw=darkgreen, >=Circle]
##   \tikzstyle{obs}=[draw=royalblue, thick, >=Circle]
## 
##   \coordinate (origin) at (0,0);
##   \node [box] (S) at ($(origin)+(1,-1)$) {$S$};
##   \node [box] (I) at ($(S)+(2,0)$) {$I$};
##   \node [box] (R) at ($(I)+(2,0)$) {$R$};
##   \coordinate (midSI) at ($(S)!0.5!(I)$);
##   \coordinate (midIR) at ($(I)!0.5!(R)$);
##   \node (C) at ($(midIR)+(0,-1)$) {$C$};
##   \draw [flow,->] (S) -- (I) node[midway,below,sloped] {$\beta\,I/N$};
##   \draw [flow,->] (I) -- (R) node[midway,above,sloped] {$\gamma$};
##   \draw [obs,->] (I.south) .. controls ($(I)+(0,-1)$) and ($(C)+(-1,0)$) .. (C)  node[midway,above,sloped] {$\psi$};
##   \draw [modulate,->] (I.north west) .. controls ($(I)+(-0.8,0.8)$) and ($(midSI)+(0,0.5)$) .. (midSI);
## 
## \end{tikzpicture}

## \begin{tikzpicture}[scale=1.3]
##   \usetikzlibrary{arrows.meta,positioning,calc}
##   \definecolor{darkgreen}{rgb}{0,0.392,0}
##   \definecolor{royalblue}{rgb}{0.263,0.431,0.933}
## 
##   \tikzstyle{box}=[draw=black, text=black, fill=white, very thick, minimum size=3em]
##   \tikzstyle{coordinate}=[inner sep=0pt,outer sep=0pt]
##   \tikzstyle{flow}=[draw=black, very thick, >=stealth]
##   \tikzstyle{modulate}=[draw=darkgreen, >=Circle]
##   \tikzstyle{obs}=[draw=royalblue, thick, >=Circle]
## 
##   \coordinate (origin) at (0,0);
##   \node [box] (S) at ($(origin)+(1,-1)$) {$S$};
##   \node [box] (I) at ($(S)+(2,0)$) {$I$};
##   \node [box] (R) at ($(I)+(2,0)$) {$R$};
##   \coordinate (overR) at ($(R)+(0,1)$);
##   \coordinate (overS) at ($(S)+(0,1)$);
##   \coordinate (midSI) at ($(S)!0.5!(I)$);
##   \coordinate (midIR) at ($(I)!0.5!(R)$);
##   \node (C) at ($(midIR)+(0,-1)$) {$C$};
##   \draw [flow,->] (S) -- (I) node[midway,below,sloped] {$\beta\,I/N$};
##   \draw [flow,->] (I) -- (R) node[midway,above,sloped] {$\gamma$};
##   \draw [flow,->] (R) -- (overR) -- (overS)  node[midway,above,sloped] {$\omega$} -- (S);
##   \draw [obs,->] (I.south) .. controls ($(I)+(0,-1)$) and ($(C)+(-1,0)$) .. (C)  node[midway,above,sloped] {$\psi$};
##   \draw [modulate,->] (I.north west) .. controls ($(I)+(-0.8,0.8)$) and ($(midSI)+(0,0.5)$) .. (midSI);
## 
## \end{tikzpicture}

## \begin{tikzpicture}[scale=1.3]
##   \usetikzlibrary{arrows.meta,positioning,calc}
##   \definecolor{darkgreen}{rgb}{0,0.392,0}
##   \definecolor{royalblue}{rgb}{0.263,0.431,0.933}
## 
##   \tikzstyle{box}=[draw=black, text=black, fill=white, very thick, minimum size=3em]
##   \tikzstyle{coordinate}=[inner sep=0pt,outer sep=0pt]
##   \tikzstyle{flow}=[draw=black, very thick, >=stealth]
##   \tikzstyle{modulate}=[draw=darkgreen, >=Circle]
##   \tikzstyle{obs}=[draw=royalblue, thick, >=Circle]
## 
##   \coordinate (origin) at (0,0);
##   \node [box] (S) at ($(origin)+(1,-1)$) {$S$};
##   \node [box] (E) at ($(S)+(2,0)$) {$E$};
##   \node [box] (I) at ($(E)+(2,0)$) {$I$};
##   \node [box] (R) at ($(I)+(2,0)$) {$R$};
##   \coordinate (overR) at ($(R)+(0,1)$);
##   \coordinate (overS) at ($(S)+(0,1)$);
##   \coordinate (midSE) at ($(S)!0.5!(E)$);
##   \coordinate (midIR) at ($(I)!0.5!(R)$);
##   \node (C) at ($(midIR)+(0,-1)$) {$C$};
##   \draw [flow,->] (S) -- (E) node[midway,below,sloped] {$\beta\,I/N$};
##   \draw [flow,->] (E) -- (I) node[midway,above,sloped] {$\sigma$};
##   \draw [flow,->] (I) -- (R) node[midway,above,sloped] {$\gamma$};
##   \draw [flow,->] (R) -- (overR) -- (overS)  node[midway,above,sloped] {$\omega$} -- (S);
##   \draw [obs,->] (I.south) .. controls ($(I)+(0,-1)$) and ($(C)+(-1,0)$) .. (C)  node[midway,above,sloped] {$\psi$};
##   \draw [modulate,->] (I.north west) .. controls ($(I)+(-0.8,0.8)$) and ($(midSE)+(0,0.8)$) .. (midSE);
## 
## \end{tikzpicture}
