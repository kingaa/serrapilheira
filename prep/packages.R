## check to see that the version of R is sufficiently recent
minRversion <- "4.3.2"
rv <- getRversion()
if (rv < minRversion)
  stop("R version >= ",minRversion," is required",call.=FALSE)

lib <- Sys.getenv("R_LIBS_USER")

inst_pkg <- function (pkglist, lib = Sys.getenv("R_LIBS_USER")) {
  op <- options(warn=2)

  pkglist <- setdiff(pkglist,rownames(installed.packages()))

  if (length(pkglist)>0) {
    cat("trying to install packages in user directory...\n")
    dir.create(lib,recursive=TRUE,showWarnings=FALSE)
    res <- try(
      install.packages(
        pkglist,
        lib=lib,
        repos=c(
          CRAN="https://cloud.r-project.org",
          kingaa="https://kingaa.github.io"
        )
      )
    )
    if (inherits(res,"try-error")) {
      stop("cannot install to ",lib,call.=FALSE)
    }
  }

  options(op)
  invisible(NULL)
}

inst_pkg("BiocManager")
BiocManager::install("ggtree")

## get list of packages to install
pkglist <- scan(
  what=character(0),
  text="
bbmle
broom
coda
colorspace
cowplot
deSolve
foreach
gifski
iterators
doFuture
doRNG
gridExtra
gtable
knitr
mvtnorm
nloptr
scales
stringi
subplex
tidyverse
pomp
circumstance
phylopomp
"
)

inst_pkg(pkglist,lib=lib)
cat("first set of packages installed successfully to user directory\n\t(",lib,")!\n")
