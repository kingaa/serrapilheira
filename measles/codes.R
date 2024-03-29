options(
  keep.source=TRUE,
  stringsAsFactors=FALSE,
  encoding="UTF-8"
)

## ----prelims,include=FALSE,cache=FALSE-----------------------------------
stopifnot(getRversion()>="4.1")
stopifnot(packageVersion("pomp")>="4.6")

set.seed(594709947L)
library(tidyverse)
library(pomp)

## ----rproc-------------------------------------------------
rproc <- Csnippet("
  double beta, br, seas, foi, dw, births;
  double rate[6], trans[6];

  // cohort effect
  if (fabs(t-floor(t)-251.0/365.0) < 0.5*dt)
    br = cohort*birthrate/dt + (1-cohort)*birthrate;
  else
    br = (1.0-cohort)*birthrate;

  // term-time seasonality
  t = (t-floor(t))*365.25;
  if ((t>=7 && t<=100) ||
      (t>=115 && t<=199) ||
      (t>=252 && t<=300) ||
      (t>=308 && t<=356))
      seas = 1.0+amplitude*0.2411/0.7589;
  else
      seas = 1.0-amplitude;

  // transmission rate
  beta = R0*(gamma+mu)*seas;

  // expected force of infection
  foi = beta*pow(I+iota,alpha)/pop;

  // white noise (extrademographic stochasticity)
  dw = rgammawn(sigmaSE,dt);

  rate[0] = foi*dw/dt;  // stochastic force of infection
  rate[1] = mu;         // natural S death
  rate[2] = sigma;      // rate of ending of latent stage
  rate[3] = mu;         // natural E death
  rate[4] = gamma;      // recovery
  rate[5] = mu;         // natural I death

  // Poisson births
  births = rpois(br*dt);

  // transitions between classes
  reulermultinom(2, S, &rate[0], dt, &trans[0]);
  reulermultinom(2, E, &rate[2], dt, &trans[2]);
  reulermultinom(2, I, &rate[4], dt, &trans[4]);

  S += births   - trans[0] - trans[1];
  E += trans[0] - trans[2] - trans[3];
  I += trans[2] - trans[4] - trans[5];
  R = pop - S - E - I;
  W += (dw - dt)/sigmaSE;  // standardized i.i.d. white noise
  C += trans[4];           // true incidence
")

## ----rinit-------------------------------------------------
rinit <- Csnippet("
  double m = pop/(S_0+E_0+I_0+R_0);
  S = nearbyint(m*S_0);
  E = nearbyint(m*E_0);
  I = nearbyint(m*I_0);
  R = nearbyint(m*R_0);
  W = 0;
  C = 0;
")

## ----dmeasure-------------------------------------------------
dmeas <- Csnippet("
  double m = rho*C;
  double v = m*(1.0-rho+psi*psi*m);
  double tol = 0.0;
  if (cases > 0.0) {
    lik = pnorm(cases+0.5,m,sqrt(v)+tol,1,0)
           - pnorm(cases-0.5,m,sqrt(v)+tol,1,0) + tol;
  } else {
    lik = pnorm(cases+0.5,m,sqrt(v)+tol,1,0) + tol;
  }
  if (give_log) lik = log(lik);
")

## ----rmeasure-------------------------------------------------
rmeas <- Csnippet("
  double m = rho*C;
  double v = m*(1.0-rho+psi*psi*m);
  double tol = 0.0;
  cases = rnorm(m,sqrt(v)+tol);
  if (cases > 0.0) {
    cases = nearbyint(cases);
  } else {
    cases = 0.0;
  }
")

## ----load-data-------------------------------------------------
daturl <- "https://kingaa.github.io/pomp/vignettes/twentycities.rda"
datfile <- file.path(tempdir(),"twentycities.rda")
download.file(daturl,destfile=datfile,mode="wb")
load(datfile)

## ----london-data-------------------------------------------------
measles |>
  mutate(year=as.integer(format(date,"%Y"))) |>
  filter(town=="London" & year>=1950 & year<1964) |>
  mutate(
    time=(julian(date,origin=as.Date("1950-01-01")))/365.25+1950
  ) |>
  filter(time>1950 & time<1964) |>
  select(time,cases) -> dat

demog |>
  filter(town=="London") |>
  select(-town) -> demogLondon

## ----prep-covariates-------------------------------------------------
demogLondon |>
  summarize(
    time=seq(from=min(year),to=max(year),by=1/12),
    pop=predict(smooth.spline(x=year,y=pop),x=time)$y,
    birthrate=predict(smooth.spline(x=year+0.5,y=births),x=time-4)$y,
    birthrate1=predict(smooth.spline(x=year+0.5,y=births),x=time)$y
  ) -> covar1

covar1 |>
  select(-birthrate1) -> covar

## ----pomp-construction-----------------------------------------------
dat |>
  pomp(t0=with(dat,2*time[1]-time[2]),
    time="time",
    rprocess=euler(rproc,delta.t=1/365.25),
    rinit=rinit,
    dmeasure=dmeas,
    rmeasure=rmeas,
    covar=covariate_table(covar,times="time"),
    accumvars=c("C","W"),
    statenames=c("S","E","I","R","C","W"),
    paramnames=c("R0","mu","sigma","gamma","alpha","iota",
      "rho","sigmaSE","psi","cohort","amplitude",
      "S_0","E_0","I_0","R_0")
  ) -> m1

## ----mles-----------------------------------------------
read_csv("
  town,loglik,loglik.sd,mu,delay,sigma,gamma,rho,R0,amplitude,alpha,iota,cohort,psi,S_0,E_0,I_0,R_0,sigmaSE
  Bedwellty,-1125.1,0.14,0.02,4,57.9,146,0.311,24.7,0.16,0.937,0.0396,0.351,0.951,0.0396,2.64e-05,2.45e-05,0.96,0.0611
  Birmingham,-3239.3,1.55,0.02,4,45.6,32.9,0.544,43.4,0.428,1.01,0.343,0.331,0.178,0.0264,8.96e-05,0.000335,0.973,0.0611
  Bradford,-2586.6,0.68,0.02,4,45.6,129,0.599,32.1,0.236,0.991,0.244,0.297,0.19,0.0365,7.41e-06,4.59e-06,0.964,0.0451
  Bristol,-2681.6,0.5,0.02,4,64.3,82.6,0.626,26.8,0.203,1.01,0.441,0.344,0.201,0.0358,9.62e-06,5.37e-06,0.964,0.0392
  Cardiff,-2364.9,0.73,0.02,4,39,143,0.602,34.4,0.223,0.996,0.141,0.267,0.27,0.0317,1.01e-05,9.21e-06,0.968,0.0539
  Consett,-1362.9,0.73,0.02,4,42.6,172,0.65,35.9,0.2,1.01,0.0731,0.31,0.406,0.0322,1.83e-05,1.97e-05,0.968,0.0712
  Dalton.in.Furness,-726.1,0.3,0.02,4,73.6,257,0.455,28.3,0.203,0.989,0.0386,0.421,0.818,0.0387,2.23e-05,2.36e-05,0.961,0.0779
  Halesworth,-318.6,0.51,0.02,4,49.6,210,0.754,33.1,0.381,0.948,0.00912,0.547,0.641,0.0526,1.99e-05,2.82e-05,0.947,0.0748
  Hastings,-1583.7,0.21,0.02,4,56.3,74.1,0.695,34.2,0.299,1,0.186,0.329,0.396,0.0233,5.61e-06,3.4e-06,0.977,0.0955
  Hull,-2729.4,0.39,0.02,4,42.1,73.9,0.582,38.9,0.221,0.968,0.142,0.275,0.256,0.0371,1.2e-05,1.13e-05,0.963,0.0636
  Leeds,-2918.6,0.23,0.02,4,40.7,35.1,0.666,47.8,0.267,1,1.25,0.592,0.167,0.0262,6.04e-05,3e-05,0.974,0.0778
  Lees,-548.1,1.1,0.02,4,45.6,244,0.612,29.7,0.153,0.968,0.0311,0.648,0.681,0.0477,2.66e-05,2.08e-05,0.952,0.0802
  Liverpool,-3403.1,0.34,0.02,4,49.4,39.3,0.494,48.1,0.305,0.978,0.263,0.191,0.136,0.0286,0.000184,0.00124,0.97,0.0533
  London,-3804.9,0.16,0.02,4,28.9,30.4,0.488,56.8,0.554,0.976,2.9,0.557,0.116,0.0297,5.17e-05,5.14e-05,0.97,0.0878
  Manchester,-3250.9,0.66,0.02,4,34.4,56.8,0.55,32.9,0.29,0.965,0.59,0.362,0.161,0.0489,2.41e-05,3.38e-05,0.951,0.0551
  Mold,-296.5,0.25,0.02,4,67.4,301,0.131,21.4,0.271,1.04,0.0145,0.436,2.87,0.064,2.61e-05,2.27e-05,0.936,0.0544
  Northwich,-1195.1,2.25,0.02,4,45.6,147,0.795,30.1,0.423,0.948,0.0602,0.236,0.402,0.0213,1.32e-05,1.58e-05,0.979,0.0857
  Nottingham,-2703.5,0.53,0.02,4,70.2,115,0.609,22.6,0.157,0.982,0.17,0.34,0.258,0.05,1.36e-05,1.41e-05,0.95,0.038
  Oswestry,-696.1,0.49,0.02,4,37.3,168,0.631,52.9,0.339,1.04,0.0298,0.263,0.476,0.0218,1.56e-05,1.61e-05,0.978,0.0699
  Sheffield,-2810.7,0.21,0.02,4,54.3,62.2,0.649,33.1,0.313,1.02,0.853,0.225,0.175,0.0291,6.04e-05,8.86e-05,0.971,0.0428") -> mles

## ----mle-----------------------------------------------
mles |> filter(town=="London") -> mle
paramnames <- c("R0","mu","sigma","gamma","alpha","iota",
  "rho","sigmaSE","psi","cohort","amplitude",
  "S_0","E_0","I_0","R_0")
mle[paramnames] |> unlist() -> theta
mle |> select(-S_0,-E_0,-I_0,-R_0) |> as.data.frame()

## ----pfilter1a-----------------------------------------------
library(doFuture)
plan(multicore)
foreach(i=1:4, .combine=c,
  .options.future=list(seed=998468235L)
) %dopar% {
  pfilter(m1,Np=10000,params=theta)
} -> pfs
## ----pfilter1b-----------------------------------------------
logmeanexp(logLik(pfs),se=TRUE)

## ----transforms-----------------------------------------------
pt <- parameter_trans(
  log=c("sigma","gamma","sigmaSE","psi","R0"),
  logit=c("cohort","amplitude"),
  barycentric=c("S_0","E_0","I_0","R_0")
)

## ----fold-transforms-----------------------------------------------
m1 |>
  pomp(partrans=pt,
    statenames=c("S","E","I","R","C","W"),
    paramnames=c("R0","mu","sigma","gamma","alpha","iota",
      "rho","sigmaSE","psi","cohort","amplitude",
      "S_0","E_0","I_0","R_0")) -> m1

## ----arma----------------------------
log_y <- as.vector(log(obs(m1)+1))
arma_fit <- arima(log_y,order=c(2,0,2))
arma_fit$loglik-sum(log_y)

## ----arma_benchmark----------------------------
arma_benchmark <- function(po,order=c(2,0,2)){
  x <- as.vector(obs(po))
  fit <- arima(log(x + 1), order = order)
  list(
    logLik = logLik(fit) - sum(log(x+1)),
    cond = dnorm(fit$resid,mean=0,sd=sqrt(fit$sigma2),log=T) - log(x+1)
  )
}

## ----arma-cond-loglik----------------------
benchmark <- arma_benchmark(m1)
pf_reps <- bake("pfilter-reps.rds",{
  library(future)
  library(doFuture)
  plan(multisession)
  foreach(i=1:20,.options.future=list(seed=50505),.combine=cbind) %dofuture% {
    m1 |> pfilter(Np=2000,params=mle[-(1:3)]) |> cond_logLik()
  }
})

## ----plot_anomalies------------------------------
op <- par(mai=c(0.8,0.8,0.1,0.1))
anomaly <- apply(pf_reps,1,mean)-benchmark$cond
plot(y=anomaly,x=time(m1),xlab="Date",ylab="Anomaly",type='h')
par(op)

## ----plot_pf_var------------------------------
op <- par(mai=c(0.8,0.8,0.1,0.1))
plot(apply(pf_reps,1,var),x=time(m1),xlab="Date",ylab="Variance",type='h')
par(op)
