remove(list = objects())
library(parallel)
library(rstan)
stopifnot(packageVersion("rstan") >= "2.9.0-3")
rstan_options(auto_write = TRUE)
options(mc.cores = detectCores())
setwd("~/Documents/Gelman Research/ADVI test/")

models <- stan_demo(0) # choose option 1

source('test_NUTS.R')
test_NUTS('new_NUTS_info.Rdata')