remove(list = objects())
library(parallel)
library(rstan)
stopifnot(packageVersion("rstan") >= "2.9.0-3")
rstan_options(auto_write = TRUE)

setwd("~/Documents/Gelman Research/ADVI test/")
options(mc.cores = detectCores())
models <- stan_demo(0) # choose option 1
source('test_ADVI.R')
load("clean_NUTS_info.Rdata")

ADVI_report <- test_ADVI(NUTS_info_list, c("meanfield", "fullrank"))
save(ADVI_report, file = "ADVI_report_info.Rdata")