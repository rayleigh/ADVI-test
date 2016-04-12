######################################
#Run first to generate NUTS info file#
######################################
remove(list = objects())
library(parallel)
library(rstan)
stopifnot(packageVersion("rstan") >= "2.9.0-3")
rstan_options(auto_write = TRUE)

options(mc.cores = detectCores())
models <- stan_demo(0) # choose option 1
#skip_models = c("ARM/Ch.17/17.5_multilevel_poisson.stan")

source('test_NUTS.R')
test_NUTS('NUTS_info.Rdata')

#Run to clean up NUTS_info_list
remove(list = objects())
load('NUTS_info.Rdata')
NUTS_info_list <- lapply(NUTS_info_list, function(info) {if (is.list(info)) return(info); return(NA)})
NUTS_info_list[["knitr/soil-carbon/soil_incubation.stan"]] <- NA
save(NUTS_info_list, file = "clean_NUTS_info.Rdata")

##################
#Run to test ADVI#
##################
remove(list = objects())
library(parallel)
library(rstan)
stopifnot(packageVersion("rstan") >= "2.9.0-3")
rstan_options(auto_write = TRUE)

options(mc.cores = detectCores())
models <- stan_demo(0) # choose option 1
source('test_ADVI.R')
load("clean_NUTS_info.Rdata")

ADVI_report <- test_ADVI(NUTS_info_list, c("meanfield", "fullrank"))
cat("Results saved in ADVI_report_info.Rdata\n")
save(ADVI_report, file = "ADVI_report_info.Rdata")