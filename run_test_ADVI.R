#DEPRECATED

remove(list = objects())
setwd("~/Documents/Gelman Research/ADVI test/")
library(parallel)
library(rstan)
stopifnot(packageVersion("rstan") >= "2.9.0-3")
rstan_options(auto_write = TRUE)

options(mc.cores = detectCores())
models <- stan_demo(0) # choose option 1
source('test_ADVI.R')
load("clean_NUTS_info.Rdata")

ADVI_output_file = "ADVI_report_info.Rdata"
ADVI_report <- test_ADVI(NUTS_info_list, c("meanfield", "fullrank"))
cat(paste("Results saved in", ADVI_output_file, "and available to view in ADVI_report\n"))
save(ADVI_report, file = ADVI_output_file)