#Configuration
remove(list = objects())
setwd("~/Documents/Gelman Research/ADVI test/")
library(parallel)
library(rstan)
stopifnot(packageVersion("rstan") >= "2.9.0-3")
rstan_options(auto_write = TRUE)

options(mc.cores = detectCores())
models <- stan_demo(0) # choose option 1

#Functions
test_ADVI <- function(NUTS_info_list, test_methods, cheat = T)
{
  models <- stan_demo(0)
  num_models <- length(models)
  SEED = 12345
  ADVI_model_info <- lapply(1:num_models, 
                            function(i) {run_advi_for_model(i, test_methods, SEED, NUTS_info_list[[models[i]]], cheat)})
  model_info <- lapply(1:num_models, 
                       function(i) {build_ADVI_results_for_model(ADVI_model_info[[i]], NUTS_info_list[[models[i]]], test_methods)})
  names(model_info) <- models
  model_info <- model_info[!sapply(model_info, is.null)]
  report_ADVI(model_info, test_methods)
  return(model_info)
}

run_advi_for_model <- function(i, test_methods, SEED, NUTS_info, cheat) {
  cat(paste(i, "\n", sep = ""))
  if (all(is.na(NUTS_info))) return(NULL)
  suppressWarnings(stan_model <- get_stanmodel(stan_demo(i, seed = SEED, iter = 1, chains = 1)))
  if (cheat) {
    lapply(test_methods, function(method) {run_advi_for_model_and_method(i, method, stan_model, SEED, NUTS_info[["inits"]])})
  } else {
    lapply(test_methods, function(method) {run_advi_for_model_and_method(i, method, stan_model, SEED, "random")})
  }
}

run_advi_for_model_and_method <- function(i, ADVI_method, stan_model, SEED, init_vals) {
  tryCatch(
    {
      ADVI <- as.matrix(stan_demo(i, object = stan_model, method = ADVI_method, seed = SEED, init = init_vals))
    },
    error = function(cond) {
      return(NA)
    })
}

build_ADVI_results_for_model <- function(ADVI_info_list, NUTS_info, methods_list) {
  if (all(is.na(NUTS_info))) return(NULL)
  info_matrix <- NUTS_info[["info_matrix"]]
  methods_info <- lapply(1:length(methods_list), function(i) {build_ADVI_results_for_model_and_method(ADVI_info_list[[i]], info_matrix, methods_list[i])})
  return(do.call(cbind, c(list(info_matrix), methods_info)))
}

build_ADVI_results_for_model_and_method <- function(ADVI_info, info_matrix, method) {
  if (all(is.na(ADVI_info))) {
    template_matrix <- matrix(0, nrow = nrow(info_matrix), ncol = 3)
    ADVI_info <- relist(NA, template_matrix)
  } else {
    ADVI_info <- get_ADVI_info(ADVI_info)
    ADVI_info <- cbind(ADVI_info, get_zscores(info_matrix, ADVI_info))
  }
  colnames(ADVI_info) <- 
    c(paste(method, "mean", sep = "_"), paste(method, "sd", sep = "_"), paste(method, "z-scores", sep = "_"))
  return(ADVI_info)  
}

get_ADVI_info <- function(ADVI_matrix) {
  mean <- colMeans(ADVI_matrix)
  sd <- apply(ADVI_matrix, 2, sd)
  return(cbind(mean, sd)[-length(mean),, drop = FALSE])
}

get_zscores <- function(info_matrix, ADVI_info) {
  NUTS_means <- info_matrix[, "mean", drop = FALSE]
  NUTS_sds <- info_matrix[, "sd", drop = FALSE]
  ADVI_means <- ADVI_info[, "mean", drop = FALSE]
  return((ADVI_means - NUTS_means) / NUTS_sds)
}

report_ADVI <- function(model_info, method_list) {
  sapply(method_list, function(method) {report_ADVI_for_method(model_info, method)})
}

report_ADVI_for_method <- function(model_info, method) {
  max_z_scores <- sapply(model_info, function(info_matrix) {get_max_zscore(info_matrix, method)}, simplify = T)
  plot_file_name = paste(method, "max_zscores_histogram.png", sep = "_")
  png(plot_file_name)
  hist(max_z_scores[!is.na(max_z_scores)], xlab = "Max abs z-scores", main = paste(method, "max abs z-scores"))
  dev.off()
  cat(paste("Printed histogram to", plot_file_name, "\n"))
  print_advi_output_message(max_z_scores, method)
}

get_max_zscore <- function(info_matrix, method) {
  z_scores <- info_matrix[, paste(method, "z-scores", sep = "_")]
  if (all(is.na(z_scores))) return(NA);
  z_scores <- abs(z_scores)
  return(unname(z_scores[which.max(z_scores)]))
}

print_advi_output_message <- function(advi_z_scores, advi_method) {
  output_message = paste("You tested the", advi_method, "ADVI algorithm on", length(advi_z_scores), "model(s).")
  output_message = paste(output_message, "\nThere are", sum(advi_z_scores > 1, na.rm = T), "model(s) with max absolute z scores above 1.")
  output_message = paste(output_message, "\nThere are", sum(advi_z_scores < 1, na.rm = T), "model(s) with max absolute z scores below 1.")
  output_message = paste(output_message, "\nThere are", sum(is.na(advi_z_scores)), "model(s) that did not turn out a result.\n")
  cat(output_message)
}

rename_matrix <- function(info_matrix, model_name) {
  rownames(info_matrix) <- sapply(rownames(info_matrix), function(name) {paste(model_name, name, sep = "_")}, simplify = T)
  return(info_matrix)
}

matrix_to_list <- function(models, info_matrix) {
  lapply(models, function(model_name) {info_matrix[grep(model_name, info_matrix[, "X"]), drop = F]})
}

#Runs the ADVI test
load("clean_NUTS_info.Rdata")
ADVI_output_file = "ADVI_report_info"
ADVI_report <- test_ADVI(NUTS_info_list, c("meanfield", "fullrank"))
save(ADVI_report, file = paste(ADVI_output_file, ".Rdata", sep = ""))
ADVI_report <- do.call(rbind, lapply(1:length(ADVI_report), function(i) {rename_matrix(ADVI_report[[i]], names(ADVI_report)[i])}))
cat(paste("Results saved in", ADVI_output_file, "and available to view in ADVI_report\n"))
write.csv(ADVI_report, file = paste(ADVI_output_file, ".csv", sep = ""))