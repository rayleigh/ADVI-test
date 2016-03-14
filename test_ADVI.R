test_ADVI <- function(NUTS_info_list, test_methods, cheat = T)
{
  models <- stan_demo(0)
  SEED <- 12345
  model_info <- list()
  model_start = 1
  if (file.exists("@temp@.Rdata")) load("@temp@.Rdata")
  for (i in model_start:length(models)) {
    cat(paste("Model", i, "\n"))
    NUTS_info = NUTS_info_list[[models[i]]]
    if (all(is.na(NUTS_info))) {next}
    suppressWarnings(stan_model <- get_stanmodel(stan_demo(i, seed = SEED, iter = 1)))
    info_matrix <- rbind(NUTS_info[["info_matrix"]], 0)
    for (j in 1:length(test_methods)) {
      method <- test_methods[j]
      ADVI_info <- get_ADVI_info_for_model(i, method, stan_model, SEED, NUTS_info[["inits"]], cheat)
      if (all(is.na(ADVI_info))) {
        info_matrix <- cbind(info_matrix, NA, NA)
        num_cols <- ncol(info_matrix)
        colnames(info_matrix)[(num_cols - 1):num_cols] <- 
          c(paste(method, "mean", sep = "_"), paste(method, "sd", sep = "_"))
      } else {
        info_matrix <- cbind(info_matrix, rbind(ADVI_info, 0))
      }
    }
    model_info[[models[i]]] <- info_matrix[-nrow(info_matrix), ]
    model_start = i + 1
    save(model_start, model_info, file = "@temp@.Rdata")
  }
  report_ADVI(model_info, test_methods)
  file.remove("@temp@.Rdata")
  return(model_info)
}

report_ADVI <- function(model_info, method_list) {
  sapply(method_list, function(method) {report_ADVI_for_method(model_info, method)})
}

report_ADVI_for_method <- function(model_info, method) {
  max_z_scores <- sapply(model_info, function(info_matrix) {get_advi_max_zscore(info_matrix, method)}, simplify = T)
  plot_file_name = paste(method, "max_zscores_histogram.png", sep = "_")
  png(plot_file_name)
  hist(max_z_scores[!is.na(max_z_scores)], xlab = "Max abs z-scores", main = paste(method, "max abs z-scores"))
  dev.off()
  print_advi_output_message(max_z_scores, method)
}

get_ADVI_info_for_model <- function(i, method, stan_model, SEED, init_vals, cheat = TRUE) {
  if (cheat) {
    ADVI <- run_advi(i, method, stan_model, SEED, init_vals)
  } else {
    ADVI <- run_advi(i, method, stan_model, SEED, "random")
  }
  if (is.null(ADVI)) return(NA)
  return(extract_ADVI_info(ADVI, method))
}

run_advi <- function(i, ADVI_method, stan_model, SEED, init_vals) {
  tryCatch(
    {
      ADVI <- stan_demo(i, object = stan_model, method = ADVI_method, seed = SEED, init = init_vals)
    },
    error = function(cond) {
      return(NULL)
    })
}

extract_ADVI_info <- function(ADVI_obj, method) {
  info_matrix <- summary(ADVI_obj)$summary[, c("mean", "sd")]
  colnames(info_matrix) <- paste(method, colnames(info_matrix), sep = "_")
  return(info_matrix[-nrow(info_matrix), ])
}

get_advi_max_zscore <- function(info_matrix, method) {
  info_matrix <- rbind(info_matrix, 0)
  NUTS_means <- info_matrix[-nrow(info_matrix), "mean"]
  NUTS_sds <- info_matrix[-nrow(info_matrix), "sd"]
  ADVI_means <- info_matrix[-nrow(info_matrix), paste(method, "mean", sep = "_")]
  if (all(is.na(ADVI_means))) return(NA);
  ADVI_zscores <- abs(((ADVI_means - NUTS_means) / NUTS_sds))
  return(unname(ADVI_zscores[which.max(ADVI_zscores)]))
}

print_advi_output_message <- function(advi_z_scores, advi_method) {
  output_message = paste("You tested the", advi_method, "ADVI algorithm on", length(advi_z_scores), "model(s).")
  output_message = paste(output_message, "\nThere are", sum(advi_z_scores > 1, na.rm = T), "model(s) with max absolute z scores above 1.")
  output_message = paste(output_message, "\nThere are", sum(advi_z_scores < 1, na.rm = T), "model(s) with max absolute z scores below 1.")
  output_message = paste(output_message, "\nThere are", sum(is.na(advi_z_scores)), "model(s) that did not turn out a result.\n")
  cat(output_message)
}

save_progress <- function(model_start, model_info, save) {
  if (!save) return()
  model_start = model_start + 1;
  save(model_start, model_info, file = "@temp@.Rdata")
}

# does not work in parallel on Windows
#z <- lapply(1:length(models), function(i) {test_ADVI(i)})
#z <- mclapply(1:length(models), function(i) {test_ADVI(i)}, mc.preschedule = FALSE)
#names(z) <- models
#z <- z[sapply(z, is.matrix)]
