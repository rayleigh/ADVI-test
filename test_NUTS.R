test_NUTS <- function(output_file) {
  models <- stan_demo(0)
  SEED <- 12345
  start = 1
  NUTS_info_list <- list()
  if (file.exists(output_file)) 
    load(output_file)
  
  for (i in start:length(models)) {
    cat(paste("Model", i, "\n"))
    
    NUTS <- run_nuts(i, SEED)
    
    if (is.null(NUTS)){
      NUTS_info_list[[models[i]]] = NA
    } else {
      NUTS_info <- extract_nuts_info(NUTS)
      if (all(rbind(NUTS_info[["info_matrix"]], NUTS_info[["info_matrix"]])[,"Rhat"] < 1.1, na.rm = T)) {
        NUTS_info_list[[models[i]]] <- NUTS_info
      } else {
        NUTS_info_list[[models[i]]] <- "FAILURE TO CONVERGE"
      }
    }
    start = start + 1
    save(start, NUTS_info_list, file = output_file)
  }
}

run_nuts <- function(i, SEED, num_iter = 2000) {
  tryCatch(
    {
      NUTS <- stan_demo(i, seed = SEED, iter = num_iter)
      if (NUTS@mode != 0) return(NULL)
      return(NUTS)
    },
    error = function(cond) {
      return(NULL)
    })
}

extract_nuts_info <- function(NUTS) {
  nuts_matrix <- summary(NUTS)$summary
  return(list("info_matrix" = nuts_matrix[-nrow(nuts_matrix), c("mean", "sd", "n_eff", "Rhat")],
              "inits" = relist(nuts_matrix[-nrow(nuts_matrix), "mean"], NUTS@inits[[1]])))
}  
