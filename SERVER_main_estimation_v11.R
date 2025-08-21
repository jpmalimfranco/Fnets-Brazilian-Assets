######################## Clean enviroment ######################################
rm(list=ls()) # limpar o Envirmonment

############################ Load Packages #####################################

# List of packages you want to ensure are installed
packages <- c("fnets", 
              "dplyr", 
              "quantmod", 
              "INLA",
              "tidyverse",
              "astsa",
              "gridExtra",
              "doParallel",
              "foreach",
              "grDevices",
              "igraph",
              "pheatmap",
              "xtable")

# Specify the CRAN repository
cran_repo <- "https://cloud.r-project.org/"  # Change this to your preferred CRAN mirror

# Function to install packages that are not installed
install_packages <- function(packages, repos) {
  # Check which packages are not installed
  not_installed <- packages[!(packages %in% installed.packages()[, "Package"])]
  
  # Install packages that are not installed
  if (length(not_installed) > 0) {
    install.packages(not_installed, repos = repos)
  }
}

# Execute the function to ensure packages are installed
install_packages(packages, cran_repo)

# Load the packages
lapply(packages, library, character.only = TRUE)

####################### Function: Networks Estimation ##########################
networks_estimation <- function(x, model){
  
  #
  out <- fnets::fnets(x,
                      center = TRUE,
                      fm.restricted = FALSE,
                      q = "ic",
                      ic.op = NULL,
                      kern.bw = NULL,
                      common.args = list(factor.var.order = NULL, 
                                         max.var.order = NULL, 
                                         trunc.lags = 20,
                                         n.perm = 10), 
                      var.order = 1, 
                      var.method = "lasso",
                      var.args = list(n.iter = NULL, 
                                      n.cores = min(parallel::detectCores() - 1, 10)),
                      do.threshold = TRUE, # adaptative threshold
                      do.lrpc = TRUE, 
                      lrpc.adaptive = FALSE,
                      tuning.args = list(tuning = "bic", 
                                         n.folds = 1, 
                                         penalty = NULL,
                                         path.length = 10, 
                                         do.plot = FALSE))
  
  # Save out
  idx <- stringr::str_remove(vol_list_name, "^vol_models_list_")
  path_1 <- paste("C:/Users/User/Dropbox/Fnets_Brazilian_Assets/Major_revision/Results/SV/out_", idx, ".RData", sep = "")
  # path_1 <- paste("/home/jpmf/Fnets_Brazilian_Assets/Codes/vol_models_list_", idx, ".RData", sep = "")
  save(out, file = path_1)
  
  
  
  # Heatmaps
  
  #
  
  # png(paste0("/home/jpmf/Fnets_Brazilian_Assets_RBFin/Results/", model ,"/Granger_heatmap.png"), res = 350, width = 4000, height = 3500)
  grDevices::png(paste0("C:/Users/User/Dropbox/Fnets_Brazilian_Assets/Major_revision/Results/", model ,"/Granger_heatmap_", idx,".png"), res = 450, width = 5500, height = 5000)
  plot(
    out,
    type = "granger",
    display = "heatmap",
    names = colnames(x),
    cex.lab = 1.3,
    cex.main = 1.8,
    legend.cex = 1.1,
    legend.width = 1.2,
    legend.shrink = 0.75,
    legend.mar = 4
  )
  dev.off()

  # png(paste0("/home/jpmf/Fnets_Brazilian_Assets_RBFin/Results/", model ,"/PC_heatmap.png"), res = 350, width = 4000, height = 3500)
  grDevices::png(paste0("C:/Users/User/Dropbox/Fnets_Brazilian_Assets/Major_revision/Results/", model ,"/PC_heatmap_", idx,".png"), res = 450, width = 5500, height = 5000)
  plot(
    out,
    type = "pc",
    display = "heatmap",
    names = colnames(x),
    cex.lab = 1.3,
    cex.main = 1.8,
    legend.cex = 1.1,
    legend.width = 1.2,
    legend.shrink = 0.75,
    legend.mar = 4
  )
  dev.off()

  # png(paste0("/home/jpmf/Fnets_Brazilian_Assets_RBFin/Results/", model ,"/LRPC_heatmap.png"), res = 350, width = 4000, height = 3500)
  grDevices::png(paste0("C:/Users/User/Dropbox/Fnets_Brazilian_Assets/Major_revision/Results/", model ,"/LRPC_heatmap_", idx,".png"), res = 450, width = 5500, height = 5000)
  plot(
    out,
    type = "lrpc",
    display = "heatmap",
    names = colnames(x),
    cex.lab = 1.3,
    cex.main = 1.8,
    legend.cex = 1.1,
    legend.width = 1.2,
    legend.shrink = 0.75,
    legend.mar = 4
  )
  dev.off()
  
}

################################## Run Networks Function #######################
#
vol_list_names <- c("vol_models_list_joesley_day", 
                    "vol_models_list_pre_pandemic", 
                    "vol_models_list_post_pandemic", 
                    "vol_models_list_ukraine_war", 
                    "vol_models_list_original")

#
for (vol_list_name in vol_list_names) {
  vol_list_name %>% print()
  
  # Load volatility measures
  path_1 <- paste0("C:/Users/User/Dropbox/Fnets_Brazilian_Assets/Major_revision/Results/Vol_results/", vol_list_name, ".RData")
  # load(file = "/home/jpmf/Fnets_Brazilian_Assets_RBFin/Results/vol_models_list.RData")
  load(file = path_1)
  
  #------------- Run Function in Parallel -------------------------------------#
  
  # Detect the number of available cores
  n_cores <- parallel::detectCores() - 1
  
  # Create the cluster with the defined number of cores
  cl <- makeCluster(n_cores)
  
  # Register the cluster to be used by foreach
  registerDoParallel(cl)
  
  # 
  # method_list <- c("HL", "OHLC", "SV", "GARCH")
  method_list <- c("SV")
  
  foreach(
    method = method_list,
    .packages = "dplyr" # Tells the workers to load the dplyr package
  ) %dopar% {
    
    # --- Start of each worker's code ---
    
    # Prepare the dataset
    x <- data.frame(vol_models_list[[method]])
    x <- x %>% dplyr::select(-c("OpenTime"))
    
    # Execute the main function
    vol_results <- networks_estimation(x = x, model = method)
    
    
  }
  
  #
  stopCluster(cl)
  
}


########################### Forecasting function ###############################
forecast_function <- function(x, model, n_window){
  pred <- list()

  for (i in 1:(dim(x)[1] - n_window + 1)) {
    #
    print(i)

    #
    out <- fnets::fnets(x[i:(n_window + i - 1), ],
                        center = TRUE,
                        fm.restricted = FALSE,
                        q = "ic",
                        ic.op = NULL,
                        kern.bw = NULL,
                        common.args = list(factor.var.order = NULL, 
                                           max.var.order = NULL, 
                                           trunc.lags = 20,
                                           n.perm = 10), 
                        var.order = 1, 
                        var.method = "lasso",
                        var.args = list(n.iter = NULL, 
                                        n.cores = min(parallel::detectCores() - 1, 10)),
                        do.threshold = TRUE, # adaptative threshold
                        do.lrpc = TRUE, 
                        lrpc.adaptive = FALSE,
                        tuning.args = list(tuning = "bic", 
                                           n.folds = 1, 
                                           penalty = NULL,
                                           path.length = 10, 
                                           do.plot = FALSE))

    pred[[model]][["Estimates"]][[i]] <- predict(out,
                                                 h = 1,
                                                 fc.restricted = FALSE)

  }

  #
  X_fore <- matrix(data = NA, nrow = length(pred[[model]][["Estimates"]]), ncol = dim(x)[2]); colnames(X_fore) <- names(x)
  for (i in 1:length(pred[[model]][["Estimates"]])) {

    # forecast
    X_fore[i, ] <- pred[[model]][["Estimates"]][[i]][["forecast"]] %>% as.vector()

  }

  #
  pred[[model]][["X_fore"]] <- as.data.frame( X_fore )

  return(pred)

}


################################# Forecasting ###################################

#
vol_list_name <- c("vol_models_list_original")

#
path_1 <- paste0("C:/Users/User/Dropbox/Fnets_Brazilian_Assets/Major_revision/Results/Vol_results/", vol_list_name, ".RData")
load(file = path_1)

# List of methods to process
method_list <- c("GARCH", "HL", "OHLC", "SV")

# Setup parallel backend
num_cores <- detectCores() - 1  # Reserve one core for system processes
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Execute parallel processing
forecast_results_list <- foreach(
  method = method_list,
  .packages = c("dplyr"),  # Packages needed in each worker
  .combine = "c",          # Combine results by concatenation
  .multicombine = TRUE,    # Optimize combination for multiple results
  .inorder = TRUE          # Maintain original method order
) %dopar% {
  # Prepare dataset
  x <- as.data.frame(vol_models_list[[method]])
  x <- select(x, -OpenTime)
  
  # Execute forecasting function
  result <- forecast_function(x = x, model = method, n_window = 252)

  # Return named result
  setNames(result, method)
  
}

# 
stopCluster(cl)

# 
forecast_results_list <- unlist(forecast_results_list, recursive = FALSE)


# Save forecasting results
save(forecast_results_list, file = "C:/Users/User/Dropbox/Fnets_Brazilian_Assets/Major_revision/Results/forecast_results.RData")



# ################################# Quit server ##################################
# q(save = "no")
