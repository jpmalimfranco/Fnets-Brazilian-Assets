######################## Clean enviroment ######################################
rm(list=ls()) # limpar o Envirmonment
cat("\14")

######################### Packages #############################################
# List of packages you want to ensure are installed
packages <- c("dplyr",
              "INLA",
              "tidyverse",
              "gridExtra",
              "TTR",
              "quantmod",
              "rugarch",
              "tictoc",
              "stochvol",
              "stringr",
              "doParallel",
              "foreach")


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

############################ Aux. functions ####################################
# This function scans each numeric column in a data frame and replaces all NaN and 0 values with a small constant (epsilon).
# It returns the updated data frame with those substitutions applied.

sub_nan_zero_for_epsilon <- function(df, epsilon) {
  for (asset in colnames(df)) {
    if (is.numeric(df[[asset]])) {
      # Identifies NaN and replaces it
      nan_indices <- is.nan(df[[asset]])
      df[[asset]][nan_indices] <- epsilon
      
      # Identifies 0 and replaces it (including new converted NaNs, if necessary)
      zero_indices <- df[[asset]] == 0
      df[[asset]][zero_indices] <- epsilon
    }
  }
  return(df)
}
epsilon <- 1e-06

########################### Load data set ######################################

#
data_set_names <- c(
  "asset_price_original",
  "asset_price_joesley_day",
  "asset_price_pre_pandemic",
  "asset_price_post_pandemic",
  "asset_price_ukraine_war"
)

# Registrar cluster
cl <- makeCluster(min(length(data_set_names), detectCores()))
registerDoParallel(cl)

foreach(data_set_name = data_set_names, .packages = c()) %dopar% {
  
  #
  print(data_set_name)
  
  #
  path_1 <- paste("C:/Users/User/Dropbox/Fnets_Brazilian_Assets/Major_revision/Data/Crises/", data_set_name, ".csv", sep="")
  # path_1 <- paste("/home/jpmf/Fnets_Brazilian_Assets/Data/", data_set_name, ".csv", sep="")
  
  df.price <- read.csv(file=path_1)
  
  
  # Arrange data returns
  df.ret = df.price[, grepl(pattern = ".SA.Adjusted$", colnames(df.price))]
  df.ret = apply(log(df.ret), 2, diff)
  df.ret = data.frame(df.ret)
  colnames(df.ret) = stringr::str_replace_all(colnames(df.ret), pattern = ".SA.Adjusted", replacement = "")
  df.ret$OpenTime = df.price$OpenTime[2:dim(df.price)[1]]
  df.ret$OpenTime = as.Date(df.ret$OpenTime)
  
  #
  # asset_names <- df.ret %>% dplyr::select(-"OpenTime") %>% colnames()
  asset_names <- colnames(df.ret)[colnames(df.ret) != "OpenTime"]
  
  # demean returns
  for (name in asset_names) {
    # print(name)
    df.ret[, name] <- df.ret[, name] - mean(df.ret[, name])

  }
  
  
  ############################## SV model (MCMC) #################################
  
  #
  start_time <- Sys.time()

  SV_MCMC_vol <- list()
  for (asset in asset_names) {
    set.seed(123)
    res <- stochvol::svsample(df.ret[, asset],
                              priormu = c(0, 100),
                              priorphi = c(5, 1.5),
                              priorsigma = 1,
                              n_chains = 5,
                              draws = 10000,
                              burnin = 1000,
                              quiet = TRUE)

    vol <- as.numeric( res[["summary"]][["latent"]][,"mean(exp(h_t/2))"] )
    SV_MCMC_vol[[asset]] <- vol

    #
    cat("MCMC SV for", asset, "\n")

  }

  SV_MCMC_vol <- as.data.frame(SV_MCMC_vol); colnames(SV_MCMC_vol) = asset_names

  #
  SV_MCMC_vol <- log( SV_MCMC_vol )

  #
  SV_MCMC_vol$OpenTime <- df.ret$OpenTime

  #
  SV_MCMC_time <- Sys.time() - start_time
  
  
  ############################## SV model (INLA) #################################
  
  # INLA
  df.ret$Time <- 1:dim(df.ret)[1]
  
  #
  start_time <- Sys.time()
  
  SV_models <- list()
  vol_list <- list()
  for (asset in asset_names) {
    
    # Formula
    formula.sv <- df.ret[, asset] ~ 0 + f(Time, model = "ar1",
                                          hyper = list(prec = list(param=c(1,0.0001)),
                                                       mean = list(fixed = FALSE)))
    
    # Gaussian SV
    SV_models[["model.gaussian"]][[asset]] <- INLA::inla(formula.sv, family = "stochvol", data = df.ret,
                                                         control.predictor=list(compute=TRUE),
                                                         verbose = FALSE,
                                                         control.inla = list(stupid.search = FALSE, reordering = "metis",
                                                                             strategy = "adaptive", int.strategy = "eb",
                                                                             optimiser = 'default'),
                                                         #control.compute = list(config = TRUE), num.threads = 1
                                                         )
    
    # save volatility
    fit <- SV_models[["model.gaussian"]][[asset]][["summary.linear.predictor"]][["mean"]]
    vol <- exp( fit / 2)
    vol_list[["model.gaussian"]][[asset]] <- vol
    
    #
    cat("INLA Gaussian SV for", asset, "\n")
    
  }
  
  #
  vol_SV <- as.data.frame(vol_list); colnames(vol_SV) = asset_names
  
  # 
  vol_SV <- log( vol_SV )
  
  #
  vol_SV$OpenTime <- df.ret$OpenTime
  
  SV_time <- Sys.time() - start_time
  
  
  ####################### OHLC Volatility: Garman and Klass (1980) ###############
  
  #
  start_time <- Sys.time()
  
  #
  OHLC_vol <- matrix(data = NA, ncol = length(asset_names), nrow = dim(df.price)[1])
  colnames(OHLC_vol) <- asset_names
  for (asset in asset_names) {
    
    #
    OHLC_data <- df.price[, c( paste0(asset, ".SA.Open"),
                               paste0(asset, ".SA.High"), 
                               paste0(asset, ".SA.Low"),
                               paste0(asset, ".SA.Close"))]
    
    OHLC_vol[, paste0(asset)] <- TTR::volatility(OHLC_data, 
                                                 n = 1, 
                                                 N = 1,
                                                 calc = "garman.klass")
    
    #
    cat("OHLC Volatility for", asset, "\n")
    
  }
  
  #
  OHLC_vol <- data.frame(OHLC_vol)
  
  #
  OHLC_vol <- sub_nan_zero_for_epsilon(OHLC_vol, epsilon = epsilon)
  
  # 
  OHLC_vol <- log( OHLC_vol ) 
  
  #
  OHLC_vol$OpenTime <- as.Date(df.price$OpenTime)
  
  #
  OHLC_time <- Sys.time() - start_time
  
  ########################## GARCH (1,1) #########################################
  
  #
  start_time <- Sys.time()
  
  #
  garch_vol <- list()
  for (asset in asset_names) {
    
    # config.
    garch_spec <- rugarch::ugarchspec(variance.model=list(model="sGARCH",
                                                 garchOrder=c(1, 1)),
                             mean.model=list(armaOrder=c(0, 0)))
    # Estimation
    fit_garch <- rugarch::ugarchfit(spec = garch_spec, data = df.ret[, asset], solver = "hybrid")
    
    #
    vol <- fit_garch@fit[["sigma"]]
    garch_vol[[asset]] <- vol
    
    #
    cat("GARCH Model for", asset, "\n")
    
  }
  
  garch_vol <- as.data.frame(garch_vol); colnames(garch_vol) = asset_names
  
  #
  garch_vol <- log( garch_vol )
  
  #
  garch_vol$OpenTime <- df.ret$OpenTime
  
  #
  GARCH_time <- Sys.time() - start_time
  
  ############################# High-Low - Parkinson #############################
  
  #
  start_time <- Sys.time()
  
  #
  HL_vol <- matrix(data = NA, ncol = length(asset_names), nrow = dim(df.price)[1])
  colnames(HL_vol) <- asset_names
  for (asset in asset_names) {
    
    #
    data <- df.price[, c( paste0(asset, ".SA.Open"),
                          paste0(asset, ".SA.High"), 
                          paste0(asset, ".SA.Low"),
                          paste0(asset, ".SA.Close"))]
    #
    HL_vol[, paste0(asset)] <- TTR::volatility(data, 
                                               n = 1, 
                                               N = 1,
                                               calc = "parkinson")
    
    #
    cat("High-Low Volatility (Parkinson) Volatility for", asset, "\n")
    
  }
  
  HL_vol <- data.frame( HL_vol )
  
  #
  HL_vol <- sub_nan_zero_for_epsilon(HL_vol, epsilon = epsilon)
  
  # 
  HL_vol <- log( HL_vol ) 
  
  #
  HL_vol$OpenTime <- as.Date(df.price$OpenTime)
  
  #
  HL_time <- Sys.time() - start_time
  
  #################### Save results in a list ####################################
  vol_models_list <- list(SV = vol_SV,
                          SV_MCMC = SV_MCMC_vol,
                          OHLC = OHLC_vol[2:dim(OHLC_vol)[1], ],
                          GARCH = garch_vol,
                          HL = HL_vol[2:dim(HL_vol)[1], ],
                          estimation_time = list(SV = SV_time, 
                                                 SV_MCMC = SV_MCMC_time,
                                                 OHLC = OHLC_time, 
                                                 GARCH = GARCH_time, 
                                                 HL = HL_time))
  
  ########################## Save Results ########################################
  
  #
  idx <- stringr::str_remove(data_set_name, "^asset_price_")
  path_2 <- paste("C:/Users/User/Dropbox/Fnets_Brazilian_Assets/Major_revision/Codes/Vol_results/vol_models_list_", idx, ".RData", sep = "")
  # path_2 <- paste("/home/jpmf/Fnets_Brazilian_Assets/Codes/vol_models_list_", idx, ".RData", sep = "")
  
  # 
  save(vol_models_list, file = path_2)
  
}

stopCluster(cl)



################################# Quit server ##################################
# q(save = "no")

