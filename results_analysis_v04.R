######################## Clean enviroment ######################################
rm(list=ls()) # limpar o Envirmonment

############################### Packages #######################################

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
              "xtable",
              "circlize",
              "viridis",
              "ggplot2",
              "reshape2",
              "multDM",
              "tseries",
              "moments",
              "xtable",
              "cowplot",
              "ggplot2",
              "GGally",
              "corrplot",
              "tidyr",
              "patchwork")

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

################################# Data Analysis ################################

# Load volatility measures
load(file = "C:/Users/User/Dropbox/Fnets_Brazilian_Assets/Major_revision/Results/Vol_results/vol_models_list_original.RData")
df.vol <- vol_models_list[["SV"]] %>% dplyr::select(c(-"OpenTime"))

# Must use exp()
df.vol <- exp(df.vol)
descr.stats.mat = matrix(data = NA, nrow = dim(df.vol)[2], ncol = 9)
rownames(descr.stats.mat) = c(colnames(df.vol))
colnames(descr.stats.mat) = c("Mean", 
                              "Std", 
                              "Skew", 
                              "Kurt",
                              "Min.", 
                              "5% Quantile", 
                              "Median", 
                              "95% Quantile ",
                              "Max.")

calc.descr.stats = function(x){
  out = rep(0, 9)
  out[1] = mean(x)
  out[2] = sd(x)
  out[3] = skewness(x)
  out[4] = kurtosis(x) # computed in kurtosis excess (package "tseries")
  out[5] = min(x)
  out[6] = stats::quantile(x, 0.05)
  out[7] = median(x)
  out[8] = stats::quantile(x, 0.95)
  out[9] = max(x)
  return(out)
}

for (i in 1:dim(df.vol)[2]) {
  
  descr.stats.mat[i, ] = calc.descr.stats(df.vol[, i])
  
}

descr_table <- xtable(descr.stats.mat, digits = 3)
descr_table <- descr_table[order(descr_table$Median, decreasing = TRUE), ] 

# descr_table %>% View()
xtable(descr_table, digits = 3, align = "lccccccccc", label = "table:descriptive_statistics",
       caption = "Descriptive Statistics")

# Sample correlation matrix
cor_matrix <- cor(df.vol)

# png(
#   "C:/Users/User/Dropbox/Fnets_Brazilian_Assets/Major_revision/Results/correlation_heatmap.png",  # Nome do arquivo
#   width = 5000,              
#   height = 4500,             
#   res = 400                  
# )

corrplot(
  cor_matrix,
  method = "color",
  type = "full",
  order = "original",
  tl.col = "black",
  tl.cex = .9,
  addCoef.col = NULL
)

# dev.off()

# Estimation Time 
df <- data.frame(vol_models_list[["estimation_time"]]) %>% t(); colnames(df) <- "Time"
row.names(df) <- c("SV via INLA", "SV via MCMC", "OHLC", "GARCH", "HL")

xtable::xtable(df, caption = "Estimation Time", label = "tab:estimation_time", align = "lc")



############################## Errors metrics ##################################



# Load forecast results
load(file = "C:/Users/User/Dropbox/Fnets_Brazilian_Assets/Major_revision/Results/forecast_results.RData")

forecast_results_list <- list(GARCH = list(Estimates=forecast_results_list[["GARCH.Estimates"]], 
                                           X_fore=forecast_results_list[["GARCH.X_fore"]]),
                              HL = list(Estimates=forecast_results_list[["HL.Estimates"]], 
                                        X_fore=forecast_results_list[["HL.X_fore"]]),
                              OHLC = list(Estimates=forecast_results_list[["OHLC.Estimates"]], 
                                          X_fore=forecast_results_list[["OHLC.X_fore"]]),
                              SV = list(Estimates=forecast_results_list[["SV.Estimates"]], 
                                        X_fore=forecast_results_list[["SV.X_fore"]]))


# Load volatility measures
load(file = "C:/Users/User/Dropbox/Fnets_Brazilian_Assets/Major_revision/Results/Vol_results/vol_models_list_original.RData")

#
n_window = 252

#
error_table <- matrix(data = NA, ncol = 12, nrow = dim( forecast_results_list[["SV"]][["X_fore"]])[2] )
colnames(error_table) <- c("MAE_SV", "MAE_OHLC", "MAE_HL", "MAE_GARCH",
                           "MAPE_SV", "MAPE_OHLC", "MAPE_HL", "MAPE_GARCH",
                           "MSE_SV", "MSE_OHLC", "MSE_HL","MSE_GARCH")
rownames(error_table) <- names( forecast_results_list[["SV"]][["X_fore"]] )

#
for (method in names(forecast_results_list)) {
  print(method)
  
  #
  vol_measure <- vol_models_list[[method]][(n_window+1):length(vol_models_list[["SV"]][["VALE3"]]), ] %>% dplyr::select(-"OpenTime")
  X_fore <- forecast_results_list[[method]][["X_fore"]][1:(dim(forecast_results_list[["SV"]][["X_fore"]])[1]-1), ] %>% as.data.frame()

  for (asset in names( forecast_results_list[["SV"]][["X_fore"]]) ) {
    print(asset)
    
    #
    actual <- vol_measure[, asset]
    predict <- X_fore[, asset]
    
    # Mean Absolute Error
    error_table[asset, paste("MAE_", method, sep = "")] <- Metrics::mae(actual = actual, predicted = predict)
    
    # Mean Absolute Percent Error
    error_table[asset, paste("MAPE_", method, sep = "")] <- Metrics::mape(actual = actual, predicted = predict)
    
    # Mean Squared Error
    error_table[asset, paste("MSE_", method, sep = "")] <- Metrics::mse(actual = actual, predicted = predict)
    
  }
  
}

#
View(error_table)


xtable(error_table,
       digits = 5,
       align = "lcccccccccccc",
       caption = "Error Metrics",
       label = "tab:error_metrics")


# Viollin Plots
df <- error_table %>% data.frame()
df$Ativo <- row.names(error_table)

dados_long <- df %>%
  pivot_longer(
    cols = -Ativo,
    names_to = c( "Metrica", "Metodo"),
    names_sep = "_",
    values_to = "Valor"
  )

# Função de plot
plot_violin <- function(metric) {
  p <- ggplot(filter(dados_long, Metrica == metric), aes(x = Metodo, y = Valor, fill = Metodo)) +
    geom_violin(trim = FALSE, alpha = 0.6, width = 0.4) +
    geom_boxplot(width = 0.1, fill = "white") +
    labs(title = metric, x = "", y = "") +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(size = 14)  # aumenta fonte do eixo x
    )
  
  # Ajustar limites dependendo da métrica
  if (metric == "MAPE") {
    p <- p + coord_cartesian(ylim = c(0.9, 1.1))
  }
  return(p)
}

p1 <- plot_violin("MAE")
p2 <- plot_violin("MAPE")
p3 <- plot_violin("MSE")

# Lado a lado
final_plot <- p1 / p2 / p3
final_plot

# ggsave("C:/Users/User/Dropbox/Fnets_Brazilian_Assets/Major_revision/Results/violin_plot.png", final_plot, width = 8, height = 10, dpi = 300)


########################## Network Metrics Table ###############################

#
network_metrics_function <- function(out){
  
  
  table_network <- matrix(data = NA, nrow = 4, ncol = 3); colnames(table_network) <- c("granger", "pc", "lrpc")
  row.names(table_network) <- c("Average Degree", "Betweenness Centrality (Average)", "Edge Density", "NNZs per Row (Average)")
  for (network_type in colnames(table_network)) {
    
    print(network_type)
    
    #
    net <- network(out, type = network_type)$network
    
    #
    w <- E(net)$weight
    dist_w <- 1 / (abs(w) + 1e-6)
    E(net)$weight <- dist_w
    
    #
    table_network["Average Degree", network_type] <- mean( igraph::degree(net, mode = "all") )
    
    #
    table_network["Betweenness Centrality (Average)", network_type] <- mean( igraph::betweenness(net,
                                                                                                 directed = is_directed(net),
                                                                                                 normalized = TRUE,
                                                                                                 weights = E(net)$weight) )
    #
    table_network["Edge Density", network_type] <- igraph::edge_density(net, loops = FALSE)
    
    #
    table_network["NNZs per Row (Average)", network_type] <- mean(rowSums( as_adjacency_matrix(net, sparse = TRUE) != 0) )
    
  }
  
  xtable::xtable( table_network, 
                  caption = "Network Metrics for Measured Networks", 
                  label = "network_metrics_for_measured_networks", 
                  align = "lccc", 
                  digits = 3)
  
}

#
idx_1 <- c("out_original")
idx_2 <- stringr::str_remove(idx_1, "^out_")
path_1 <- paste0("C:/Users/User/Dropbox/Fnets_Brazilian_Assets/Major_revision/Results/SV/", idx, ".RData")
load(file = path_1)

network_metrics_function(out = out)


# out_joesley_day
load(file = "C:/Users/User/Dropbox/Fnets_Brazilian_Assets/Major_revision/Results/SV/out_joesley_day.RData")
network_metrics_function(out = out)

# out_pre_pandemic
load(file = "C:/Users/User/Dropbox/Fnets_Brazilian_Assets/Major_revision/Results/SV/out_pre_pandemic.RData")
network_metrics_function(out = out)

# out_post_pandemic
load(file = "C:/Users/User/Dropbox/Fnets_Brazilian_Assets/Major_revision/Results/SV/out_post_pandemic.RData")
network_metrics_function(out = out)

# out_ukraine_war
load(file = "C:/Users/User/Dropbox/Fnets_Brazilian_Assets/Major_revision/Results/SV/out_ukraine_war.RData")
network_metrics_function(out = out)



