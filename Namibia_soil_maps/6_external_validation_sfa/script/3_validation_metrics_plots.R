#############################################################################################
### External validation Namibia maps
### Yuri Andrei Gelsleichter, Marina Coetzee
### Jan 2026
#############################################################################################

### Set working directory automatically 
# Load library
library(this.path)
path_ <- this.dir()
setwd(path_)

# verify the working directory
getwd()

# clean memory and workspace
gc();rm(list=ls())

# options(scipen = 0) # turn off scientific notation
options(scipen = 999) # turn off scientific notation

#_______________________________________________________________________________
### Load datasets ----
#¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
sfa <- read.csv("../output/2_sfa_0_20_50_cm.csv")
nam <- read.csv("../output/2_nam_0_20_50_cm.csv")
isda <- read.csv("../output/2_isda_0_20_50_cm.csv")
sgri <- read.csv("../output/2_sgrid_0_20_50_cm.csv")

sfa[1:5, 1:5]
nam[1:5, 1:5]
isda[1:5, 1:5]
sgri[1:5, 1:5]

names(sfa)
names(nam)
names(isda)
names(sgri)


#_______________________________________________________________________________
### Manual approach ----
#¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨

df <- data.frame(Observed = sfa$OC.pct_sfa_0_20_cm, Predicted = nam$OC.pct_nam_0_20_cm)
df <- data.frame(Observed = sfa$OC.pct_sfa_20_50_cm, Predicted = nam$OC.pct_nam_20_50_cm)

library(tidyr)
df <- df |> tidyr::drop_na()

library(caret)
# Calculate the RMSE
# mts_0_20 <- caret::postResample(sfa$OC.pct_sfa_0_20_cm, nam$OC.pct_nam_0_20_cm) 
# mts_20_50 <- caret::postResample(sfa$OC.pct_sfa_20_50_cm, nam$OC.pct_nam_20_50_cm) 

val_mts <- caret::postResample(df$Predicted, df$Observed) 

metrics <- paste0("R² ", round(val_mts["Rsquared"] , 3),
                  "\nRMSE ", round(val_mts["RMSE"], 3),
                  "\nMAE ", round(val_mts["MAE"], 3)
                  )

library(ggplot2) # for coord_obs_pred()
library(tune) # for coord_obs_pred()

# Create the ggplot
p <- ggplot(df, aes(x = Predicted, y = Observed)) +
# p <- ggplot(data = soc_nam_nt_pts_df, aes(x = OCpct_0_5_cm_mean, y = orgC)) +
# p <- ggplot(data = soc_nam_nt_pts_df, aes(x = OCpct_0_30_cm_mean, y = orgC)) +
  geom_point(aes(color = "Data Points"), alpha = 0.5) +  # Scatter plot
  geom_smooth(method = 'lm', aes(color = "Fitted Line"), se = F, fullrange = T, linewidth = 0.6) +  # Fitted line
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", linewidth = 0.3, color = "black") +  # 1:1 line
  scale_color_manual(name = "", # Legend name
    values = c("Data Points" = "blue", "Fitted Line" = "dodgerblue3")) +
  ggtitle("Namibia National SOC Map with S4A") +
  scale_x_continuous(limits = c(min(df), max(df))) + # Extend the geom_somth line
  scale_y_continuous(limits = c(min(df), max(df))) + # Extend the geom_somth line
  xlab(paste0("Predicted (Nam_nat) SOC %")) +
  ylab(paste0("Observed (S4A) SOC %")) +
  annotate("text", x = .75, y = 0.2, label = metrics, parse = F) +
# annotate("text", x = .75, y = 0.2,
#            label = paste("R² ", round(val_mts["Rsquared"], 3), "\n",
#                          "RMSE ", round(val_mts["RMSE"], 3)#, "\n",
#                          # "MAE ", round(val_mts["MAE"], 3)
#                          ), parse = F) +
  theme_minimal() + coord_obs_pred() # library(tune)
p

# Save the plot using ggsave
# ggsave(filename= paste0("../output/obs_pred_S4A_Nam_national_0_20cm.png"), 
#        plot = p, 
#        width = 9, height = 9, dpi = 300)

#_______________________________________________________________________________
### Automated way ----
#¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨

# List of predicted datasets
pred_datasets <- list(Nam_nat = nam, iSDA = isda, S_Grids = sgri)
pred_names <- names(pred_datasets)
suffix <- c("nam", "isda", "sgri")

# Function to strip suffixes and get common vars
get_common_vars <- function(obs_cols, pred_cols, obs_suffix = "_sfa_", pred_suffix) {
  obs_vars <- gsub(paste0(obs_suffix, "(0_20_cm|20_50_cm)"), "", obs_cols)
  obs_vars <- unique(gsub("Soil.ID", NA, obs_vars))
  pred_vars <- gsub(paste0(pred_suffix, "(0_20_cm|20_50_cm)"), "", pred_cols)
  pred_vars <- unique(gsub("Soil.ID", NA, pred_vars))
  common_vars <- intersect(obs_vars, pred_vars)
  return(common_vars)
}

### Debug
# obs_vars <- gsub(paste0("_sfa_", "(0_20_cm|20_50_cm)"), "", names(sfa))
# obs_vars <- unique(gsub("Soil.ID", NA, obs_vars))
# pred_vars <- gsub(paste0("_sgri_", "(0_20_cm|20_50_cm)"), "", names(sgrid))
# pred_vars <- unique(gsub("Soil.ID", NA, pred_vars))
# common_vars <- intersect(obs_vars, pred_vars)

# Results storage
results <- list()

# Plot function
make_plot <- function(df, plt_name, var, depth, metrics) {
  ggplot(df, aes(x = Predicted, y = Observed)) +
    geom_point(aes(color = "Data Points"), alpha = 0.5) +
    geom_smooth(method = 'lm', formula = y ~ x, aes(color = "Fitted Line"), se = F, fullrange = T, linewidth = 0.6) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", linewidth = 0.3, color = "black") +
    scale_color_manual(name = "", values = c("Data Points" = "blue", "Fitted Line" = "dodgerblue3")) +
    # ggtitle(paste("S4A /", plt_name)) +
    ggtitle(paste("S4A /", plt_name), subtitle = depth) +
    scale_x_continuous(limits = c(min(df), max(df))) +
    scale_y_continuous(limits = c(min(df), max(df))) +
    xlab(paste0("Predicted (", plt_name, ") ", var)) +
    ylab(paste0("Observed (S4A) ", var)) +
    annotate("text", x = Inf, y = -Inf, label = metrics, hjust = 1, vjust = -1, parse = F) +
    theme_minimal(base_size = 18) +  # Increases all fonts
    coord_obs_pred()
}

# Loop over datasets
for (i in seq_along(pred_datasets)) {
  pred <- pred_datasets[[i]]
  plt_name <- pred_names[i]
  pred_name <- suffix[i]
  pred_suffix <- paste0("_", tolower(pred_name), "_")
  
  # Merge on Soil.ID
  merged <- merge(sfa, pred, by = "Soil.ID", all = FALSE)
  
  # Get common vars (without depths)
  common_vars <- get_common_vars(names(sfa), names(pred), "_sfa_", pred_suffix)
  common_vars <- common_vars[complete.cases(common_vars)]
  
  # For each common var
  for (var in common_vars) {
    # For each depth
    for (depth in c("0_20_cm", "20_50_cm")) {
      obs_col <- paste0(var, "_sfa_", depth)
      pred_col <- paste0(var, pred_suffix, depth)
      
      if (obs_col %in% names(merged) && pred_col %in% names(merged)) {
        df <- data.frame(Observed = merged[[obs_col]], Predicted = merged[[pred_col]])
        df <- df |> tidyr::drop_na()
        
        if (nrow(df) > 0) {
          val_mts <- caret::postResample(df$Predicted, df$Observed)
          
          metrics_text <- paste0("R² ", round(val_mts["Rsquared"], 3),
                                 "\nRMSE ", round(val_mts["RMSE"], 3),
                                 "\nMAE ", round(val_mts["MAE"], 3))
          
          # Store results
          key <- paste(pred_name, var, depth, sep = "_")
          results[[key]] <- val_mts
          
          # Generate plot
          # p <- make_plot(df, plt_name, paste(var, depth), metrics_text)
          p <- make_plot(df, plt_name, var, depth, metrics_text)
          # print(p)  
          ggsave(paste0("../output/plots/", key, ".png"), 
                 plot = p, 
                 width = 9, height = 9, dpi = 300)
        }
      }
    }
  }
}

#_______________________________________________________________________________
### Output results as table ----
#¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
results

results_df <- do.call(rbind, lapply(names(results), function(k) {
  data.frame(Dataset_Var_Depth = k, RMSE = results[[k]]["RMSE"], Rsquared = results[[k]]["Rsquared"], MAE = results[[k]]["MAE"])
}))
print(results_df)
rownames(results_df) <- NULL

library(dplyr)
results_df <- results_df |> 
  dplyr::mutate(across(c(RMSE, Rsquared, MAE), ~ round(., 3)))

results_df <- results_df |>
tidyr::separate(col = Dataset_Var_Depth, 
                into = c("Dataset", "Variable", "Upper_Depth", "Lower_Depth", "depth_untis"), 
                sep = "_", extra = "merge")

results_df <- results_df |> 
  dplyr::mutate(Depth = paste(Upper_Depth, Lower_Depth, sep = "-")) |> 
  dplyr::arrange(Variable, Upper_Depth, Dataset) |> 
  dplyr::select(Variable, Depth, Dataset, RMSE, Rsquared, MAE)


results_df
write.csv(results_df, "../output/validation_metrics_namibia_maps.csv", row.names = FALSE)

#¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
### End script ----
#_______________________________________________________________________________