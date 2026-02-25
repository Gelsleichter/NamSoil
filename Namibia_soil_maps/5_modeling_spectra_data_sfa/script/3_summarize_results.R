###_____________________________________________________________________________
### Magyar Agrar- es Elettudomanyi Egyetem - MATE
### Modeling spectral data to predict soil properties, SFA Namibia
### Nov 2025 
### Yuri Andrei Gelsleichter, Marina Coetzee
###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨

###_____________________________________________________________________________
# +--- slant ::: https://www.askapache.com/online-tools/figlet-ascii/
#     __  ___________   
#    /  |/  /  _/ __ \  
#   / /|_/ // // /_/ /  
#  / /  / // // _, _/   
# /_/  /_/___/_/ |_|    
###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨

### Set working directory automatically ----
library(this.path)
path_ <- this.dir()
setwd(path_)
getwd()
# clean memory
# gc(); rm(list=ls())

library(data.table) # for fread() and rbindlist()
library(parallel) # for detectCores() and mclapply()
library(dplyr) # for summarize()

# Gather csv files (very fast for many files)
# List all metrics CSV files from the output directory
files <- list.files(
  path = "../output/modeling_output", 
  pattern = "results_metrics.csv", 
  recursive = TRUE, 
  full.names = TRUE
)
length(files) # 5040 files
cores <- detectCores() # - 1  # keep one core for system
system.time(
  list_dt <- mclapply(files, fread, mc.cores = cores)
)[3] # 5 seconds 
system.time(
  df <- rbindlist(list_dt)
)[3] # 2 seconds

dim(df) # 5040

# write combined results
readr::write_csv(df, "../output/modeling_output/1_gathered_results_all.csv")

# Calculate means grouped by variable columns mean
summary_df <- df |> 
  # dplyr::group_by(target, spectral, seg_col, seg_value, preproc, model) |> 
  dplyr::group_by(target, preproc, model) |> 
  # dplyr::summarise(across(where(is.numeric), mean, na.rm = TRUE)) ### old syntax, deprecated as of dplyr 1.1.0.
  # dplyr::summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop") # without rounding
  dplyr::summarise(across(where(is.numeric), \(x) round(mean(x, na.rm = TRUE), 3)), .groups = "drop") |> 
  dplyr::select(-ncomp_plsr, -fold_index, -repeatition)

# Calculate means grouped by variable columns median
summary_df_median <- df |> 
  dplyr::group_by(target, preproc, model) |> 
  dplyr::summarise(across(where(is.numeric), \(x) round(median(x, na.rm = TRUE), 3)), .groups = "drop") |> 
  dplyr::select(-ncomp_plsr, -fold_index, -repeatition)

# Write summary to CSV
fwrite(summary_df_median, "../output/modeling_output/2_averaged_median_results_by_each_split.csv")

# Select best results
top_3 <- summary_df_median |> 
  dplyr::group_by(target) |> 
  dplyr::arrange(RMSE_val) |> 
  dplyr::slice_head(n = 3)

fwrite(top_3, "../output/modeling_output/3_averaged_median_top_3_results_by_property.csv")

# Select best results
top_1 <- top_3 |> 
  dplyr::arrange(desc(R2_val)) |> 
  dplyr::slice_head(n = 1)

fwrite(top_1, "../output/modeling_output/4_averaged_median_top_results_by_property.csv")

###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
### Fetch the best model and prediction form each combination ----
###_____________________________________________________________________________
# from each target, preproc, model combination, fetch the best model and prediction files in "df" object

top_to_select <- top_1 |>
  tidyr::unite("comb_method", target:model, na.rm = TRUE, remove = FALSE)
mthds <- unique(top_to_select$comb_method)

to_select_df <- df |>
  tidyr::unite("comb_method", target:model, na.rm = TRUE, remove = FALSE)

narrowed_models <- to_select_df |>
  dplyr::filter(comb_method %in% mthds)

# Select best results from RMSE
top_3_selected_models <- narrowed_models |> 
  dplyr::select(-comb_method, -timestamp) |>
  dplyr::group_by(target) |> 
  dplyr::arrange(RMSE_val) |> 
  dplyr::slice_head(n = 3)

fwrite(top_3_selected_models, "../output/modeling_output/5_narrowed_top_3_models_low_rmse_hr2.csv")

# Select best results from top R2 on RMSE selected models
selected_models <- top_3_selected_models |> 
  dplyr::arrange(desc(R2_val)) |> 
  dplyr::slice_head(n = 1)

fwrite(selected_models, "../output/modeling_output/6_selected_models_low_rmse_hr2.csv")

as.data.frame(selected_models)

sm <- selected_models

to_fetch <- paste0(sm$target, "_", sm$preproc, "_", sm$model, "_", sm$resample, 
                   "_fold_index_", sm$fold_index, "_repeat_", sm$repeatition, 
                   "_predicted_values.csv")

# List all predicted values CSV files from the output directory
predicted_files <- list.files(
  path = "../output/modeling_output", 
  pattern = "predicted_values.csv", 
  recursive = TRUE, 
  full.names = TRUE
)

# Filter predicted files 
# pred <- predicted_files[predicted_files %in% to_fetch]
pred <- predicted_files[basename(predicted_files) %in% to_fetch]

# Combine all predictions into one data frame
list_pred <- lapply(pred, fread)
predicted <- do.call(cbind, list_pred)

# Add data to original dataset
# s4a_df <- read.csv("../output/dataset/merged_wc_mir.csv")
s4a_df <- fread("../output/dataset/merged_wc_mir.csv", check.names = T, select = 1:55)[1:193, ]
# as.data.frame(s4a_df) |> tail()
s4a_df <- as.data.frame(s4a_df)
predicted <- as.data.frame(predicted)

# s4a_df <- s4a_df[1:65, colnames(predicted)]
s4a_df <- s4a_df[, c(colnames(s4a_df)[1:9], colnames(predicted))]

predicted <- cbind(s4a_df[66:nrow(s4a_df), 1:9], predicted)

total_df <- rbind(s4a_df[1:65, ], predicted)

total_df[total_df < 0] <- 0

# Write combined predictions to CSV
fwrite(total_df, "../output/modeling_output/8_wet_chem_mir_predicted_values_s4a.csv")
fwrite(total_df, "../../6_external_validation_sfa/output/s4a_wet_chem_mir_predicted_values.csv") # for external validation

###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
### Plots ----
###_____________________________________________________________________________

###_____________________________________________________________________________
### Check predictions ----
###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨

library(ggplot2)
library(gridExtra)

props <- c("BaseSat", "Ca", "CEC", "Clay", "EC", "K", "Mg", "Na", "orgC", "P", "pH.H2O", "Sa", "Si", "totN")

plots <- lapply(props, function(col) {
  ggplot(total_df, aes(x = .data[[col]], color = data_type, fill = data_type)) +
    geom_histogram(aes(y = after_stat(density)), alpha = 0.3, position = "identity", bins = 30) +
    # geom_histogram(aes(y = after_stat(count)), alpha = 0.3, position = "identity", bins = 30) +
    geom_density(alpha = 0.2) +
    labs(title = col, x = col, y = "Density") +
    theme_minimal()
})
do.call(grid.arrange, c(plots, ncol = 3))

png("../output/modeling_output/density_plots_wet_chem_predicted_values.png", width = 1400, height = 1000)
do.call(grid.arrange, c(plots, ncol = 3))
dev.off()

plots <- lapply(props, function(col) {
  ggplot(total_df, aes(x = .data[[col]], fill = data_type, color = data_type)) +
    geom_histogram(alpha = 0.4, position = "identity", bins = 30) +
    # geom_density(aes(y = after_stat(count)), alpha = 0.2) +
    # geom_density(aes(y = after_stat(density)), alpha = 0.2) +
    labs(title = col, x = col, y = "Frequency") +
    theme_minimal()
})
do.call(grid.arrange, c(plots, ncol = 3))
png("../output/modeling_output/histogram_plots_wet_chem_predicted_values.png", width = 1400, height = 1000)
do.call(grid.arrange, c(plots, ncol = 3))
dev.off()


png("../output/modeling_output/histogram_plots_wet_chem_predicted_values_rbase_hist.png", width = 1200, height = 1000)
# par(mfrow = c(5, 3)) # set up the plotting area: 3 rows and 5 columns
par(mfrow = c(5, 3), mar = c(1.5, 2, 1.5, 2), oma = c(0, 0, 1, 0)) # mar = c(bottom, left, top, right) 
for(col in props) {
  
train <- total_df[total_df$data_type == "Train_CV", col]
pred <- total_df[total_df$data_type == "Pred", col]

h_train <- hist(train, plot = FALSE, breaks = 30)
h_pred <- hist(pred, plot = FALSE, breaks = 30)

d_train <- density(train)
d_pred <- density(pred)

max_y <- max(h_train$counts, h_pred$counts)
max_y <- max_y + max_y * 0.1

plot(h_train, col = rgb(0,0,1,0.4), border = "blue", 
     main = col, xlab = col, ylab = "Frequency", ylim = c(0, max_y))
lines(d_train$x, d_train$y * length(train) * diff(h_train$breaks)[1], col = "blue", lwd = 2)

plot(h_pred, col = adjustcolor("orange", alpha.f = 0.3), border = "orange2", add = T)
lines(d_pred$x, d_pred$y * length(pred) * diff(h_pred$breaks)[1], col = "orange2", lwd = 2)

if (col == "BaseSat") {
legend("topright", legend = c("Train_CV", "Pred"), pt.bg = "transparent", bg = "transparent", box.lwd = "",
       fill = c(rgb(0,0,1,0.4), adjustcolor("orange", alpha.f = 0.3)), border = c("blue","orange2"))
}
}
dev.off()


for (col in props) {
p <- ggplot(total_df, aes(x = .data[[col]], fill = data_type, color = data_type)) +
  geom_histogram(alpha = 0.4, position = "identity", bins = 30) +
  labs(title = col, x = col, y = "Frequency") +
  theme_minimal()
ggsave(paste0("../output/modeling_output/plot_by_property/histogram_", col, "_wet_chem_predicted_values.png"), 
       p, width = 8, height = 6)
}
for (col in props) {
p <- ggplot(total_df, aes(x = .data[[col]], fill = data_type, color = data_type)) +
  # geom_histogram(alpha = 0.4, position = "identity", bins = 30) +
  geom_density(aes(y = after_stat(density)), alpha = 0.2) +
  labs(title = col, x = col, y = "Density") +
  theme_minimal()
ggsave(paste0("../output/modeling_output/plot_by_property/density_", col, "_wet_chem_predicted_values.png"), 
       p, width = 8, height = 6)
}

###_____________________________________________________________________________
### Check models ----
###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨

# top_to_plot <- top_1 |> 
#   tidyr::unite("comb_method", target:model, na.rm = TRUE, remove = FALSE)
# mthds <- unique(top_to_plot$comb_method)
# 
# all_to_plot <- df |> 
#   tidyr::unite("comb_method", target:model, na.rm = TRUE, remove = FALSE)
# 
# selected_to_plot <- all_to_plot |> 
#   dplyr::filter(comb_method %in% mthds) |> 
#   dplyr::select(-comb_method)

selected_to_plot <- df

library(dplyr)
library(tidyr)
library(ggplot2)

# Transform to long format (for ggplot)
# df_long <- df |> 
df_long <- selected_to_plot |> 
  # dplyr::select("target", "preproc", "model", 
  #               "R2_cal", "MSE_cal", "RMSE_cal", "MAE_cal", "R2_val", "MSE_val", 
  #               "RMSE_val", "MAE_val") |> 
  dplyr::select("target", "preproc", "model", 
                "R2_cal", "RMSE_cal", "R2_val", "RMSE_val") |> 
  tidyr::pivot_longer(cols = where(is.numeric), names_to = "metric", values_to = "value")
glimpse(df_long)
table(df_long$metric)

# Filter R2 for plotting
df_plot <- df_long |> 
  dplyr::filter(grepl("^R2", metric)) |> 
  dplyr::mutate(metric = factor(metric, levels = c("R2_cal", "R2_val", "R2_test")))

ggplot(df_plot, aes(x = value, y = target , fill = model)) +
  geom_boxplot() +
  facet_grid(target ~ metric, scales = "free_y") +
  theme_minimal() +
  labs(title = "R2 Soil Properties", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.y = element_text(angle = 0)) # Horizontal labels

ggplot(df_plot, aes(x = metric, y = value, fill = model)) +
  geom_boxplot() +
  facet_grid(preproc ~ target, scales = "free_y") +
  theme_minimal() +
  labs(title = "R2 Soil Properties", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        strip.text.y = element_text(angle = 0), 
        strip.text.x = element_text(angle = 60))

ggplot(df_plot, aes(x = metric, y = value, fill = preproc)) +
  geom_boxplot() +
  facet_grid(model ~ target, scales = "free_y") +
  theme_minimal() +
  labs(title = "R2 Soil Properties", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        strip.text.y = element_text(angle = 0), 
        strip.text.x = element_text(angle = 60))

df_plot |> filter(metric == "R2_val") |>
  ggplot(aes(x = preproc, y = value, fill = model)) +
  geom_boxplot() +
  # facet_grid(metric ~ target, scales = "free_y") +
  facet_grid(target ~ metric, scales = "free_y") +
  theme_minimal() +
  labs(title = "R2 Soil Properties", x = "", y = "") +
  theme(text = element_text(size = 20), 
        axis.text.x = element_text(angle = 60, hjust = 1),
        strip.text.y = element_text(angle = 0), 
        strip.text.x = element_text(angle = 60)) +
  stat_summary(fun = median, geom = "text", 
               aes(label = round(after_stat(y), 3)), 
               position = position_dodge(width = 0.75), 
               vjust = -0.5, size = 3, color = "black")
ggsave("../output/modeling_output/boxplots_R2_choose_specs.png", limitsize = FALSE, 
       width = 10, height = 90)


# Filter RMSE for plotting
df_plot <- df_long |> 
  dplyr::filter(grepl("^RMSE", metric)) |> 
  dplyr::mutate(metric = factor(metric, levels = c("RMSE_cal", "RMSE_val", "RMSE_test")))

ggplot(df_plot, aes(x = value, y = target , fill = model)) +
  geom_boxplot() +
  facet_grid(target ~ metric, scales = "free_y") +
  theme_minimal() +
  labs(title = "RMSE Soil Properties", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.y = element_text(angle = 0)) # Horizontal labels

ggplot(df_plot, aes(x = metric, y = value, fill = model)) +
  geom_boxplot() +
  facet_grid(preproc ~ target, scales = "free_y") +
  theme_minimal() +
  labs(title = "RMSE Soil Properties", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        strip.text.y = element_text(angle = 0), 
        strip.text.x = element_text(angle = 60))

ggplot(df_plot, aes(x = metric, y = value, fill = preproc)) +
  geom_boxplot() +
  facet_grid(model ~ target, scales = "free_y") +
  theme_minimal() +
  labs(title = "RMSE Soil Properties", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        strip.text.y = element_text(angle = 0), 
        strip.text.x = element_text(angle = 60))

df_plot |> filter(metric == "RMSE_val") |>
  ggplot(aes(x = preproc, y = value, fill = model)) +
  geom_boxplot() +
  # scale_y_reverse() +
  # facet_grid(metric ~ target, scales = "free_y") +
  facet_grid(target ~ metric, scales = "free_y") +
  theme_minimal() +
  labs(title = "RMSE Soil Properties", x = "", y = "") +
  theme(text = element_text(size = 20), 
        axis.text.x = element_text(angle = 60, hjust = 1),
        strip.text.y = element_text(angle = 0), 
        strip.text.x = element_text(angle = 60)) +
  stat_summary(fun = median, geom = "text", 
               aes(label = round(after_stat(y), 3)), 
               position = position_dodge(width = 0.75), 
               vjust = -0.5, size = 3, color = "black")
ggsave("../output/modeling_output/boxplots_RMSE_choose_specs.png", limitsize = FALSE, 
       width = 10, height = 90)

# Per target (soil property)
targets <- unique(df$target)
for (t in targets) {
  df_subset <- df_long |> filter(target == t)
  p <- ggplot(df_subset, aes(x = model, y = value, fill = model)) +
    geom_boxplot() +
    facet_wrap(~ metric, scales = "free_y") +
    theme_minimal() +
    labs(title = paste("Boxplots to Target:", t))
  ggsave(paste0("../output/modeling_output/plot_by_property/boxplot_", t, ".png"), p, width = 10, height = 6)
}


###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
### End scrip ----
###_____________________________________________________________________________
