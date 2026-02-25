#¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
# Mapping NEON sites -- functions
# Yuri Andrei Gelsleichter
# May 2025 
#_______________________________________________________________________________

#¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
# Function to create and save the plot ----
#_______________________________________________________________________
library(tune) # for coord_obs_pred()
library(ggplot2) # for ggplot()
library(ggExtra) # for ggMarginal()	

plt <- function(data_obs_pred, ttl, lims, rsq, rmse_, plt_name, plot_dir) {
  p <- ggplot(data_obs_pred, aes(x = Predicted, y = Observed)) +
    geom_point(aes(color = "Data Points"), alpha = 0.5) +  # Scatter plot
    geom_smooth(method = 'lm', aes(color = "Fitted Line"), se = F, fullrange = T, linewidth = 0.6) +  # Fitted line
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", linewidth = 0.3, color = "black") +  # 1:1 line
    scale_color_manual(name = "", values = c("Data Points" = "blue", "Fitted Line" = "darkorange1")) +
    ggtitle(ttl) +
    scale_x_continuous(limits = c(0, lims)) + # Extend the geom_somth line
    scale_y_continuous(limits = c(0, lims)) + # Extend the geom_somth line
    xlab(paste0("Predicted ", target, " (", unity[[target]], ")")) +
    ylab(paste0("Observed " , target, " (", unity[[target]], ")")) +
    annotate(size = 5, "text", x = lims*.9, y = lims*.1, 
             label = paste("R² ", round(rsq, 2), "\n",
                           "RMSE ", round(rmse_, 2)), parse = F) +
    theme_minimal() + 
    # theme(legend.position = "top") +  # Move legend to top
    # theme(legend.position = c(lims*.9, lims*.2)) + # c(x, y)
    theme(legend.position = c(0.88, 0.3), # c(x, y)
          # legend.text = element_text(size=11),
          text = element_text(size= 18)) + 
    coord_obs_pred() # library(tune)
  # p
  
  # Add marginal histograms
  phist <- ggMarginal(p, 
                      alpha = 0.2,
                      type = "densigram", # "density", "histogram", "boxplot", "violin", "densigram"
                      margins = c("both"), # set for x (Predicted only) or y (Observed only)
                      xparams = list(fill = "cyan2", col = "grey70", linewidth= 0.3), # bins = 30
                      yparams = list(fill = "cyan2", col = "grey70", linewidth= 0.3)) # bins = 30
  
  # Save the plot using ggsave
  ggsave(filename= paste0(plot_dir, plt_name), 
         plot = phist, # or 'p'
         width = 9, height = 9, dpi = 300)
  # return(p) # avoid dependency on global variables
  return(phist)
}

#¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
# Plot Covariate Importance ----
#_______________________________________________________________________
plt_var_imp <- function(var_imp_df, exp_dt_dir, exp_plt_dir, ttl, plot_dir, plt_name) {
  # Data frame for variable importance
  var_imp_df$Variable <- rownames(var_imp_df)
  rownames(var_imp_df) <- NULL
  
  # Rename columns to facilitate use in ggplot2
  colnames(var_imp_df) <- c("Importance", "Variable")
  
  # Sort by importance
  var_imp_df <- var_imp_df[order(-var_imp_df$Importance),]
  
  # Rescale importance values
  var_imp_df$Importance <- scales::rescale(var_imp_df$Importance, to = c(1, 100))
  
  var_imp_df_lim <- var_imp_df[1:100, ]  # Limit to top 100 variables for clarity
  
  # Get number of variables to adjust plot height
  nr <- nrow(var_imp_df_lim) 
  
  # Remove rows with NA values (in case the num of vars is less than 100)
  var_imp_df_lim <- var_imp_df_lim[complete.cases(var_imp_df_lim), ]
  
  # Create the lollipop plot
  p <- ggplot(var_imp_df_lim, aes(x = reorder(Variable, Importance), y = Importance)) +
    geom_segment(aes(xend = reorder(Variable, Importance), yend = 0), 
                 color = "dodgerblue", 
                 linewidth = 0.6) +  # Match linewidth with first plot
    geom_point(color = "dodgerblue", 
               size = 3, 
               alpha = 0.5) +  # Match alpha with first plot
    coord_flip() +
    scale_y_continuous(limits = c(0, 100)) +  # Set consistent y-axis limits
    xlab("Covariates") +
    ylab("Importance") +
    ggtitle(ttl) +
    theme_minimal() +
    theme(
      text = element_text(size = 18),  # Match text size with first plot
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14, face = "bold"),
      legend.position = "none"  # Remove legend to match first plot style
    )
  # p
  
  # Save the plot
  ggsave(filename = paste0(exp_plt_dir, plt_name), 
         plot = p, 
         width = 6, 
         height = (nr*0.16), # Adjust height based on number of variables
         dpi = 300)
  
  # Write CSV with scaled importance
  write.csv(var_imp_df, file = paste0(exp_dt_dir, cov_imp_data_name), row.names = F)

  return(p)
}
