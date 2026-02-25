################################################################################
## R script for Digital Soil Mapping data preparation for Namibia Soil Map  ####
## Sep 2024                                                                 ####
## Author: Dr. Yuri Andrei Gelsleichter                                     ####
## License: CC-BY-NC-SA                                                     ####
################################################################################
#     ____  _       _ __        __   _____       _ __
#    / __ \(_)___ _(_) /_____ _/ /  / ___/____  (_) /
#   / / / / / __ `/ / __/ __ `/ /   \__ \/ __ \/ / / 
#  / /_/ / / /_/ / / /_/ /_/ / /   ___/ / /_/ / / /  
# /_____/_/\__, /_/\__/\__,_/_/   /____/\____/_/_/   
#         /____/                                     
#      __  ___                  _            
#     /  |/  /___ _____  ____  (_)___  ____ _
#    / /|_/ / __ `/ __ \/ __ \/ / __ \/ __ `/
#   / /  / / /_/ / /_/ / /_/ / / / / / /_/ / 
#  /_/  /_/\__,_/ .___/ .___/_/_/ /_/\__, /  
#              /_/   /_/            /____/   
# In linux terminal run: figlet -f slant Digital Soil Mapping 
# Or +--- slant ::: https:\\www.askapache.com/online-tools/figlet-ascii/
################################################################################

################################################################################
#' ### Auto setwd
library(this.path)
path_ <- this.dir()
setwd(path_)
getwd()
# gc(); rm(list=ls())
library(tidyr)

# sdb <- read.csv("../output/datasets/splines_soil_preperties_to_model_gee/Namibia_ea_spline_all_properties_2025_03_03.csv")
sdb <- read.csv("../output/datasets/splines_soil_preperties_to_model_gee/Namibia_ea_spline_all_properties_2025_03_03_depths_0_30_6_100.csv")

ea_spl_result_soil_type <- sdb

sapply(ea_spl_result_soil_type, max, na.rm = TRUE)
sapply(ea_spl_result_soil_type, min, na.rm = TRUE)

################################################################################
### Plot boxplot and hist for each soil property
### on data after spline 
# options(max.print = 600)

dfplt <- ea_spl_result_soil_type
dfplt |> dim()
names(dfplt)

for (i in 5:52) { # across all soil properties
  
  cat("Processing soil property: ", names(dfplt)[i], "\n", 
      which(names(dfplt) == names(dfplt)[i]), " of ", length(names(dfplt)), "\n")
  
  sprop_name <- names(dfplt)[i]
  sprop <- dfplt[, c(1, i)]
  sprop <- tidyr::drop_na(sprop)
  
  png(filename = paste0("../output/plots/stats_plots/hist_boxplot_after_spline_depths_0_30_60_100/hist_", sprop_name, ".png"), 
      width = 14, height = 10, units = "cm", pointsize = 12,
      bg = "transparent", res = 300)
  hist(sprop[[2]], main = sprop_name, 
       xlab = sprop_name, col = "skyblue", border = "grey40")
  dev.off()
  
  # Calculate summary statistics
  sts <- round(summary(sprop[[2]])[c(1, 3, 4, 6)], 3)
  
  png(filename = paste0("../output/plots/stats_plots/hist_boxplot_after_spline_depths_0_30_60_100/boxplot_", sprop_name, ".png"),
      width = 10, height = 6, units = "cm", pointsize = 12,
      bg = "transparent", res = 300)
  
  par(mar = c(5, 4, 4, 6))  # bottom, left, top, right
  
  boxplot(sprop[[2]], main = sprop_name, col = "skyblue", border = "grey40", horizontal = TRUE)
  
  # Place stats text
  mtext(side = 4, 
        line = .4, # Distance from margin to text
        las = 1, # Rotate the text to match with 90 degrees
        text = paste("Min:", sts[1], "\nMedian:", sts[2], "\nMean:", sts[3], "\nMax:", sts[4]), 
        cex = 0.8, 
        col = "black")
  
  dev.off()
}


#   #####   #####   #####   #####   #####   #####   #####   #####   #####   ####
#####   #####   #####   #####   #####   #####   #####   #####   #####   #####   
#   #####   #####   #####   #####   #####   #####   #####   #####   #####   ####
### End script
#####   #####   #####   #####   #####   #####   #####   #####   #####   #####   
#   #####   #####   #####   #####   #####   #####   #####   #####   #####   ####
#####   #####   #####   #####   #####   #####   #####   #####   #####   #####  






