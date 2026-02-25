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
# gc(); rm(list=ls())

sdb <- read.csv("../output/datasets/dataset_for_splines/Dataset_for_splines_2025_03_03.csv")
# sdb$X <- NULL
names(sdb)[1] <- "Soil.ID"
names(sdb)[5] <- "y"
names(sdb)[6] <- "x"
df <- sdb

### Reorder columns for ea_spline (must be "Soil.ID", "Upper.Depth", "Lower.Depth")
# Also select
df <- df[, c("Soil.ID", "Upper.Depth", "Lower.Depth", "Dataset", "HONU", 
             "Year", "y", "x", "WRB2015", "Sand.53.um.pct", "Silt.2.53um.pct", 
             "Clay.2.um.pct", "BD.kg.dm3", "pH.water", "EC.uS.cm.EL25", 
             "EC.uS.cm.ELCO", "Ca.mg.kg", "Mg.mg.kg", "K.mg.kg", "Na.mg.kg", 
             "CEC.cmol.kg", "BS.pct", "N.mg.kg", "P.mg.kg", "OC.pct")]

sdb[sdb$Soil.ID == "TFO-P2016", ]

################################################################################
### Basic statistics (plots)
################################################################################
library(tidyverse) # for drop NA

df[1:10, ]
df |> names()

for (i in 10:25) {
  
  cat("Processing soil property: ", names(df)[i], "\n", 
      which(names(df) == names(df)[i]), " of ", length(names(df)), "\n")
  
  sprop_name <- names(df)[i]
  sprop <- df[, c(6, i)]
  sprop <- drop_na(sprop)
  
  png(filename = paste0("../output/plots/stats_plots/hist_boxplot_original_data/hist_", sprop_name, ".png"), 
      width = 14, height = 10, units = "cm", pointsize = 12,
      bg = "transparent", res = 300)
  hist(sprop[[1]], main = sprop_name, xlab = "Years", col = "skyblue", border = "grey40", 
       # xlim = c(min(df$Year, na.rm = TRUE), max(df$Year, na.rm = TRUE)))
       xlim = c(1990, 2025))
  dev.off()
  
  # png(filename = paste0("../output/plots/stats_plots/hist_boxplot_original_data/hist_to_stack/hist_", sprop_name, ".png"), 
  #     width = 14, height = 10, units = "cm", pointsize = 12,
  #     bg = "transparent", res = 300)
  # if (i != 25) {
  # hist(sprop[[1]], main = sprop_name, xlab = "", col = "skyblue", border = "grey40", axes =F)
  # } else {
  # hist(sprop[[1]], main = sprop_name, xlab = "Years", col = "skyblue", border = "grey40", 
  #      xlim = c(1990, 2025))
  # }
  # dev.off()
  
  
  # Calculate summary statistics
  sts <- round(summary(sprop[[2]])[c(1, 3, 4, 6)], 3)
  
  png(filename = paste0("../output/plots/stats_plots/hist_boxplot_original_data/boxplot_", sprop_name, ".png"),
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

#########################################
### Stack the histograms
#########################################
# library(magick)
# # List png files in the directory
# # image_files <- list.files("../output/plots/stats_plots/hist_to_stack", pattern = "\\.png$", full.names = TRUE)
# image_files <- list.files("../output/plots/stats_plots", pattern = "^hist.*\\.png$", full.names = TRUE)
# 
# # read the images
# images <- image_read(image_files)
# 
# # Stacking the images vertically
# stacked_image <- image_append(images, stack = TRUE)
# 
# # Save the result
# image_write(stacked_image, "../output/plots/stats_plots/hist_to_stack/stack_hist.png")
# # image_write(stacked_image, "../output/plots/stats_plots/stack_hist.png")
# 
# # Load the libraries
# library(png)
# library(grid)
# library(gridExtra)
# 
# # Define the file paths for the PNG images
# image_files <- list.files("../output/plots/stats_plots", pattern = "^hist.*\\.png$", full.names = TRUE)
# 
# # Read the images and convert them to raster objects
# image_rasters <- lapply(image_files, function(file) {
#   img <- readPNG(file)
#   rasterGrob(img, interpolate = TRUE)
# })
# 
# # Arrange the images in a 4x4 grid
# png(filename = paste0("../output/plots/stats_plots/hist_to_stack/4x4_hist.png"),
#     width = 40, height = 40, units = "cm", pointsize = 12,
#     bg = "transparent", res = 300)
# grid.arrange(grobs = image_rasters, ncol = 4, nrow = 4, 
#              # top = -8, bottom = -8, left = NULL, right = NULL,  # Remove external margins
#              # padding = 0.1  # Adjust the space between images
# )
# dev.off()

# if (!require("cowplot")) install.packages("cowplot")
# png(filename = paste0("../output/plots/stats_plots/4x4_hist.png"),
#     width = 40, height = 40, units = "cm", pointsize = 12,
#     bg = "transparent", res = 300)
# plot_grid(
#   plotlist = image_rasters,
#   ncol = 4, nrow = 4,
#   align = "v",  # Alinha horizontal e verticalmente
#   # axis = "tblr", # Ajusta os eixos
#   rel_heights = rep(0.01, 4)#,  # Altura relativa das linhas
#   # rel_widths = rep(-10, 4),   # Largura relativa das colunas
#   # label_size = 0,           # Remove rótulos
#   # hjust = 0, vjust = -10#,     # Ajusta alinhamento
#   # scale = 0.95              # Reduz o tamanho das imagens para diminuir o espaço
# )
# dev.off()

################################################################################
### Plot spatial distribution
################################################################################
library(terra)
nam <- terra::vect("../input/National_boundary.shp")

df[1:10, ]
df[1:10, c(7:8, 10:25)] # x, y, properties
df |> names()

# Plot one
points_vect <- vect(df, geom = c("x", "y"), crs = "EPSG:4326")
terra::plot(nam, border = "grey30", axes =F, lwd = 1)
terra::plot(points_vect, col = "blue", add = TRUE)

# Plot all in a loop
for (i in 10:25) {
  sprop_name <- names(df)[i]
  sprop <- df[, c(7:8, i)] # x, y, property
  sprop <- drop_na(sprop)
  
  png(filename = paste0("../output/plots/spatial_distrib_shape/spat_dist_", sprop_name, "_v3.png"), 
      width = 10, height = 14, units = "cm", pointsize = 12,
      bg = "transparent", res = 300)
  
  points_vect <- vect(sprop, geom = c("x", "y"), crs = "EPSG:4326")
  
  terra::plot(nam, border = "grey30", axes =F, lwd = 0.8, main = sprop_name)
  terra::plot(points_vect, col = "blue", add = T, pch= 21, cex = 0.4)
  dev.off()
}

################################################################################
### Remove outliers (based on the next step, boxplots)
################################################################################
"
Coments on removing outliers:
- Before applying splines:
  - For both EL25 and ELCO, filter out > 15000 BEFORE applying splines, but please check values afterwards. 
    Current spline values for EL25 goes only to max 2290 (about 117 records in original data higher than this), 
    and ELCO only goes to 2896 (216 original records higher than this).
  - For P, filter out all 0s (GIZ... project samples; they should have been NA, not 0) and > 50 BEFORE applying splines. 
    Please check values afterwards, as original data has some values above 20, but splines don't.
  - Please remove Ca > 10000; Mg > 1000; K > 1000; Na > 2000 BEFORE applying splines.
  - For Total N, filter out >= 3000 BEFORE applying splines.

- After applying splines:
  - For pH (water), please filter out values <= 4, or >=11 AFTER creating the splines. 
  - and change 0s in Ca, Mg, K, Na of record OKA0013 to NA
  - For CEC, filter out all values higher than 100 BEFORE AND AFTER applying splines
"

options(max.print = 2500) # increase the print limit  

# Checks for P (in GIZ project)
df[grepl("GIZ", df$Soil.ID), ] # Select only the GIZ project samples
df[grepl("GIZ", df$Soil.ID), "Soil.ID"] # check if all are GIZ only (with Soil.ID column)
df[grepl("GIZ", df$Soil.ID), "P.mg.kg"] # check if all are GIZ in P.mg.kg column
df[grepl("GIZ", df$Soil.ID) & df$P.mg.kg == 0, c(1:8, 24)] # test the filtering (show more cols) (130 lines)
# Filter out the 0s in P.mg.kg
df[grepl("GIZ", df$Soil.ID) & df$P.mg.kg == 0, "P.mg.kg"] # test the filtering (with P.mg.kg col only)
# df[grepl("GIZ", df$Soil.ID) & df$P.mg.kg == 0, "P.mg.kg"] <- NA # Set them to NA
# due the NA, the operation cannot be done, thus we have to 'leave' the NA out of selection to set NA
df[grepl("GIZ", df$Soil.ID) & df$P.mg.kg == 0 & !is.na(df$P.mg.kg), c(1:8, 24)]
df[grepl("GIZ", df$Soil.ID) & df$P.mg.kg == 0 & !is.na(df$P.mg.kg), "P.mg.kg"]
df[grepl("GIZ", df$Soil.ID) & df$P.mg.kg == 0 & !is.na(df$P.mg.kg), "P.mg.kg"] <- NA
# check the changes
df[grepl("GIZ", df$Soil.ID), c(1:8, 24)] 

# Filter out the > 50 in P.mg.kg
df[which(df$P.mg.kg >= 50), ] 
df[which(df$P.mg.kg >= 50), "P.mg.kg"] 
df[which(df$P.mg.kg >= 50), "P.mg.kg"] <- NA 

# Set the 0s in Ca, Mg, K, Na of record OKA0013 to NA
df[grepl("OKA-0013", df$Soil.ID), ] # grep the OKA-0013 profile
df[grepl("OKA-0013", df$Soil.ID), c("Ca.mg.kg", "Mg.mg.kg", "K.mg.kg", "Na.mg.kg")] # grep the OKA-0013 profile and the 
df[grepl("OKA-0013", df$Soil.ID) & df$Ca.mg.kg == 0 & !is.na(df$Ca.mg.kg), "Ca.mg.kg"] # grep the OKA-0013 profile and the Ca.mg.kg == 0
df[grepl("OKA-0013", df$Soil.ID) & df$Ca.mg.kg == 0 & !is.na(df$Ca.mg.kg), "Ca.mg.kg"] <- NA # Set to 0
df[grepl("OKA-0013", df$Soil.ID) & df$Na.mg.kg == 0 & !is.na(df$Na.mg.kg), "Na.mg.kg"] # grep the OKA-0013 profile and the Na.mg.kg == 0
df[grepl("OKA-0013", df$Soil.ID) & df$Na.mg.kg == 0 & !is.na(df$Na.mg.kg), "Na.mg.kg"] <- NA # Set to 0
df[grepl("OKA-0013", df$Soil.ID), ] # Check the changes

# Filter out the other properties
df[which(df$EC.uS.cm.EL25 >= 15000), ] # check the row
df[which(df$EC.uS.cm.EL25 >= 15000), "EC.uS.cm.EL25"] # check the value
df[which(df$EC.uS.cm.EL25 >= 15000), "EC.uS.cm.EL25"] <- NA # drop the value

df[which(df$EC.uS.cm.ELCO >= 15000), ] 
df[which(df$EC.uS.cm.ELCO >= 15000), "EC.uS.cm.ELCO"] 
df[which(df$EC.uS.cm.ELCO >= 15000), "EC.uS.cm.ELCO"] <- NA 

df[which(df$Ca.mg.kg >= 10000), ] 
df[which(df$Ca.mg.kg >= 10000), "Ca.mg.kg"] 
df[which(df$Ca.mg.kg >= 10000), "Ca.mg.kg"] <- NA 

df[which(df$Mg.mg.kg >= 1000), ] 
df[which(df$Mg.mg.kg >= 1000), "Mg.mg.kg"] 
df[which(df$Mg.mg.kg >= 1000), "Mg.mg.kg"] <- NA 

df[which(df$K.mg.kg >= 1000), ] 
df[which(df$K.mg.kg >= 1000), "K.mg.kg"] 
df[which(df$K.mg.kg >= 1000), "K.mg.kg"] <- NA 

df[which(df$N.mg.kg >= 3000), ] 
df[which(df$N.mg.kg >= 3000), "N.mg.kg"] 
df[which(df$N.mg.kg >= 3000), "N.mg.kg"] <- NA 

df[which(df$Na.mg.kg >= 2000), ] 
df[which(df$Na.mg.kg >= 2000), "Na.mg.kg"] 
df[which(df$Na.mg.kg >= 2000), "Na.mg.kg"] <- NA 

df[which(df$CEC.cmol.kg >= 100), ] 
df[which(df$CEC.cmol.kg >= 100), "CEC.cmol.kg"] 
df[which(df$CEC.cmol.kg >= 100), "CEC.cmol.kg"] <- NA 

df[which(df$OC.pct >= 5.5), ] 
df[which(df$OC.pct >= 5.5), "OC.pct"] 
df[which(df$OC.pct >= 5.5), "OC.pct"] <- NA 

# Typo
df[which(df$Upper.Depth >= 700), ] 
df[which(df$Upper.Depth >= 700), "Upper.Depth"] 
df[which(df$Upper.Depth >= 700), "Upper.Depth"] <- 0

max(df$OC.pct, na.rm = TRUE) 
# hist(df$OC.pct) 

################################################################################
### Reorder rows as depth increase
################################################################################
library(dplyr) # for arrange() 
library(ithir)
# Demosntration and check
# data(oneProfile)
# op <- rbind(oneProfile[1:3, ], oneProfile[3:1, ]) |> as.data.frame()
# op$Soil.ID <- c("A", "A", "A", "B", "B", "B")
# op$HorizID <- c(1, 2, 3, 1, 2, 3)
# op |> dplyr::arrange(Soil.ID, Upper.Boundary, Lower.Boundary)
# op |>
#  group_by(Soil.ID) |>
#  mutate(horizon_order = row_number()) |>
#  arrange(Upper.Boundary, Lower.Boundary, .by_group = TRUE) |>
#  ungroup()
# 
# # See tests
# ithir::ea_spline(obj = oneProfile[1:3, ], var.name="C.kg.m3.", d = c(0, 5, 15, 30, 60, 100))$harmonised
# ithir::ea_spline(obj = oneProfile[3:1, ], var.name="C.kg.m3.", d = c(0, 5, 15, 30, 60, 100))$harmonised
# 
# # the vhigh can corce the original data values, so the limits must be set close to the original data values
# ithir::ea_spline(obj = oneProfile, vhigh = 10, var.name="C.kg.m3.", d = c(0, 5, 15, 30, 60, 100))$harmonised

# Implement the reordering
df <- df |> dplyr::arrange(Soil.ID, Upper.Depth, Lower.Depth)

################################################################################
# Function to interpolate missing data in a vector using spline
# spline_interpolation <- function(vec) {
#   # Perform spline interpolation
#   result <- stats::spline(
#     x = seq_along(vec)[!is.na(vec)],  # Indices of non-NA values
#     y = vec[!is.na(vec)],             # Non-NA values
#     xout = seq_along(vec),            # Indices for all values
#     method = "fmm"                    # Spline method
#   )$y                                 # Capture the y (interpolated) values
#   # Return the interpolated result
#   return(result)
# }
# spline_interpolation(c(NA, 0.366, 0.488, 0.180, NA, 0.162, 0.104, 0.110, 0.034, NA))
# spline_interpolation(c(6, NA, NA, 0.3, 1.2))
# spline_interpolation(c(0.3, 1.2, NA, NA, NA))

spline_interpolation <- function(vec) {
  # Perform spline interpolation
  result <- stats::spline(
    x = seq_along(vec)[!is.na(vec)],  # Indices of non-NA values
    y = vec[!is.na(vec)],             # Non-NA values
    xout = seq_along(vec),            # Indices for all values
    method = "natural"
  )$y                                 # Capture the y (interpolated) values
  # Return the interpolated result
  return(result)
}

# quick test
spline_interpolation(c(6, NA, NA, 0.3, 1.2))
spline_interpolation(c(0.3, 1.2, NA, NA, NA))
spline_interpolation(c(0.3, 1.2, NA, NA, 6))
spline_interpolation(c(2, NA, NA, NA, NA))

### Function to get the outliers limits across all groups per column, in other words soil profiles outlier limits at certain property
### Source: https://www.r-bloggers.com/2020/01/how-to-remove-outliers-in-r/
### https://stackoverflow.com/questions/4787332/how-to-remove-outliers-from-a-dataset

# Function identify the outliers bounderies
outliers_lims_df <- function(df, columns) {
  # Check if all columns exist in the dataframe
  if (!all(columns %in% names(df))) {
    stop("One or more specified columns do not exist in the dataframe.")
  }
  
  # Function to calculate the limits of outliers
  outlier_limits <- function(x) {
    qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
    iqr <- qnt[2] - qnt[1]
    lower_limit <- qnt[1] - (1.5 * iqr)
    upper_limit <- qnt[2] + (1.5 * iqr)
    return(c(lower_limit, upper_limit))
  }
  
  # Initialize lists to store the results
  lower_limits <- numeric(length(columns))
  upper_limits <- numeric(length(columns))
  
  # Calculate and get the limits for each soil property (column)
  for (i in seq_along(columns)) {
    limits <- outlier_limits(df[[columns[i]]])
    lower_limits[i] <- limits[1]
    upper_limits[i] <- limits[2]
  }
  
  # Create a dataframe with the results from soil properties (columns)
  result_df <- data.frame(
    soil_property = columns,
    lower_limit = lower_limits,
    upper_limit = upper_limits
  )
  
  # Return the dataframe with the results
  return(result_df)
}

# quick test
dd <- data.frame(x = c(0, 10, 18, 32, 45, 60, 130), 
                 y = c(-55, 10, 180, 32, 45, 60, 130))
limits_df <- outliers_lims_df(dd, c("x", "y"))
limits_df

################################################################################
### Outliers limits
df |> names() |> dput()

limits_df <- outliers_lims_df(df, c("Upper.Depth", "Lower.Depth", "Sand.53.um.pct", "Silt.2.53um.pct", 
                                     "Clay.2.um.pct", "BD.kg.dm3", "pH.water", "EC.uS.cm.EL25", 
                                     "EC.uS.cm.ELCO", "Ca.mg.kg", "Mg.mg.kg", "K.mg.kg", "Na.mg.kg", 
                                     "CEC.cmol.kg", "BS.pct", "N.mg.kg", "P.mg.kg", "OC.pct"))
limits_df

# Adjust the limits manually
# In "lower_limit", no one can be negative
limits_df$lower_limit[limits_df$lower_limit < 0] <- 0

# # In "lower_limit", Manual adjustments
limits_df$lower_limit[limits_df$soil_property == "Lower.Depth"] <- 1
limits_df$lower_limit[limits_df$soil_property == "OC.pct"] <- 0.001
limits_df$lower_limit[limits_df$soil_property == "pH.water"] <- 4

# # In "upper_limit", Manual adjustments
limits_df$upper_limit[limits_df$soil_property == "Sand.53.um.pct"] <- 100
limits_df$upper_limit[limits_df$soil_property == "BS.pct"] <- 100
limits_df$upper_limit[limits_df$soil_property == "OC.pct"] <- 5
limits_df$upper_limit[limits_df$soil_property == "pH.water"] <- 11

limits_df

# boxplot(df$pH.water)
# hist(df$pH.water)
# min(df$Sand.53.um.pct, na.rm=T)
# hist(df$Sand.53.um.pct)
# boxplot(df$Sand.53.um.pct)

################################################################################
### Missing data interpolation 
################################################################################

# Define the replacement strategy
# replace_negative_with <- "NA" # or:
replace_negative_with <- "zero"

# Indicate the columns to be interpolated
interp_columns <- c("Upper.Depth", "Lower.Depth", "Sand.53.um.pct", "Silt.2.53um.pct", 
                    "Clay.2.um.pct", "BD.kg.dm3", "pH.water", "EC.uS.cm.EL25", 
                    "EC.uS.cm.ELCO", "Ca.mg.kg", "Mg.mg.kg", "K.mg.kg", "Na.mg.kg", 
                    "CEC.cmol.kg", "BS.pct", "N.mg.kg", "P.mg.kg", "OC.pct")


# Get the indexes of the columns
indices <- which(names(df) %in% interp_columns)

# Create an empty dataframe with same names to receive the results
result <- df[0, ]

# Gather the profiles ID to iterate over it
profiles <- unique(df$Soil.ID)

################################################################################
### Test with a single profile
# Define the profiles to test
# profiles <- c("AMP-NA-0003", "AO-0010", "KB-0028", "KH-0036", "LDN-Okaksc-0016", 
#               "SASSCAL-I-P2503", "GIZ-Bush-SOC-140", "DAM-0017", "DAM-0019", "NH-0028")
# 
# df |> dplyr::filter(Soil.ID == "AMP-NA-0003")
# df |> dplyr::filter(Soil.ID == "AO-0010")
# df |> dplyr::filter(Soil.ID == "KB-0028")
# df |> dplyr::filter(Soil.ID == "KH-0036")
# df |> dplyr::filter(Soil.ID == "GIZ-Bush-SOC-140")
# df |> dplyr::filter(Soil.ID == "DAM-0017")
# df |> dplyr::filter(Soil.ID == "DAM-0019")
# df |> dplyr::filter(Soil.ID == "LDN-Okaksc-0016")
# 
# df |> dplyr::filter(Soil.ID == "SASSCAL-I-P2503")
# df |> dplyr::filter(Soil.ID == "NH-0028")
# 
# prof <- "LDN-Okaksc-0016"
# prof <- "SASSCAL-I-P2503"
# prof <- "NH-0028"
# 
# indices
# i <- 25 # OC
# i <- 13 # BD
### end test
################################################################################

# Iterate over each profile
system.time({
for (prof in profiles) {
  dfp <- df |> dplyr::filter(Soil.ID == prof)
  
  ### Loop status, print the profile being processed
  cat("Processing profile: ", prof, "\n", 
      which(profiles == prof), " of ", length(profiles), "\n")
  
  # Iterate over each column indexes
  for (i in indices) {
    # define lower and upper limits
    ll <- limits_df |> dplyr::filter(soil_property == names(dfp)[i]) |> dplyr::pull(lower_limit)
    ul <- limits_df |> dplyr::filter(soil_property == names(dfp)[i]) |> dplyr::pull(upper_limit)
    na_indexes <- seq_along(dfp[[i]])[is.na(dfp[[i]])] # to avoid rewrite over original values
    
    # Check if all values in the vector are NA or non-NA
    if (all(is.na(dfp[i])) | all(!is.na(dfp[i]))) {
      # Assign the original vector to the output
      dfp[i] <- dfp[i]
    } else {
      # Perform spline interpolation on the vector
      vect <- spline_interpolation(dfp[[i]])
      vect <- round(vect, 3)
      
      # Check if any interpolated values are negative
      if (any(vect < 0)) {
        # Replace negative values based on the replace_negative_with variable
        if (replace_negative_with == "zero") {
          vect[vect < 0] <- 0
        } else {
          if (replace_negative_with == "NA") {
            vect[vect < 0] <- NA
          }
        }
      } else {
        # Check and replace values lower than ll
        if (any(vect[na_indexes] < ll)) {
          vect[na_indexes][vect[na_indexes] < ll] <- round(ll, 3)
        }
        # Check and replace values higther than ul
        if (any(vect[na_indexes] > ul)) {
          vect[na_indexes][vect[na_indexes] > ul] <- round(ul, 3)
        }
      }
      # Assign the processed vector to the output
      dfp[[i]] <- vect
    }
  }
  result <- rbind(result, dfp)
}
})[3] # 4 min
# Return the output
result

table(is.na(df)) # 146239 NA, update 146250
table(is.na(result)) # 143226 NA, update 143228
# 146239 - 143226
# 3013 observations were interpolated (inputed)
# 146250 - 143228 # update
# update 3022
# 179109 - 170477 # update
# update 8632 # Feb 2025 
179245 - 170613 # update
# update 8632 # Mar 2025 (same number of interpolations as Feb, even with changes in the input due outliers removed)

# Write on disc
write.csv(result, "../output/datasets/dataset_for_splines/Interpolated_NA_in_Dataset_for_splines_2024_09_09.csv", row.names = F)

################################################################################
### Change the limits for spline depth interpolation
################################################################################
"
About the limits for missing data and spline depths interpolation:

The limits inside spline function can corce the opriginal data values, 
so the limits for spline must be set close to the original data values.

Which is a different case of missing data interpolation, where the limits
are set to avoid extrapolation.
"

data.frame(min_original_data= sapply(df, min, na.rm = TRUE), 
           max_original_data= sapply(df, max, na.rm = TRUE))

limits_df
# In "lower_limit", Manual adjustments
limits_df$lower_limit[limits_df$soil_property == "Upper.Depth"] <- 0
limits_df$lower_limit[limits_df$soil_property == "Lower.Depth"] <- 1
limits_df$lower_limit[limits_df$soil_property == "Sand.53.um.pct"] <- 2.1
limits_df$lower_limit[limits_df$soil_property == "Silt.2.53um.pct"] <- 0
limits_df$lower_limit[limits_df$soil_property == "Clay.2.um.pct"] <- 0
limits_df$lower_limit[limits_df$soil_property == "BD.kg.dm3"] <- 0.421
limits_df$lower_limit[limits_df$soil_property == "pH.water"] <- 4
limits_df$lower_limit[limits_df$soil_property == "EC.uS.cm.EL25"] <- 2
limits_df$lower_limit[limits_df$soil_property == "EC.uS.cm.ELCO"] <- 2
limits_df$lower_limit[limits_df$soil_property == "Ca.mg.kg"] <- 1
limits_df$lower_limit[limits_df$soil_property == "Mg.mg.kg"] <- 0
limits_df$lower_limit[limits_df$soil_property == "K.mg.kg"] <- 0
limits_df$lower_limit[limits_df$soil_property == "Na.mg.kg"] <- 0
limits_df$lower_limit[limits_df$soil_property == "CEC.cmol.kg"] <- 0.04
limits_df$lower_limit[limits_df$soil_property == "BS.pct"] <- 1.17
limits_df$lower_limit[limits_df$soil_property == "N.mg.kg"] <- 10
limits_df$lower_limit[limits_df$soil_property == "P.mg.kg"] <- 0.01
limits_df$lower_limit[limits_df$soil_property == "OC.pct"] <- 0.001

# In "upper_limit", Manual adjustments
limits_df$upper_limit[limits_df$soil_property == "Upper.Depth"] <- 100
limits_df$upper_limit[limits_df$soil_property == "Lower.Depth"] <- 700
limits_df$upper_limit[limits_df$soil_property == "Sand.53.um.pct"] <- 100
limits_df$upper_limit[limits_df$soil_property == "Silt.2.53um.pct"] <- 74.3
limits_df$upper_limit[limits_df$soil_property == "Clay.2.um.pct"] <- 79
limits_df$upper_limit[limits_df$soil_property == "BD.kg.dm3"] <- 2.208
limits_df$upper_limit[limits_df$soil_property == "pH.water"] <- 11
limits_df$upper_limit[limits_df$soil_property == "EC.uS.cm.EL25"] <- 14230
limits_df$upper_limit[limits_df$soil_property == "EC.uS.cm.ELCO"] <- 13860
limits_df$upper_limit[limits_df$soil_property == "Ca.mg.kg"] <- 9815.93
limits_df$upper_limit[limits_df$soil_property == "Mg.mg.kg"] <- 771
limits_df$upper_limit[limits_df$soil_property == "K.mg.kg"] <- 947
limits_df$upper_limit[limits_df$soil_property == "Na.mg.kg"] <- 1684
limits_df$upper_limit[limits_df$soil_property == "CEC.cmol.kg"] <- 54.08
limits_df$upper_limit[limits_df$soil_property == "BS.pct"] <- 100
limits_df$upper_limit[limits_df$soil_property == "N.mg.kg"] <- 2800
limits_df$upper_limit[limits_df$soil_property == "P.mg.kg"] <- 49
limits_df$upper_limit[limits_df$soil_property == "OC.pct"] <- 4.2
limits_df

################################################################################
### The spline depth interpolation
################################################################################

# Mode function, source: https://stackoverflow.com/a/25635740/14361772
mode.vect <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

# quick test
mode.vect(c("p3","p3","p3","p4",NA))
mode.vect(c("p5","p5","p2","p2")) # get the first mode


library(dplyr)
library(ithir)

# Reuse the object result as df
df <- result

table(is.na(df$x)) 
table(is.na(df$y)) 

################################################################################
# User definitions
################################################################################
# Indicate the depths interpolated
depths_ <- c(0,5,15,30,60,100,200)

# Indicate the columns to be depths interpolated
depths_interp_columns <- c("Sand.53.um.pct", "Silt.2.53um.pct", 
                           "Clay.2.um.pct", "BD.kg.dm3", "pH.water", "EC.uS.cm.EL25", 
                           "EC.uS.cm.ELCO", "Ca.mg.kg", "Mg.mg.kg", "K.mg.kg", "Na.mg.kg", 
                           "CEC.cmol.kg", "BS.pct", "N.mg.kg", "P.mg.kg", "OC.pct")

################################################################################
### Gather the profiles ID to iterate over it
################################################################################
profiles <- unique(df$Soil.ID)

################################################################################
### Test with a single profile
# prof <- "SASSCAL-I-P2503"
# prof <- c("SASSCAL-I-P2503", "LDN-Okaksc-0016")
# 
# indices
# i <- 25 # OC
# i <- 13 # BD
# 
# #### splines
# # install.packages("devtools")
# # library(devtools)
# # install_github("brendo1001/ithir_github/pkg") #ithir package # https://smartdigiag.com/DSM_book/pages/ithir/
# library(ithir)
# data("oneProfile")
# # test with shorther profile
# ithir::ea_spline(obj = oneProfile[1:3, ], var.name="C.kg.m3.", d = c(0, 5, 15, 30, 60, 100))$harmonised
# ithir::ea_spline(obj = oneProfile[1, ], var.name="C.kg.m3.", d = c(0, 30))$harmonised
# ithir::ea_spline(obj = oneProfile[1:2, ], var.name="C.kg.m3.", d = c(0, 30))$harmonised
# ithir::ea_spline(obj = oneProfile[3:1, ], var.name="C.kg.m3.", d = c(0, 5, 15, 30, 60, 100))$harmonised
### end test
################################################################################

################################################################################
### Preparing the output dataframe to be appended with the results
################################################################################
# Get the indexes of the columns
indices <- which(names(df) %in% depths_interp_columns)

# Create an empty dataframe with same names to receive the results
column_names <- c()
for (col in depths_interp_columns) {
  for (i in 1:(length(depths_) - 1)) {
    column_name <- paste(col, depths_[i], depths_[i + 1], "cm", sep = "_")
    column_names <- c(column_names, column_name)
  }
}

# Create an empty dataframe to receive the results
ea_spl_result <- data.frame(matrix(ncol = length(column_names) + 3, nrow = length(profiles)))
# Rename the columns
colnames(ea_spl_result) <- c("Soil.ID", column_names, "x", "y")
# Set ID column as character
ea_spl_result$Soil.ID <- as.character(ea_spl_result$Soil.ID)
# Set columns from the second onward to numeric (this is necessary to merge (append) the results)
ea_spl_result[, 2:ncol(ea_spl_result)] <- sapply(ea_spl_result[, 2:ncol(ea_spl_result)], as.numeric)
# str(ea_spl_result)

# Set Soil.ID 
ea_spl_result$Soil.ID <- as.character(profiles)

################################################################################
### Iterate over each profile
################################################################################
system.time({
for (prof in profiles) {
  dfp <- df |> dplyr::filter(Soil.ID == prof)
  
  ### Loop status, print the profile being processed
  cat("Processing profile: ", prof, "\n", 
  which(profiles == prof), " of ", length(profiles), "\n")
    
  ################################################################################
  ### Iterate over each column set
  ################################################################################
  for (i in indices) {

    # define upper limit
    ll <- limits_df |> dplyr::filter(soil_property == names(dfp)[i]) |> dplyr::pull(lower_limit)
    ul <- limits_df |> dplyr::filter(soil_property == names(dfp)[i]) |> dplyr::pull(upper_limit)
    
    ### === The following pattern is necessary to match the columns to be selected to receive the results ===
    # Call the pattern
    pattern_ <- paste0("^", names(dfp)[i], "_")
    # Detect the columns that match the pattern
    dfp_col_names <- names(ea_spl_result)[grep(pattern_, names(ea_spl_result))]
    
    # Check if any values in the property vector are NA
    # Also in the depths columns (2 and 3)
    if (any(is.na(dfp[i]) | is.na(dfp[2]) | is.na(dfp[3]))) {
      # Assign a df with NA to this case
      na_result <- data.frame(matrix(NA, ncol = length(dfp_col_names), nrow = 1))
      colnames(na_result) <- dfp_col_names
      ea_spl_result[ea_spl_result$Soil.ID == mode.vect(dfp$Soil.ID), dfp_col_names] <- na_result
      
    } else {
      # clean the previous results
      if (exists("sp.fit")) { # since in the first loop did no exists, this if is useful 
        rm(sp.fit)
      }
      # Perform spline interpolation on the vector
      sp.fit <- ithir::ea_spline(dfp, 
                                 var.name= names(dfp)[i],
                                 lam = 0.1, 
                                 d = depths_, 
                                 vlow = ll, 
                                 vhigh = ul,
                                 show.progress= T)
      
      # Drop the "soil depth" column (not necessary and would be necessary to manage it)
      sp.fit$harmonised$`soil depth` <- NULL
      # Append the results in the result dataframe using the colnems as reference
      ea_spl_result[ea_spl_result$Soil.ID == sp.fit$harmonised$id, dfp_col_names] <- sp.fit$harmonised |> dplyr::select(-id) |> round(3)
    }
  }
  # Add the coordinates
  coords <- c(x= mode.vect(dfp[["x"]]), y= mode.vect(dfp[["y"]]))
  ea_spl_result[ea_spl_result$Soil.ID == mode.vect(dfp[["Soil.ID"]]), c("x", "y")] <- coords 
}
})[3] # 5 min
# Call the results
ea_spl_result

################################################################################
### Rectify values lower than zero
# Count the values lower than zero
sum(sapply(ea_spl_result, function(x) sum(x == -9999.000, na.rm = TRUE))) # has zero 
# Replace -9999.000 with NA
ea_spl_result[ea_spl_result == -9999.000] <- NA

# Replace other spurious values with NA 
# Identifying columns that end with '_cm'
cm_columns <- grep("_cm$", names(ea_spl_result))

# Count the values lower than zero
sum(sapply(ea_spl_result, function(x) sum(x < 0, na.rm = TRUE))) # has zero 
# Replace negative values with zero in these columns
ea_spl_result[cm_columns] <- lapply(ea_spl_result[cm_columns], function(x) replace(x, x < 0, 0))

ea_spl_result
# table(is.na(ea_spl_result))

################################################################################
### Check interpolated outside limits
options(max.print = 2500) # increase the print limit  
ea_spl_result[which(ea_spl_result$pH.water_100_200_cm < 4), ]

#####  #####  #####   #####   #####   #####   #####   #####   #####   ####
# Join the results with the Soil types
### Test join with small example
df1 <- df[150:165, c("Soil.ID", "WRB2015")]
df2 <- ea_spl_result[, 1:4]
inner_join(df2, df1, by = c("Soil.ID"), 
           multiple = "last",
           keep = F) |> as.data.frame()

### Join the results with the Soil types
ea_spl_result_soil_type <- inner_join(ea_spl_result, df[ , c("Soil.ID", "WRB2015")], 
                                      by = c("Soil.ID"), 
                                      multiple = "last",
                                      keep = F) |> as.data.frame()
### Rearrange the columns
ea_spl_result_soil_type <- ea_spl_result_soil_type |>
  relocate(c(WRB2015, x, y), .after = Soil.ID)

### Write on disc
write.csv(ea_spl_result_soil_type, 
          "../output/datasets/splines_soil_preperties_to_model_gee/Namibia_ea_spline_all_properties_2025_03_03_depths_0_5_15_30_60_100_200.csv", 
          row.names = F)

write.csv(ea_spl_result_soil_type, 
          "../output/datasets/splines_soil_preperties_to_model_gee/Namibia_ea_spline_all_properties_2025_03_03_null_depths_0_5_15_30_60_100_200.csv", 
          na = "", ### Important for gee
          row.names = F)

### Check max and min values for outliers and wrong extrapolation
sapply(df, max, na.rm = TRUE)
sapply(df, min, na.rm = TRUE)

sapply(ea_spl_result_soil_type, max, na.rm = TRUE)
sapply(ea_spl_result_soil_type, min, na.rm = TRUE)

################################################################################
### Export interpolated depths (for cluster analysis)
################################################################################
########### Export each soil property as .csv and .xlsx in loop ############
sdb_spl <- ea_spl_result_soil_type
sdb_spl |> names()
sdb_spl[5:ncol(sdb_spl)] |> names()
cols_export <- sdb_spl[5:ncol(sdb_spl)] |> names()

sdb_spl[7] |> head()
sdb_spl[cols_export[1]] |> head()
# i="OC__0-30 cm" # to debug with zeros

for (i in cols_export) {
  ### Loop status, print the profile being processed
  cat("Processing column: ", i, "\n", 
      which(cols_export == i), " of ", length(cols_export), "\n")
  
  # Select the columns
  soil_prop_coords <- sdb_spl |> 
    # dplyr::select(c(Soil.ID, Latitude, Longitude, all_of(i)))
    dplyr::select(c(x, y, all_of(i)))
  soil_prop_coords_exp <- tidyr::drop_na(soil_prop_coords)
  
  # Reorder columns
  soil_prop_coords_exp <- soil_prop_coords_exp[, c(i, "x", "y")]
  # Export
  # Create paths
  export_path_csv <- paste0("../output/datasets/splines_soil_preperties_to_model_gee/by_property_individual_files/", 
                            "Nam_spl_", i, ".csv")
  export_path_xls <- gsub(".csv", ".xlsx", export_path_csv) # change extension

  # csv
  write.csv(
    soil_prop_coords_exp,
    file = export_path_csv,
    row.names = FALSE
  )
  
  # xlsx
  # write_xlsx(
  #   soil_prop_coords_exp,
  #   path = export_path_xls,
  #   col_names = TRUE,
  #   format_headers = TRUE#,
  #   # use_zip64 = FALSE
  # )
}


#### Count the number of profiles with soil classification
cnt <- list()
cnt_df <- data.frame(soil_class = character(), soil_prop = numeric())

sdb_spl[1:10, 1:10]

for (i in cols_export) {
  ### Loop status, print the profile being processed
  cat("Processing column: ", i, "\n", 
      which(cols_export == i), " of ", length(cols_export), "\n")
  
  # Select the columns
  soil_prop_coords <- sdb_spl |> 
    # dplyr::select(c(Soil.ID, Latitude, Longitude, all_of(i)))
    dplyr::select(c(Soil.ID, WRB2015, x, y, all_of(i)))
  soil_prop_coords_exp <- tidyr::drop_na(soil_prop_coords)
  
  cnt_df$soil_class <- unique(soil_prop_coords_exp$WRB2015)
  cnt_df$soil_prop <- length(unique(soil_prop_coords_exp$Soil.ID))
  # Reorder columns
  # soil_prop_coords_exp <- soil_prop_coords_exp[, c(i, "x", "y")]
  # Export
  # Create paths
  # export_path_csv <- paste0("../output/datasets/splines_soil_preperties_to_model_gee/by_property_individual_files/", 
  #                           "Nam_spl_", i, ".csv")
  # export_path_xls <- gsub(".csv", ".xlsx", export_path_csv) # change extension
  
  # csv
  # write.csv(
  #   soil_prop_coords_exp,
  #   file = export_path_csv,
  #   row.names = FALSE
  # )
}

# Count non-missing data for each WRB2015
wxspxdp <- sdb_spl %>%
  group_by(WRB2015) %>%
  summarise(across(everything(), ~ sum(!is.na(.)), .names = "non_missing_{col}")) %>%
  pivot_longer(cols = starts_with("non_missing_"), names_to = "Column", values_to = "Count") %>%
  filter(Count > 0) %>%
  mutate(Column = sub("non_missing_", "", Column))
write.csv(wxspxdp, "../output/datasets/splines_soil_preperties_to_model_gee/metrics_report/count_WRBSRG_x_Soil_prop_x_depth.csv", row.names = T)

library(dplyr)
library(stringr)
# Filter the depths
wxspxdp_filt <- wxspxdp %>%
  filter(str_detect(Column, "0_5_cm|5_15_cm|15_30_cm"))
write.csv(wxspxdp_filt, "../output/datasets/splines_soil_preperties_to_model_gee/metrics_report/count_WRBSRG_x_Soil_prop_x_depth_30cm.csv", row.names = T)
wxspxdp_filt |> as.data.frame()

################################################################################
### Report
################################################################################
dim(sdb)
unique(sdb$Soil.ID) |> length()
"
3013 observations (horizons) were interpolated (inputed), update 3022


Number of soil horizons in the original database: 9789
Number of soil profiles in the original database: 4960

Number of soil profiles with soil classification: 2866


"
table(is.na(ea_spl_result_soil_type))
non_na_count <- sapply(ea_spl_result_soil_type, function(x) sum(!is.na(x)))
non_na_count <- as.data.frame(non_na_count)
non_na_count$percent_complete <- round(non_na_count$non_na_count / nrow(ea_spl_result_soil_type) * 100, 2)

write.csv(non_na_count, "../output/datasets/splines_soil_preperties_to_model_gee/metrics_report/Non_NA_count.csv", row.names = T)

# Create the plot of horiz bars
nac_df <- non_na_count
nac_df$variable <- rownames(nac_df)

library(ggplot2)

# with rank
ggplot(nac_df, aes(x = reorder(variable, non_na_count), y = non_na_count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Non-NA Count per soil propery",
       x = "Variable",
       y = "Non-NA Count") +
  theme_minimal() +
  theme(plot.margin = margin(10, 30, 10, 20)) +  # margins: top, right, bottom, left
  # ylim(0, 3000)  # Lim x to 3000
  geom_text(aes(label = paste0(non_na_count, " | ", percent_complete, "%")), 
            hjust = -0.1, size = 3) + # add text
  ylim(0, 5400)  # Lim x to 3000

ggsave("../output/datasets/splines_soil_preperties_to_model_gee/metrics_report/non_na_count_plot_ranked.png", 
       plot = last_plot(), width = 9, height = 18, units = "in", dpi = 300)

# with levels rank
nac_df$variable <- factor(nac_df$variable, levels = rev(unique(nac_df$variable)))


ggplot(nac_df, aes(x = variable, y = non_na_count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Non-NA Count per soil propery",
       x = "Variable",
       y = "Non-NA Count") +
  theme_minimal() +
  theme(plot.margin = margin(10, 30, 10, 10)) +  # margins: top, right, bottom, left
  # ylim(0, 3000)  # Lim x to 3000
  geom_text(aes(label = paste0(non_na_count, " | ", percent_complete, "%")), 
            hjust = -0.1, size = 3) + # add text
  ylim(0, 5400)  # Lim x to 3000
  
ggsave("../output/datasets/splines_soil_preperties_to_model_gee/metrics_report/non_na_count_plot_levels.png", 
       plot = last_plot(), width = 9, height = 18, units = "in", dpi = 300)
       

################################################################################

mxmn1 <- data.frame(min_original_data= sapply(df, min, na.rm = TRUE), 
                    max_original_data= sapply(df, max, na.rm = TRUE))
write.csv(mxmn1, "../output/datasets/splines_soil_preperties_to_model_gee/metrics_report/min_max_original_dataset.csv", row.names = T)

mxmn2 <- data.frame(min_spline_data= sapply(ea_spl_result_soil_type, min, na.rm = TRUE),
                    max_spline_data= sapply(ea_spl_result_soil_type, max, na.rm = TRUE))
write.csv(mxmn2, "../output/datasets/splines_soil_preperties_to_model_gee/metrics_report/min_max_spline_dataset.csv", row.names = T)

################################################################################
### Centroids
################################################################################
dfctd <- ea_spl_result_soil_type
dfctd |> names()
dfctd$Soil.ID <- NULL
dfctd$x <- NULL
dfctd$y <- NULL

library(tidyverse)
library(dplyr)

# Calculate the mean of the sand columns grouped by WRB2015 (centroids) 
# centroids
ctd <- dfctd |>
  group_by(WRB2015) |>
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

# Centroids + count the number of profiles per soil class (across all depths)
# ctd <- dfctd %>%
#   group_by(WRB2015) %>%
#   summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
#             count = n()) %>%
#   as.data.frame()

# Count the number of profiles per soil class (at each depth)
ctd_counts <- dfctd %>% 
  group_by(WRB2015) %>% 
  summarise(across(where(is.numeric), ~ sum(!is.na(.x))))

# Centroids + count the number of profiles per soil class (at each depth)
# ctd_summary <- dfctd %>% 
#   group_by(WRB2015) %>% 
#   summarise(across(
#     where(is.numeric), 
#     list(mean = ~ mean(.x, na.rm = TRUE),
#          count = ~ sum(!is.na(.x))), 
#     .names = "{.col}_{.fn}"
#   ))
# ctd_summary |> as.data.frame()

# Remove rows with NAs
ctd1 <- ctd %>%
  filter(if_any(where(is.numeric), ~ !is.na(.)))

# Remove rows with NAs in WRB2015
ctd2 <- ctd1 %>%
  filter(!is.na(WRB2015)) |> 
  filter(!WRB2015 %in% c("Arenosol / Fluvisol", "Calcisol / Cambisol"))

write.csv(ctd2, "../output/datasets/splines_soil_preperties_to_model_gee/metrics_report/Namibia_centroids.csv", row.names = F)

################################################################################
### Plots centroids splines
################################################################################
library(tidyverse)
library(ggplot2)

### Recall the data
ctd_plot <- ctd2

# Get the unique soil classes
soil_classes <- unique(ctd_plot$WRB2015)

# Soil properties
soil_prop <- c("Sand.53.um.pct", "Silt.2.53um.pct", 
                  "Clay.2.um.pct", "BD.kg.dm3", "pH.water", "EC.uS.cm.EL25", 
                  "EC.uS.cm.ELCO", "Ca.mg.kg", "Mg.mg.kg", "K.mg.kg", "Na.mg.kg", 
                  "CEC.cmol.kg", "BS.pct", "N.mg.kg", "P.mg.kg", "OC.pct")

# Iterate over each prefix in the list of columns to plot
for (soil_p in soil_prop) {
  
  cat("Processing column: ", soil_p, "\n")
  cat("Processing column: ", which(soil_prop == soil_p), " of ", length(soil_prop), "\n")
  
  # Select the columns that match the prefix
  selected_columns <- grep(paste0("^", soil_p), names(ctd_plot), value = TRUE)
  
  # Convert the data to long format
  spl_plot <- ctd_plot |> 
    select(WRB2015, all_of(selected_columns)) |> 
    pivot_longer(cols = all_of(selected_columns), 
                 names_to = "soil_p_depths", values_to = "soil_p")
  spl_plot_count <- ctd_counts |> 
    select(WRB2015, all_of(selected_columns)) |> 
    pivot_longer(cols = all_of(selected_columns), 
                 names_to = "soil_p_depths", values_to = "soil_p")
  
  spl_plot <- spl_plot |> 
    left_join(spl_plot_count, by = c("WRB2015", "soil_p_depths"), keep = F)
  
  names(spl_plot) <- c("WRB2015", "soil_p_depths", "soil_p", "count")
  
  # Iterate over each soil class
  for (i in soil_classes) {
    
    cat("Processing soil class: ", i, "\n")
    cat("Processing soil class: ", which(soil_classes == i), " of ", length(soil_classes), "\n")
    
    spl_plot_class <- spl_plot |> 
      filter(WRB2015 == i)
    
    # Convert to factor and reorder the levels
    # spl_plot_class$soil_p_depths <- as.factor(spl_plot_class$soil_p_depths)
    # levels(spl_plot_class$soil_p_depths) <- spl_plot_class$soil_p_depths
    
    # Get the depths
    spl_plot_class$depth <- depths_[1:6]
    
    # Get the depths
    # dphs = gsub(paste0("^", soil_p, "_"), "", spl_plot_class$soil_p_depths)
    
    # Create the plot
    ggplot(spl_plot_class, aes(x = depth, y = soil_p)) +
      geom_smooth(method = "loess", color = "skyblue", se = F, linewidth= .8) +  # add a smoothed line
      # geom_line(color = "grey90", linewidth= 0.3) + # add line
      geom_point(color = "grey60", size= 0.4) + # add points
      geom_text(aes(label = count), hjust = 0.4, color = "black", size = 3) + # add text
      scale_x_reverse() +  # Inverte the x-axis
      coord_flip() +  # Flip the coordinates
      labs(x = "Depth (cm)", y = "Soil Property") +  # add labels
      theme_minimal() +  # Use minimal theme
      ggtitle(paste(i, "\n", soil_p)) # Add title
      
      # Save the plot
      # pth <- paste0("../output/datasets/splines_soil_preperties_to_model_gee/metrics_report/plots/", 
      pth <- paste0("../output/plots/spline_depths_plots/", 
                    paste(i, "_", soil_p, "_v3.png", sep = ""))
    ggsave(pth, plot = last_plot(), width = 3, height = 6)
    
  }
}


################################################################################
### Plot boxplot and hist for each soil property
### on original data and after data treatment and interpolation of missing values
options(max.print = 100)
df[1:10, ]
df |> names()

for (i in 10:25) {
  sprop_name <- names(df)[i]
  sprop <- df[, c(1, i)]
  sprop <- drop_na(sprop)
  
  png(filename = paste0("../output/plots/stats_plots/hist_boxplot_original_data_after_interpolation_of_missing/hist_", sprop_name, ".png"), 
      width = 14, height = 10, units = "cm", pointsize = 12,
      bg = "transparent", res = 300)
  hist(sprop[[2]], main = sprop_name, 
       xlab = sprop_name, col = "skyblue", border = "grey40")
  dev.off()
  
  # Calculate summary statistics
  sts <- round(summary(sprop[[2]])[c(1, 3, 4, 6)], 3)
  
  png(filename = paste0("../output/plots/stats_plots/hist_boxplot_original_data_after_interpolation_of_missing/boxplot_", sprop_name, ".png"),
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


################################################################################
### Plot boxplot and hist for each soil property
### on data after spline 
options(max.print = 600)
dfplt <- ea_spl_result_soil_type
dfplt |> dim()

for (i in 5:100) { # across all soil properties
  
  cat("Processing soil property: ", names(dfplt)[i], "\n", 
      which(names(dfplt) == names(dfplt)[i]), " of ", length(names(dfplt)), "\n")
  
  sprop_name <- names(dfplt)[i]
  sprop <- dfplt[, c(1, i)]
  sprop <- drop_na(sprop)
  
  png(filename = paste0("../output/plots/stats_plots/hist_boxplot_after_spline/hist_", sprop_name, ".png"), 
      width = 14, height = 10, units = "cm", pointsize = 12,
      bg = "transparent", res = 300)
  hist(sprop[[2]], main = sprop_name, 
       xlab = sprop_name, col = "skyblue", border = "grey40")
  dev.off()
  
  # Calculate summary statistics
  sts <- round(summary(sprop[[2]])[c(1, 3, 4, 6)], 3)
  
  png(filename = paste0("../output/plots/stats_plots/hist_boxplot_after_spline/boxplot_", sprop_name, ".png"),
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






