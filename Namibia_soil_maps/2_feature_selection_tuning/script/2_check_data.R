###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
### Yuri Gelsleichter, Marina Coetzze
### Check data
### Jun 2025
###_____________________________________________________________________________

### Set working directory automatically ----
library(this.path)
path_ <- this.dir()
setwd(path_)
getwd()
# clean memory
# gc(); rm(list=ls())

#¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
# Load packges ----
#_______________________________________________________________________________
library(tidyr)
library(dplyr)

#¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
# Load the data ----
#_______________________________________________________________________________
# Regression matrix
df <- read.csv("../input/extracted_points_Nam_0_30_60_100_cm.csv")
names(df) |> dput()
df |> str()

# Data used to extract regression matrix
df <- read.csv(paste0("../../1_data_treatement/output/datasets/splines_soil_preperties_to_model_gee/", 
                      "Namibia_ea_spline_all_properties_2025_03_03_null_depths_0_30_60_100_fix.csv"))
df |> str()
# Convert soil properties to numeric, (null values coerced to character)
df <- df |> dplyr::mutate(dplyr::across(dplyr::ends_with("_cm"), as.numeric))
df |> str()
table(is.na(df$BS.pct_0_30_cm))
table(df$BS.pct_0_30_cm)
boxplot(df$BS.pct_0_30_cm)
dev.off()

# Data used to to modeling in gee with NA
df <- read.csv(paste0("../../1_data_treatement/output/datasets/splines_soil_preperties_to_model_gee/", 
                      "Namibia_ea_spline_all_properties_2025_03_03_null_depths_0_30_6_100.csv"))
df |> str()
boxplot(df$BS.pct_0_30_cm)
dev.off()
df <- df |> dplyr::mutate(dplyr::across(dplyr::ends_with("_cm"), as.numeric))
df |> str()

# Data used to to modeling in gee, with NA
df <- read.csv(paste0("../../1_data_treatement/output/datasets/splines_soil_preperties_to_model_gee/", 
                      "Namibia_ea_spline_all_properties_2025_03_03_depths_0_30_6_100.csv"))
df |> str()
boxplot(df$BS.pct_0_30_cm)
dev.off()
df <- df |> dplyr::mutate(dplyr::across(dplyr::ends_with("_cm"), as.numeric))
names(df) |> dput()
df |> str()


