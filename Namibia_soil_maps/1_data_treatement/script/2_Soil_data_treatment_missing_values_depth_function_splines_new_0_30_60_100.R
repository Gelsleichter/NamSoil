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
library(terra)
nam <- terra::vect("../input/National_boundary.shp")

df[1:10, ]
df[1:10, c(7:8, 10:25)] # x, y, properties
df |> names()

# Plot one
points_vect <- vect(df, geom = c("x", "y"), crs = "EPSG:4326")
terra::plot(nam, border = "grey30", axes =F, lwd = 1)
terra::plot(points_vect, col = "blue", add = TRUE)

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

# Implement the reordering
df <- df |> dplyr::arrange(Soil.ID, Upper.Depth, Lower.Depth)

################################################################################

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

### Check how many values were interpolated
table(is.na(df)) 
table(is.na(result)) 
# 179245-170613 # False values df-result = 8632 sample interpolated

# Get a summary of missing values before and after interpolation
colSums(!is.na(df[ , interp_columns]))
colSums(!is.na(result[ , interp_columns]))

report <- data.frame(
  original_data        = colSums(!is.na(df[ , interp_columns])),
  after_interpolation  = colSums(!is.na(result[ , interp_columns]))
)
report$number_of_interpolated <- report$after_interpolation - report$original_data

writeLines(
  text = capture.output(print(report)),
  con  = "../output/datasets/dataset_for_splines/Report_number_of_samples_per_soil_property_before_and_after_interpolation.txt"
)

################################################################################
### Change the limits for spline depth interpolation
################################################################################
"
About the limits for missing data and spline depths interpolation:

The limits inside spline function can corce the original data values, 
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
depths_ <- c(0,30,60,100)

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
          "../output/datasets/splines_soil_preperties_to_model_gee/Namibia_ea_spline_all_properties_2025_03_03_depths_0_30_60_100.csv", 
          row.names = F)

write.csv(ea_spl_result_soil_type, 
          "../output/datasets/splines_soil_preperties_to_model_gee/Namibia_ea_spline_all_properties_2025_03_03_null_depths_0_30_60_100.csv", 
          na = "", ### Important for gee
          row.names = F)

### Check max and min values for outliers and wrong extrapolation
sapply(df, max, na.rm = TRUE)
sapply(df, min, na.rm = TRUE)

sapply(ea_spl_result_soil_type, max, na.rm = TRUE)
sapply(ea_spl_result_soil_type, min, na.rm = TRUE)
