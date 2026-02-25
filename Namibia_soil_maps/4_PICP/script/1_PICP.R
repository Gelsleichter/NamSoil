###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
### Mapping Namibia
### Yuri Andrei Gelsleichter, Marina Coetz
### Jun 2025
###_____________________________________________________________________________

### Set working directory automatically 
library(this.path)
path_ <- this.dir()
setwd(path_)

### Check the "new" working directory 
getwd()

### clean memory
# gc(); rm(list=ls())

### Set options
options(scipen = 999, digits = 7)

# install.packages('terra', repos='https://rspatial.r-universe.dev') # dev version 1.8.40
library(terra) # for rasters and vectors
library(dplyr)
library(stringr)
library(readr)

### Input data ----
# observed (point data)
obs <- read.csv("../../1_data_treatement/output/datasets/splines_soil_preperties_to_model_gee/Namibia_ea_spline_all_properties_2025_03_03_null_depths_0_30_60_100_fix.csv")
# Replace "null" and "" with NA
obs[obs == "" | obs == "null"] <- NA
as_tibble(obs)
names(obs)

# List TIFF files, predicted (Maps)
rsts <- list.files("../../3_final_maps/input/", pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
rsts

###_____________________________________________________________________________
### Prepare the dataframe with reference properties
###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
# Data frame with reference properties
ref_df <- data.frame(matrix(c(
  "Sand",       "Sand",  "Sand",                                           "%",            "YlOrBr",
  "Silt",       "Silt",  "Silt",                                           "%",            "YlOrBr",
  "Clay",       "Clay",  "Clay",                                           "%",            "YlOrBr",
  "BD",         "BD",    "Bulk Density",                                   "kg dm-3",      "Greys",
  "pHwater",    "pH",    "pH",                                             "water",        "RdYlBu",
  "ECuScmEL25", "EC.25", "Electrical conductivity of 2:5 supernatant",     "µS cm-1",      "YlGnBu", # dS m-1
  "ECuScmELCO", "EC",    "Electrical conductivity of saturation extract",  "µS cm-1",      "PuBu",
  "Ca",         "Ca",    "Calcium",                                        "mg kg-1",      "YlOrRd",
  "Mgmgkg",     "Mg",    "Magnesium",                                      "mg kg-1",      "BuPu",
  "Kmgkg",      "K",     "Potassium",                                      "mg kg-1",      "BuGn",
  "Namgkg",     "Na",    "Sodium",                                         "mg kg-1",      "YlGn",
  "CEC",        "CEC",   "Cation Exchange Capacity",                       "cmolc kg-1",   "RdPu",
  "BS",         "BS",    "Base Saturation",                                "%",            "Blues",
  "Nmgkg",      "N",     "Nitrogen",                                       "mg kg-1",      "Greens",
  "Pmgkg",      "P",     "Phosphorus",                                     "mg kg-1",      "Purples",
  "OC",         "OC",    "Organic Carbon",                                 "%",            "Oranges"
), nrow=16, byrow=TRUE), stringsAsFactors=FALSE)
colnames(ref_df) <- c("prop.grep", "prop.leg.title", "prop.title", "unit", "pcol")
as_tibble(ref_df)

# Load PICP function
source("./0_PICP_source_function.R") # Load the PICP function

# Create a file to hold PICP results at 90%
if (!file.exists("../output/PICP_90_ccc.csv")) {
  dplyr::tibble(Soil_property = character(), PICP_90 = numeric(), CCC = numeric()) |> 
    readr::write_csv("../output/PICP_90_ccc.csv")
}

###_____________________________________________________________________________
### Loop over each soil property to Calculate PICP for each soil property
###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
# Soil property columns (exclude ID, classification and coordinates)
prop_cols <- setdiff(names(obs), c("Soil.ID", "WRB2015", "x", "y"))

# Loop over each soil property
for (col in prop_cols) {
  cat(sprintf("Iteration: %d of %d – %s\n", 
              which(prop_cols == col), length(prop_cols), col))

  # Keep only points with observed value for this property
  df <- obs[!is.na(as.numeric(obs[[col]])), c("x", "y", col)]
  if (nrow(df) == 0) next
  
  # Create SpatVector of observation points
  v <- terra::vect(df, geom = c("x", "y"), crs = "EPSG:4326")
  
  # Find corresponding prediction raster
  pattern <- gsub("\\.", "", col) 
  r_file <- rsts[grep(pattern, rsts, ignore.case = TRUE)]
  if (length(r_file) == 0) next
  
  # Load only the mean prediction layer
  r <- terra::rast(r_file)[[1]]
  
  # Extract property and depth from the file name
  file_name <- basename(r_file)
  prop_match <- ref_df$prop.grep[sapply(ref_df$prop.grep, function(x) grepl(x, file_name))]
  depth <- str_extract(file_name, "\\d+_\\d+_cm")
  depth <- gsub("_", "-", depth)

  # Extract more property details
  ras_name <- names(r) # e.g., "Silt253umpct_0_30_cm_mean"
  partes <- unlist(str_split(ras_name, "_"))
  depth <- paste0(partes[2], "-", partes[3], " ", "cm")
  interval <- partes[5]
  prop_match <- ref_df$prop.grep[sapply(ref_df$prop.grep, function(p) grepl(p, ras_name, fixed = TRUE))][1]
  if (is.na(prop_match)) {
    warning(paste("Property not found for:", ras_name))
    soil_property <- "Unknown"
    unit <- ""
    pcol <- ""
  } else {
    match_row <- ref_df[ref_df$prop.grep == prop_match, ]
    soil_property <- match_row$prop.title
    s_prop_lg_tll <- match_row$prop.leg.title
    unit <- match_row$unit
    pcol <- match_row$pcol
  }
  
  # Helper function to convert units to expression for superscript formatting
  unit_to_expression <- function(unit) {
    switch(unit,
           "µS cm-1" = mu*phantom()~S ~ cm^{-~1},
           "cmolc kg-1" = cmol[c] ~ kg^{-~1},
           "kg dm-3" = kg ~ dm^{-~3},
           "mg kg-1" = mg ~ kg^{-~1},
           unit)  # Return unit as-is if no superscript needed
  }
  
  # Extract predicted values at observation points
  extr <- terra::extract(r, v, bind = T, xy = F) # xy = T gives the coordinates together
  cat("Points extracted from raster \n")
  cat("Raster name is:", ras_name, "\n")
  # cat("Unity is:", unit, "\n")
  
  # Convert to data frame
  ext_df <- terra::as.data.frame(extr)

  # Remove rows with NA
  ext_df <- ext_df[complete.cases(ext_df), ]

  # Rename columns
  colnames(ext_df) <- c("observed", "predicted")
  ext_df$observed <- as.numeric(ext_df$observed)
  ext_df$predicted <- as.numeric(ext_df$predicted)

  # Construct title label for PICP plot
  title_label <- if (unit %in% c("µS cm-1", "cmolc kg-1", "kg dm-3", "mg kg-1")) {
    bquote("PICP " ~ .(s_prop_lg_tll) ~ "(" * .(unit_to_expression(unit)) * ")" ~ .(depth))
  } else {
    paste0("PICP ", s_prop_lg_tll, " (", unit, ") ", depth)
  }

  ### For debugging: Plot PICP graph without calculating PICP
  # results <- data.frame(cl=1:10, picp=1:10) # Initialize empty data frame for results
  # ggplot(data = results, aes(x= cl, y = picp)) + # add data
  #   geom_point() + # add points
  #   # geom_text(data = ccc, aes(x= x, y =y, label = paste("CCC = ", round(CCC, 2)))) + # add CCC value
  #   geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = 'red')+ # add 1:1 line
  #   # labs(x = 'Confidence level', y = 'PICP', title = 'PICP to confidence level') + # labels
  #   labs(x = 'PI level (%)', y = 'PICP (%)', title = title_label) + # labels
  #   # coord_fixed(ratio=1) +
  #   tune::coord_obs_pred() + # make the plot square # https://tune.tidymodels.org/reference/coord_obs_pred.html
  #   theme_bw() + # make it look good
  #   theme(text = element_text(size= 14), # increase base text size
  #         plot.title = element_text(size = 14)) # increase text size on title
  
  # Calculate PICP
  picp_res <- calcPICP(data = ext_df, 
                       response = ext_df$observed, 
                       pred = ext_df$predicted, 
                       title_label = title_label, 
                       exp_plt_dir = "../output/plots/", 
                       plt_name = paste0("PICP_", ras_name, ".png"))

  # Export PICP at 90% and CCC
  picp_90 <- picp_res$Results |> 
    filter(cl == 90) |> 
    pull(picp)  
  ccc_val <- picp_res$CCC$CCC

  tibble(
    Soil_property = ras_name,
    PICP_90       = round(picp_90, 3),
    CCC           = round(ccc_val, 3)
  ) |> 
    readr::write_csv("../output/PICP_90_ccc.csv", append = TRUE)
}

#¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
# End script ----
#_______________________________________________________________________________


