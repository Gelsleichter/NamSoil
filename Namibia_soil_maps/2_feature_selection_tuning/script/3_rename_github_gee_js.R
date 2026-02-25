###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
### Yuri Gelsleichter, Marina Coetzze
### Tuning and feature selection for Random Forest Modelling 
### May 2025
###_____________________________________________________________________________

### Set working directory automatically ----
library(this.path)
path_ <- this.dir()
setwd(path_)
getwd()
# clean memory
# gc(); rm(list=ls())

dir()
dir("../")
dir("../output/")
dir("../output/properties_settings/")

library(dplyr)

# Define the original and destination directories
source_dir <- "../output/properties_settings"
dest_dir <- file.path(source_dir, "js")

# Create the 'js' directory if it doesn't exist
if (!dir.exists(dest_dir)) {
  dir.create(dest_dir)
}

# List all .txt files in the original directory
files <- list.files(path = source_dir, pattern = "\\.txt$", full.names = TRUE)

# Function to copy and rename files
copy_and_rename_files <- function(file_paths, destination) {
  # Extract only the file names
  file_names <- basename(file_paths)
  
  # Apply substitutions with gsub and regex
  new_names <- file_names %>%
    # Remove the "bor_" prefix
    gsub("^bor_", "", .) %>%
    # Replace unit suffixes and specific terms
    gsub("BDkgdm3", "BD", .) %>%
    gsub("BSpct", "BS", .) %>%
    gsub("Camgkg", "Ca", .) %>%
    gsub("CECcmolkg", "CEC", .) %>%
    gsub("Clay2umpct", "Clay", .) %>%
    gsub("ECuScmEL25", "EC_el25", .) %>%
    gsub("ECuScmELCO", "EC_elco", .) %>%
    gsub("Kmgkg", "K", .) %>%
    gsub("Mgmgkg", "Mg", .) %>%
    gsub("Namgkg", "Na", .) %>%
    gsub("Nmgkg", "N", .) %>%
    gsub("OCpct", "OC", .) %>%
    gsub("pHwater", "pH", .) %>%
    gsub("Pmgkg", "P", .) %>%
    gsub("Sand53umpct", "Sand", .) %>%
    gsub("Silt253umpct", "Silt", .) %>%
    # Remove "_cm" before the extension
    gsub("_cm", "", .) %>%
    # Change the extension from ".txt" to ".js"
    # gsub("\\.txt$", ".js", .)
    gsub("\\.txt$", "", .) # to fit in the already establish gee script
  
  # Build full paths for the new files in the 'js' directory
  new_paths <- file.path(destination, new_names)
  
  # Copy the files to the new directory
  file.copy(from = file_paths, to = new_paths)
  
  # Return the new paths
  return(new_paths)
}

# Execute the function
new_file_paths <- copy_and_rename_files(files, dest_dir)

# Display the new paths
print(new_file_paths)




