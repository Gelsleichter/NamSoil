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
### Define working directory for input output files in correct places
################################################################################
#' ### Auto setwd
#' ### Define the working directory as the folder of the script
library(rstudioapi); current_path <- getActiveDocumentContext()$path; setwd(dirname(current_path)) ### https://eranraviv.com/r-tips-and-tricks-working-directory/
getwd()
gc(); rm(list=ls())

dir() # see what is in the directory
dir("..") # see what is in the directory above

################################################################################
### Load data from spreadsheet
################################################################################
# library(readr)
library(readxl)
library(writexl) # install.packages("writexl")
library(dplyr) # for glimpse

path_ <- paste0("../input/")
file_ <- paste0("NamProfCleaned_21Mar2024.xlsx")

####################################
### Site Description
s.prof.info <- readxl::read_xlsx(
  path = paste0(path_, file_),
  # sheet = "Prof Registration", # in: NamProfCleaned_20Oct2023_v1.xlsx
  sheet = "Reg&Site", # in: NamProfCleaned_21Mar2024_cleaned.xlsx
  # col_types = "numeric",
  # .name_repair = "unique"
  .name_repair = "universal_quiet" ### for "universal_quiet", see: https://stackoverflow.com/questions/75603754/problem-with-importing-a-csv-file-with-extremely-long-column-names/75643299#75643299
)
s.prof.info
# str(s.prof.info) ### Make sure that data is numeric (double) or integer
names(s.prof.info)
glimpse(s.prof.info)

####################################
### Horizons
horiz <- readxl::read_xlsx(
  path = paste0(path_, file_),
  # sheet = "Horizons", # in: NamProfCleaned_20Oct2023_v1.xlsx
  sheet = "Hor&prof", # in: NamProfCleaned_21Mar2024_cleaned.xlsx
  # col_types = "numeric",
  # .name_repair = "unique"
  .name_repair = "universal_quiet",
  guess_max = 10000 # *
) 
"
*The columsn EC has NA and NaN mixed and this can make the default guess considering EC as 
'logical' instead of 'numeric', using the 'guess_max = 10000' will check the first 10000 columns 
insted of default 1000, and then it will be set as 'Numeric' instead of 'logical', the value can be 'Inf' also
"
horiz
names(horiz)
glimpse(horiz)


################################################################################
### Data checking
# slec1 <- c("PRID", "Latitude", "Longitude", "Sand......53.µm....SDTO.","Silt......2.53µm....STPC.","Clay........2.µm.....CLPC.","Bulk.Density.kg.dm3","pH.water..PHAQ.","Electrical.Conductivity.µS.cm..2.soil.5.water.suspension...EL25.","Electrical.Conductivity.µS.cm..Saturated.Paste.Extract...ELCO.","Ca.mg.kg..Extractable..1M.NH4acetate..AAS...SOCA.","Mg.mg.kg..Extractable..1M.NH4acetate..AAS...SOMG.","K.mg.kg..Extractable..1M.NH4acetate..AAS...SOLK.","Na.mg.kg..Extractable..1M.NH4acetate..AAS...SONA.","CEC.Measured...cmol....kg...","Base.Saturation.Estimate.......from.pHwater.","Total.Nitrogen..mg.kg...TOTN.","Phosphorus..Olsen..mg.kg","Organic.Carbon...WB")
# slec2 <- c("PRID", "Latitude", "Longitude", "Coordinate.Reference.System")
# 
# horiz |> dplyr::filter(PRID == "AMP-NA-0003") |> as.data.frame() |> select(all_of(slec1))
# horiz |> dplyr::filter(PRID == "AO-0010") |> as.data.frame() |> select(all_of(slec1))
# horiz |> dplyr::filter(PRID == "KB-0028") |> as.data.frame() |> select(all_of(slec1))
# horiz |> dplyr::filter(PRID == "KH-0036") |> as.data.frame() |> select(all_of(slec1))
# horiz |> dplyr::filter(PRID == "GIZ-Bush-SOC-140") |> as.data.frame() |> select(all_of(slec1))
# horiz |> dplyr::filter(PRID == "DAM-0017") |> as.data.frame() |> select(all_of(slec1))
# horiz |> dplyr::filter(PRID == "DAM-0019") |> as.data.frame() |> select(all_of(slec1))
# horiz |> dplyr::filter(PRID == "LDN-Okaksc-0016") |> as.data.frame() |> select(all_of(slec1))
# horiz |> dplyr::filter(PRID == "SASSCAL-I-P2503") |> as.data.frame() |> select(all_of(slec1))
# horiz |> dplyr::filter(PRID == "NH-0028") |> as.data.frame() |> select(all_of(slec1))
# 
# 
# soil_profiles_check <- c(#"AMP-NA-0003"
#                          "AO-0010"
#                          ,"KB-0028"
#                          ,"KH-0036"
#                          #,"GIZ-Bush-SOC-140"
#                          #,"DAM-0017"
#                          #,"DAM-0019"
#                          #,"LDN-Okaksc-0016"
#                          )
# df1 <- horiz |> dplyr::filter(PRID %in% soil_profiles_check) |> as.data.frame() |> select(all_of(slec1))
# df2 <- s.prof.info |> dplyr::filter(PRID %in% soil_profiles_check) |> as.data.frame() |> select(all_of(slec2))
# df1
# df2
# 
# df <- df1 %>%
#   full_join(df2, by = c("PRID", "Latitude", "Longitude"), 
#             keep = F) |> as.data.frame()
# df
### Data checking - end
################################################################################

################################################################################
## Select and organize lines (rows) of interest: 
s.prof.info
# site.descr
horiz
# library(tidyverse)
# full_join()

### Test join with small example
df1 <- s.prof.info[1:10, 1:15]
# df2 <- site.descr[1:10, 1:8]
df3 <- horiz[1:10, 1:8]

df1 |> as.data.frame()
# df2
df3 |> as.data.frame()

df1 %>%
  # full_join(df2, by = c("PRID", "Latitude", "Longitude"), keep = F) %>% 
  # full_join(df3, by = c("PRID", "Latitude", "Longitude"), keep = F) |> as.data.frame()
  full_join(df3, by = c("PRID"), suffix = c("__let_blank", ".y"), keep = F) |> as.data.frame() #### differences in coordinates round across horizons cause the column of Coordinate.Reference.System not be included in the final merge, then removing long and lat it waorks well
# Note that the remaining columns with same name from more than one dataset
# are duplicated and renamed, for example: 'Dataset.x' and 'Dataset.y'

### Join all
df1 <- s.prof.info
# df2 <- site.descr
df3 <- horiz

df <- df1 %>%
  # full_join(df2, by = c("PRID", "Latitude", "Longitude"), keep = F) %>% 
  # full_join(df3, by = c("PRID", "Latitude", "Longitude"), keep = F)
  full_join(df3, by = c("PRID"), suffix = c("", ".y"), keep = F) |> as.data.frame() #### differences in coordinates round across horizons cause the column of Coordinate.Reference.System not be included in the final merge, then removing long and lat it waorks well
# Note that the remaining columns with same name from more than one dataset
# are duplicated and renamed, for example: 'Dataset.x' and 'Dataset.y'

# install.packages("skimr")
library(skimr)

df |> names() |> dput()
df |> glimpse()
df1 |> glimpse()
# df2 |> glimpse()
df3 |> glimpse()
# df1 |> skim()
# df$Status

### Rename columns from Dataset.x to Dataset
# names(df)[2] <- "Dataset"

### Select: pH, texture, EC, base saturation, OC, CaCO3, and soil classes.
df <- df[ , c(
"PRID",
"Dataset",
"HONU",
"Year",
"Coordinate.Reference.System",
"Latitude",
"Longitude",
# "Elevation.m...GPS",
"Surveyor.s.",
# "Status",
# "Profile.Classification.Original",
# "Profile.Classification.Original.Code",
# "Classification.System",
"WRB2015.Reclassification.RSG",
# "WRB2015.Reclassification.Principal.Qualifiers",
# "WRB2015.Reclassification.Supplementary.Qualifiers",
# "WRB.Classification.by.S.Nambambi",
# "Location",
# "Laboratory",
# "Soil.Lab.ID",
### "Upper.Depth.cm", ### this carried excel troubleshot, thus the values are wrong
### "Lower.Depth.cm", ### this carried excel troubleshot, thus the values are wrong
"Upper.Depth.cm..contains...", # this [contains >]
"Lower.Depth.cm..contains...", # this [contains >]
"Sand......53.µm....SDTO.",
"Silt......2.53µm....STPC.",
"Clay........2.µm.....CLPC.",
"Bulk.Density.kg.dm3",
"pH.water..PHAQ.",
"Electrical.Conductivity.µS.cm..2.soil.5.water.suspension...EL25.",
"Electrical.Conductivity.µS.cm..Saturated.Paste.Extract...ELCO.",
"Ca.mg.kg..Extractable..1M.NH4acetate..AAS...SOCA.",
"Mg.mg.kg..Extractable..1M.NH4acetate..AAS...SOMG.",
"K.mg.kg..Extractable..1M.NH4acetate..AAS...SOLK.",
"Na.mg.kg..Extractable..1M.NH4acetate..AAS...SONA.",
"CEC.Measured...cmol....kg...",
"Base.Saturation.Estimate.......from.pHwater.",
"Total.Nitrogen..mg.kg...TOTN.",
"Phosphorus..Olsen..mg.kg",
"Organic.Carbon...WB"
)]
str(df)

skim(df) %>%
  dplyr::select(-numeric.sd,
                -numeric.p0, 
                -numeric.p25, 
                -numeric.p50,
                -numeric.p75,
                -numeric.p100)

df |> as.data.frame()
df |> glimpse()

##############################
### Rename
names(df) <- c(
  "PRID",
  "Dataset",
  "HONU",
  "Year",
  "crs",
  "Latitude",
  "Longitude",
  "Surveyor",
  "WRB2015",
  "Upper.Depth",
  "Lower.Depth",
  "Sand.53.um.pct",
  "Silt.2.53um.pct",
  "Clay.2.um.pct",
  "BD.kg.dm3",
  "pH.water",
  "EC.uS.cm.EL25",
  "EC.uS.cm.ELCO",
  "Ca.mg.kg",
  "Mg.mg.kg",
  "K.mg.kg",
  "Na.mg.kg",
  "CEC.cmol.kg",
  "BS.pct",
  "N.mg.kg",
  "P.mg.kg",
  "OC.pct") 

names(df)
df$Year |> table()
df$BS.pct |> table() |> as.data.frame()
df$pH.water |> table() |> as.data.frame()
table(is.na(df$pH.water))
sum(with(as.data.frame(table(df$pH.water)), Freq[as.numeric(as.character(Var1)) < 7]))

# Fix > and / in Depth(s)
table(df$Upper.Depth) |> as.data.frame()
table(df$Lower.Depth) |> as.data.frame()

library(stringr)
# Create a column to mark values with ">"
df$has_greater_than_ud <- grepl(">", df$Upper.Depth)
df$has_greater_than_ld <- grepl(">", df$Lower.Depth)

# Function to clean the values
clean_depth <- function(x) {
  x <- gsub(">", "", x)
  x <- gsub(" ", "", x)
  
  if (grepl("/", x)) {
    nums <- as.numeric(unlist(str_split(x, "/")))
    return(mean(nums, na.rm = TRUE))
  } else {
    return(as.numeric(x))
  }
}

# Apply cleaning
df$Upper.Depth_clean <- sapply(df$Upper.Depth, clean_depth)

# Add 20% to values that had ">"
df$Upper.Depth_final <- ifelse(df$has_greater_than_ud,
                               df$Upper.Depth_clean * 1.20,
                               df$Upper.Depth_clean)

# Check the values that had ">"
table(df$Upper.Depth_final, useNA = "ifany")

# Conclude and overwrite
df$Upper.Depth <- df$Upper.Depth_final

# Apply cleaning
df$Lower.Depth_clean <- sapply(df$Lower.Depth, clean_depth)

# Add 20% to values that had ">"
df$Lower.Depth_final <- ifelse(df$has_greater_than_ld,
                               df$Lower.Depth_clean * 1.20,
                               df$Lower.Depth_clean)

# Check the values that had ">"
table(df$Lower.Depth_final, useNA = "ifany")

# Conclude and overwrite
df$Lower.Depth <- df$Lower.Depth_final

# Remove the columns used for cleaning
drop_cols <- c("has_greater_than_ud", "has_greater_than_ld", "Upper.Depth_clean", 
               "Upper.Depth_final", "Lower.Depth_clean", "Lower.Depth_final")
df <- df[ , !(names(df) %in% drop_cols)]
str(df)

# Replace "80 - 90" to "85" and "90 - 100" to "95" in BS.pct
# No need anymore because the function will do of it from pH
# df$BS.pct <- gsub("^80 - 90$", "85", df$BS.pct) # the "^" anchor the match to the start of the string and "$" to anchor it to the end
# df$BS.pct <- gsub("^90 - 100$", "95", df$BS.pct)

### Set NA, replace only the ones not derived from CEC
df$BS.pct <- gsub("^80 - 90$", NA, df$BS.pct) # the "^" anchor the match to the start of the string and "$" to anchor it to the end
df$BS.pct <- gsub("^90 - 100$", NA, df$BS.pct)

# Function to calculate bs from pH_H2O :: source: https://www.scielo.br/j/brag/a/ZRt3zZkvJdKpDBTfSWfyJKd/?format=pdf&lang=pt
bs_estim <- function(pH) {
  bs <- (pH - 4.67) / 0.0256
  bs <- round(bs, 2)
  return(bs)
}
# run a test
test <- c(4.5, 5.5, 6.5, 7.5, 8.5, 9.5)
bs_out <- bs_estim(test)
bs_out[bs_out > 100] <- 100
bs_out[bs_out < 1] <- NA
bs_out
# Check the min for pH
min(df$pH.water, na.rm = T)
min(bs_out, na.rm = T)
# boxplot(bs_out, na.rm = T)

# Execute the BS calculations
bs_out <- bs_estim(df$pH.water)
bs_out[bs_out > 100] <- 100
# table(bs_out[bs_out < 1])
bs_out[bs_out < 1] <- NA

# Overwrite the BS column with the calculated values and replace only in NA places
### Test
real_obs_BS <- c(NA, 50, 80, 70, 100, NA, 100, 100)
pedotrans_estimn <- c(43, 51, 81, 71, 91, 101, 101, 101)
real_obs_BS[is.na(real_obs_BS)] <- pedotrans_estimn[is.na(real_obs_BS)]
real_obs_BS

# Overwrite the BS column with the calculated values and replace only in NA places
# Run
df$BS.pct <- as.numeric(df$BS.pct)
df$BS.pct[is.na(df$BS.pct)] <- bs_out[is.na(df$BS.pct)]
### df$BS.pct <- bs_out ### this will replace all values, not only the NA ones
# hist(df$BS.pct)

### Reproject EPSG:4293 (Schwarzeck) to EPSG:4326
# Check dimensions of the dataframe
dim(df)
# View the structure of the dataframe to understand types of each column
str(df)

# Remove any non-numeric characters from the Latitude and Longitude columns
df$Latitude <- gsub("[^0-9.-]", "", df$Latitude)
df$Longitude <- gsub("[^0-9.-]", "", df$Longitude)
df$Upper.Depth <- gsub("[^0-9.-]", "", df$Upper.Depth)
df$EC.uS.cm.ELCO <- gsub("[^0-9.-]", "", df$EC.uS.cm.ELCO)

# Convert Latitude and Longitude columns to numeric data type
df$Upper.Depth <- as.numeric(df$Upper.Depth)
df$Latitude <- as.numeric(df$Latitude)
df$Longitude <- as.numeric(df$Longitude)
df$EC.uS.cm.ELCO <- as.numeric(df$EC.uS.cm.ELCO)

# Check for missing values in the Latitude and Longitude columns
table(is.na(df$Latitude)) # previously was 5
table(is.na(df$Longitude)) # previously was 9
table(is.na(df$Upper.Depth)) # this has more NA, but coords had none
table(is.na(df$BS.pct)) # this has more NA, but coords had none

# Use drop_na() to remove rows with missing values in Latitude or Longitude columns
library(tidyr)
df <- drop_na(df, Latitude)
df <- drop_na(df, Longitude)

# Inspect the first 10 rows of the coordinate reference system column
df$crs[1:10]

library(dplyr)
# Filter rows where the CRS is set to Schwarzeck
df_sch <- df %>% filter(crs == "Geographic, Schwarzeck")
# Filter rows where the CRS is WGS84
df_wgs <- df %>% filter(crs == "Geographic, WGS84")

dim(df_sch)
dim(df_wgs)
# 3697+6078 # update: 6671 + 6629 = 13300

library(terra)
# Create a spatial vector from the Schwarzeck data
sampl_point_sch <- terra::vect(df_sch, 
                               geom=c("Longitude", "Latitude"), # specify columns for geometry
                               crs= "EPSG:4293", # set the CRS to Schwarzeck
                               keepgeom=T) # keep geometry information
# Reproject the Schwarzeck coordinates to WGS84
sampl_point_sch <- terra::project(sampl_point_sch, "EPSG:4326")

# Convert the spatial data back to a dataframe and include geometry as columns
sampl_point_sch.df <- terra::as.data.frame(sampl_point_sch, geom = "XY")

# Check the names of columns in the WGS84 dataframe
names(df_wgs)
# Check the names of columns in the transformed Schwarzeck dataframe
names(sampl_point_sch.df)
# Remove the old Latitude and Longitude columns from the Schwarzeck dataframe
sampl_point_sch.df$Latitude <- NULL
sampl_point_sch.df$Longitude <- NULL

# Rename columns to match those in the WGS84 dataframe for consistency
names(sampl_point_sch.df)[26]
names(sampl_point_sch.df)[27]
names(sampl_point_sch.df)[26] <- "Longitude"
names(sampl_point_sch.df)[27] <- "Latitude"

# Ensure the Schwarzeck dataframe has the same columns as the WGS84 dataframe
sampl_point_sch.df <- sampl_point_sch.df[ , names(df_wgs)]

# Combine the reprojected Schwarzeck data and the original WGS84 data
df <- rbind(sampl_point_sch.df, df_wgs)

# Set the CRS column to indicate all data is now in WGS84
df$crs <- "Geographic_WGS84"

# Check frequency of different CRS settings in the dataframe
table(df$crs)

# Remove the CRS column as it is now redundant
df$crs <- NULL

#################################################
### Export data table for splines
### BD has only 79 samples (horiz.), and surveyor is not relevant here, thus, drop both 
df <- df %>%
  dplyr::select(-Surveyor)
names(df)

write_xlsx(
  df,
  # path = "../output/datasets/dataset_for_splines/Dataset_for_splines_2024_09_09.xlsx",
  path = "../output/datasets/dataset_for_splines/Dataset_for_splines_2025_03_03.xlsx",
  col_names = TRUE,
  format_headers = TRUE#,
  # use_zip64 = FALSE
)
write.csv(df, file = "../output/datasets/dataset_for_splines/Dataset_for_splines_2025_03_03.csv", row.names = F)
#################################################

### Verify the dominant Upper.Depth 
hist(df$Upper.Depth)
hist(df$Upper.Depth, breaks = 60)
hist(df$Upper.Depth, breaks = 60, xaxt = "n", main = "Histograma de Upper.Depth", xlim=c(0, 100))
tick_positions <- seq(min(df$Upper.Depth, na.rm = TRUE), max(df$Upper.Depth, na.rm = TRUE), by = 10)
axis(1, at = tick_positions, labels = tick_positions)

hist(df$Lower.Depth)
hist(df$Lower.Depth, breaks = 60)
hist(df$Lower.Depth, breaks = 100, xaxt = "n", main = "Histograma de Lower.Depth", xlim=c(0, 200))
tick_positions <- seq(min(df$Lower.Depth, na.rm = TRUE), max(df$Lower.Depth, na.rm = TRUE), by = 10)
axis(1, at = tick_positions, labels = tick_positions)

ld <- df |> drop_na(Lower.Depth)
ld <- ld[ld$Lower.Depth < 150, ]
min(ld$Lower.Depth)

hl <- hist(ld$Lower.Depth, breaks = 60)

text(x = hl$mids, 
     y = hl$counts, 
     labels = hl$counts, pos = 3, cex = 0.8, col = "red")
axis(1, at = hl$breaks, labels = hl$breaks, tick = TRUE, cex.axis = 0.7)

plot(density(ld$Lower.Depth, na.rm = T))
