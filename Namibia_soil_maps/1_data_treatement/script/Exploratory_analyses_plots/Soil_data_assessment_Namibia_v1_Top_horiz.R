################################################################################
### Yuri Gelsleichter
### Soil_data_assessment_Namibia
### Oct 2023
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
dir("../..") # see what is in the directory above

################################################################################
### Load data from spreadsheet
################################################################################
library(readxl)
library(writexl)
library(dplyr) # for glimpse

path_ <- paste0("../../input/")
file_ <- paste0("earlier_version/NamProfCleaned_20Oct2023_v1.xlsx")
# file_ <- paste0("NamProfCleaned_21Mar2024.xlsx")

####################################
### Site Description
s.prof.info <- readxl::read_xlsx(
  path = paste0(path_, file_),
  sheet = "Prof Registration", # in: NamProfCleaned_20Oct2023_v1.xlsx
  # sheet = "Reg&Site", # in: NamProfCleaned_21Mar2024_cleaned.xlsx
  # col_types = c(rep("guess", 5), "text", rep("guess", 33 - 6)), # 33 is the total number of columns
  # col_types = "numeric",
  # .name_repair = "unique"
  # .name_repair = "universal"
  # .name_repair = "check_unique"
  # .name_repair = "minimal"
  .name_repair = "universal_quiet" ### for "universal_quiet", see: https://stackoverflow.com/questions/75603754/problem-with-importing-a-csv-file-with-extremely-long-column-names/75643299#75643299
  
)
s.prof.info
# str(s.prof.info) ### Make sure that data is numeric (double) or integer
names(s.prof.info)
glimpse(s.prof.info)

####################################
### Site Description
site.descr <- readxl::read_xlsx(
  path = paste0(path_, file_),
  sheet = "Site Description",
  # col_types = "numeric",
  # .name_repair = "unique"
  # .name_repair = "universal"
  .name_repair = "universal_quiet" 
  
)
site.descr
names(site.descr)
glimpse(site.descr)

####################################
### Horizons
horiz <- readxl::read_xlsx(
  path = paste0(path_, file_),
  sheet = "Horizons",
  # col_types = "numeric",
  # .name_repair = "unique"
  # .name_repair = "universal"
  .name_repair = "universal_quiet" 
)
horiz
names(horiz)
glimpse(horiz)

#########################################################
## Select and organize lines (rows) of interest: 
s.prof.info
site.descr
horiz

# library(tidyverse)
# full_join()

### Test join with small example
df1 <- s.prof.info[1:10, 1:15]
df2 <- site.descr[1:10, 1:8]
df3 <- horiz[1:10, 1:8]

df1
df2
df3

df1 %>%
  full_join(df2, by = c("PRID", "Latitude", "Longitude"), keep = F) %>% 
  full_join(df3, by = c("PRID", "Latitude", "Longitude"), keep = F)
# Note that the remaining columns with same name from more than one dataset
# are duplicated and renamed, for example: 'Dataset.x' and 'Dataset.y'

### Join all
df1 <- s.prof.info
df2 <- site.descr
df3 <- horiz

df <- df1 %>%
  full_join(df2, by = c("PRID", "Latitude", "Longitude"), keep = F) %>% 
  full_join(df3, by = c("PRID", "Latitude", "Longitude"), keep = F)
# Note that the remaining columns with same name from more than one dataset
# are duplicated and renamed, for example: 'Dataset.x' and 'Dataset.y'

df |> glimpse()
df1 |> glimpse()
df2 |> glimpse()
df3 |> glimpse()
df3 |> skim()
# df$Status

# pH, texture, EC, base saturation, OC, CaCO3, and soil classes.

df <- df[ , c(
"PRID",
"Dataset",
"HONU",
# "Year",
# "Coordinate.Reference.System",
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
"Upper.Depth.cm",
"Lower.Depth.cm",
"Sand......53.µm....SDTO.",
# "Silt......2.53µm....STPC.",
"Clay........2.µm.....CLPC.",
# "Silt...Clay..",
# "Texture..Code....PSCL.",
"Bulk.Density.kg.dm3",
"pH.water..PHAQ.",
# "pH.water.other.lab",
# "Electrical.Conductivity.µS.cm..2.soil.5.water.suspension...EL25.",
"Electrical.Conductivity.dS.m..2.soil.5.water.suspension...EL25.",
# "Electrical.Conductivity.µS.cm..Saturated.Paste.Extract...ELCO.",
# "Electrical.Conductivity.dS.m..Saturated.Paste.Extract...ELCO.",
# "Total.Carbonate.Equivalent.g.kg...TCEQ.",
# "Carbonate.Estimate......46",
# "CEC.Effective...cmol....kg...Sum.of.Exchangeable.Bases.",
# "CEC.Measured...cmol....kg...",
"Base.Saturation.Estimate.......from.pHwater.",
# "Base.Saturation........CECeffective.CECmeasured.",
"Organic.Carbon.."
# ,
# "Organic.Carbon.g.kg",
# "Organic.Matter....OC.1.74.",
# "Horizon.Code.Short",
# "Horizon.Code.Original",
# "Munsell_col_moist..",
# "Hue_moist",
# "Value_moist",
# "Chroma_moist",
# "Colour.Moist.Desc..Munsell.",
# "Munsell_col_dry",
# "Hue_dry",
# "Value_dry",
# "Chroma_dry"
)]

library(skimr)
skim(df) %>%
  select(-numeric.sd,
         -numeric.p0, 
         -numeric.p25, 
         -numeric.p50,
         -numeric.p75,
         -numeric.p100)

df |> as.data.frame()
df |> glimpse()

##############################
### Rename
names(df) <- c("PRID", "Dataset", "HONU", "Latitude", 
               "Longitude", "Surveyor", "WRB2015", 
               "Upper.Depth", "Lower.Depth", "Sand", 
               "Clay", "BD", "pH", "EC", "BS", "OC") 
names(df)

dfc <- df[!is.na(df$BS), ]
dfc$BS

df$BS <- gsub("90 - 100", "95", df$BS)
df$BS <- gsub("80 - 90", "85", df$BS)

table(unique(df$BS))
df$BS <- as.integer(df$BS)

df$Latitude <- as.numeric(df$Latitude)
df$Longitude <- as.numeric(df$Longitude)

library(tidyr)
# Use drop_na() to drop rows with missing values
df <- drop_na(df, Latitude)
df <- drop_na(df, Longitude)
df <- drop_na(df, HONU)
df

# skim(df)

### Get Namibia shapefile 
# Convert to spatial vector object 
library(terra)
# install.packages("rnaturalearth")
# library(rnaturalearth)
# namibia <- ne_countries(country = "Namibia")

# install.packages("geodata")
library(geodata)
# country_codes()
country_codes(query="Nam")
namibia_shp <- geodata::gadm(country = "NAM", level=0)
# plot(namibia_shp)

################################################################################
### Plot the Top hor.

# Replace NA with zero in the "Value" column
# df$Value[is.na(df$Value)] <- 0

# Replace all NAs with zeros in the entire data frame
# df_non_na <- replace(df, is.na(df), 0)

# Specify the range of columns to replace (Sand, Clay, BD, pH, EC, BS, OC)
df[1:3, c(3, 10:16)] 

df_non_na <- df
# Loop through the specified columns and replace NAs with zeros
for (col_index in c(10:16)) {
  df_non_na[, col_index][is.na(df_non_na[, col_index])] <- 0
}

# df_non_na <- df_non_na |>
#   filter(HONU != 1)

df_non_na <- df_non_na |>
  filter(HONU == 1) # == 1 for To; and != 1 for Subsurface

### Combinations and hierarchy
lis_ <- list(
  # c("WRB2015"),
  c("OC"),
  c("pH"),
  c("BS"),
  c("Sand"),
  c("Clay"),
  c("EC"),
  c("BD"),
  c("OC", "pH"),
  c("OC", "pH", "BS"),
  c("OC", "pH", "BS", "Sand"),
  c("OC", "pH", "BS", "Sand", "Clay"),
  c("OC", "pH", "BS", "Sand", "Clay", "EC"),
  c("OC", "pH", "BS", "Sand", "Clay", "EC", "BD")
)

lis_

skim(df_non_na)

##### for test #####
# lis_ <- c("OC", "pH") 
##### for test only #####

for (i in lis_) {
# print(
  dff <- df_non_na[ , paste0(c("PRID", 
                               "HONU",
                               "Latitude", "Longitude", i))]
  # )  
# rm(dff)
##### for test #####
# dff <- df_non_na[ , paste0(c("PRID", "HONU", "Latitude", "Longitude", "OC", "EC", "pH"))]
# dff <- df_non_na[ , paste0(c("PRID", "HONU", "Latitude", "Longitude", "OC", "pH", "BS", "Sand", "Clay", "EC", "BD"))]
##### for test #####
# md <- dff %>%
#   group_by(PRID) %>%
#   summarize(across(-c(HONU), mean))
# Group by "PRID" and calculate the mean for all columns except "HONU"
md <- aggregate(. ~ PRID, data = dff[, -which(names(dff) %in% "HONU")], mean)
# md

# columns_to_filter <- c("OC", "EC", "pH")  # Add or remove column names as needed
# filtered_data <- md %>%
#   filter(if_any(all_of(columns_to_filter), ~ . != 0))

# filtered_data <- md %>%
#   filter(OC != 0, EC != 0, pH != 0)

### Keep the line if any is non zero
# md <- md %>%
  # filter_at(vars(-PRID, -Latitude, -Longitude), any_vars(. != 0))
### Keep the line if all are non zero
md <- md %>%
  filter_at(vars(-PRID, -Latitude, -Longitude), all_vars(. != 0))

# skim(md)
# md |> as.data.frame()
# table(md$OC) |> length()

sampl_point <- terra::vect(md, 
                           geom=c("Longitude", "Latitude"), # col names of geometry
                           crs= "+proj=longlat +datum=WGS84 +no_defs", 
                           # crs= "epsg:4326", 
                           keepgeom=T)
# type="polygons")
#################################################
# Create the plot and save it as a PNG image
png(paste0("../../output/plots/exploratory_spatial_distrib_soil_prop_top_bottom_depth/Top_hor_Soil_prop_", paste(i, collapse = "_"), "_num_pnts_", nrow(md), ".png"), 
    width = 1200, height = 1400, res = 200)
title_ <- paste0("Soil prop.: ", paste(i, collapse = ", "))
terra::plot(main= title_, namibia_shp, axes= F)
terra::plot(sampl_point, add= T, cex= 0.1, axes= F, pch= 19, col= "blue")
# mtext(paste(i, collapse = ", "), side = 1, line = 1)
mtext(paste0("Top hor. samples \n all with data,", " num points: ", nrow(md)), side = 1, line = 1)

dev.off()  # Close the PNG device to save
#################################################
### Export data table
write_xlsx(
  md,
  path = paste0("../../output/datasets/exploratory_spatial_distrib_soil_prop_top_bottom_depth/Top_hor_Soil_prop_", paste(i, collapse = "_"), "_num_pnts_", nrow(md), ".xlsx"),
  col_names = TRUE,
  format_headers = TRUE#,
  # use_zip64 = FALSE
)

# Export as .shp / .gpkg
writeVector(sampl_point, paste0("../../output/shapes/Top_hor_Soil_prop_", paste(i, collapse = "_"), "_num_pnts_", nrow(md), ".shp"), overwrite= TRUE)
# writeVector(sampl_point, paste0("../output/Soil_prop_", paste(i, collapse = "_"), ".gpkg"), overwrite=TRUE)

} # end of loop



################################################################
### For "WRB2015"
################################################################
dff <- df[ , paste0(c("PRID", "HONU", "Latitude", "Longitude", "WRB2015"))]

dff <- dff |>
  filter(HONU == 1)

library(tidyverse)
dff <- drop_na(dff, WRB2015)

md <- dff

sampl_point <- terra::vect(md,
                           geom=c("Longitude", "Latitude"), # col names of geometry
                           crs= "+proj=longlat +datum=WGS84 +no_defs",
                           # crs= "epsg:4326",
                           keepgeom=T)
# type="polygons")
i <- "WRB2015"
#################################################
# Create the plot and save it as a PNG image
png(paste0("../../output/plots/exploratory_spatial_distrib_soil_prop_top_bottom_depth/", paste(i, collapse = "_"), "_num_pnts_", nrow(md), ".png"), 
    width = 1200, height = 1400, res = 200)
title_ <- paste0("Soil prop.: ", paste(i, collapse = ", "))
terra::plot(main= title_, namibia_shp, axes= F)
terra::plot(sampl_point, add= T, cex= 0.1, axes= F, pch= 19, col= "blue")
# mtext(paste(i, collapse = ", "), side = 1, line = 1)
mtext(paste0("WRB2015 locations, \n", " num points: ", nrow(md)), side = 1, line = 1)

dev.off()  # Close the PNG device to save
#################################################
### Export data table
write_xlsx(
  md,
  path = paste0("../../output/datasets/exploratory_spatial_distrib_soil_prop_top_bottom_depth/", paste(i, collapse = "_"), "_num_pnts_", nrow(md), ".xlsx"),
  col_names = TRUE,
  format_headers = TRUE#,
  # use_zip64 = FALSE
)

# Export as .shp / .gpkg
writeVector(sampl_point, paste0("../../output/shapes/WRB2015_", paste(i, collapse = "_"), "_num_pnts_", nrow(md), ".shp"), overwrite= TRUE)

################################
################################
################################
################################

