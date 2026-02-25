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
  sheet = "Prof Registration",
  # col_types = "numeric",
  # .name_repair = "unique"
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
  .name_repair = "universal_quiet"
)
horiz
names(horiz)
glimpse(horiz)

################################################################################
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
library(skimr)

df |> glimpse()
df1 |> glimpse()
df2 |> glimpse()
df3 |> glimpse()
# df1 |> skim()
# df$Status

# pH, texture, EC, base saturation, OC, CaCO3, and soil classes.

df <- df[ , c(
"PRID",
"Dataset",
"HONU",
"Year",
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
names(df) <- c("PRID", "Dataset", "HONU", "Year", "Latitude", 
               "Longitude", "Surveyor", "WRB2015", 
               "Upper.Depth", "Lower.Depth", "Sand", 
               "Clay", "BD", "pH", "EC", "BS", "OC") 
names(df)
df$Year |> table()

################################################################################
### check the most common dates of soil collection 
sort(table(s.prof.info$Year), decreasing= T)

# Assuming 'numbers' is your vector of numbers
# all data
frequencies <- table(s.prof.info$Year)  # Calculate the frequency of each number

# filtering Upper.Depth == 0 
library(dplyr)
filt_df <- df %>%
  filter(Upper.Depth == 0)

# Filter out rows where OC is missing and Upper.Depth is 0
library(tidyr)
filt_df <- filt_df %>% drop_na(OC)

### with OC data only
frequencies <- table(filt_df$Year)  # Calculate the frequency of each number
sorted_frequencies <- sort(frequencies, decreasing = TRUE)  # Sort frequencies in descending order

# Calculate the upper limit for the y-axis
ymax <- max(sorted_frequencies) * 1.25 # To give some space above the highest bar

# Create the bar plot with adjusted y-axis limits
barplot(sorted_frequencies, main = "Most Common Soil Sample Years in Namibia", 
        xlab = "Years", 
        ylab = "Frequency", 
        col = "green", 
        # las = 2, 
        xaxt = "n",
        ylim = c(0, ymax))

# Adding labels 
# text(x = barplot(sorted_frequencies, plot = FALSE), y = -50, 
#      labels = names(sorted_frequencies), 
#      srt = 45, pos = 1, xpd = TRUE, cex = 1.1)
### or
bp <- barplot(sorted_frequencies, plot = FALSE)
text(x = bp - 0.5, y = -50, labels = names(sorted_frequencies), 
     srt = 45, pos = 1, xpd = TRUE, cex = 1.1)

# Add the values on top of each bar
text(x = bp, y = sorted_frequencies + 5, 
     labels = sorted_frequencies, 
     pos = 3, cex = 0.8)

dev.off()

df |> skim()
filt_df |> skim()

### Check NA and deal with BD
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

###################################################################
### BD has only 79 samples (horiz.), thus, not selected 
###################################################################
df <- df %>% 
  select(-BD, -Surveyor)

### First drop all lines with any NA
skim(df)
df_non_na <- drop_na(df)

### Then select the 1st horizon
df_non_na <- df_non_na |>
  filter(HONU == 1)

skim(df_non_na)

df_non_na$WRB2015 |> unique()

df_non_na$WRB2015 |> table()

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
### Plot the soil classes with properties 

df_non_na[df_non_na$WRB2015 == "Arenosol", ]

df_non_na$WRB2015 |> table()

df_non_na$WRB2015 |> unique()

### The Soil class: "Calcisol / Cambisol" is giving a problem because of the bar, thus we have to fix it
df_non_na[df_non_na$WRB2015 == "Calcisol / Cambisol", "WRB2015"] <- "Calcisol_Cambisol"

df_non_na$WRB2015 |> unique()

soil_list <- df_non_na$WRB2015 |> unique()
soil_list

################################################################################
### Create, full data, shape file, and plot for each soil classes
################################################################################

for (i in soil_list) {
  md <- df_non_na[df_non_na$WRB2015 == i, ]

md

sampl_point <- terra::vect(md, 
                           geom=c("Longitude", "Latitude"), # col names of geometry
                           crs= "+proj=longlat +datum=WGS84 +no_defs", 
                           # crs= "epsg:4326", 
                           keepgeom=T)
# type="polygons")
#################################################
# Create the plot and save it as a PNG image
png(paste0("../../output/plots/exploratory_spatial_distrib_soil_prop_top_bottom_depth/Soil_class_complete_data_", paste(i, collapse = "_"), "_num_pnts_", nrow(md), ".png"), 
    width = 1200, height = 1200, res = 200)
title_ <- paste0("Soil class: ", paste(i, collapse = ", "))
terra::plot(main= title_, namibia_shp, axes= F)
terra::plot(sampl_point, add= T, cex= 0.1, axes= F, pch= 19, col= "blue")
# mtext(paste(i, collapse = ", "), side = 1, line = 1)
mtext(paste0("Soil class with \n all with data,", " num points: ", nrow(md)), side = 1, line = 1)

dev.off()  # Close the PNG device to save
#################################################
### Export data table
write_xlsx(
  md,
  path = paste0("../../output/datasets/exploratory_spatial_distrib_soil_prop_top_bottom_depth/Soil_class_complete_data_", paste(i, collapse = "_"), "_num_pnts_", nrow(md), ".xlsx"),
  col_names = TRUE,
  format_headers = TRUE#,
  # use_zip64 = FALSE
)

# Export as .shp / .gpkg
writeVector(sampl_point, paste0("../../output/shapes/Soil_class_complete_data_", paste(i, collapse = "_"), "_num_pnts_", nrow(md), ".shp"), overwrite= TRUE)
# writeVector(sampl_point, paste0("../output/Soil_prop_", paste(i, collapse = "_"), ".gpkg"), overwrite=TRUE)

} # end of loop



################################################################################
### Create, full data, shape file, and plot for ALL soil classes
################################################################################
library(tidyverse)
skim(df)
df_non_na <- drop_na(df)
skim(df_non_na)

md <- df_non_na |>
  filter(HONU == 1)

md$HONU

sampl_point <- terra::vect(md,
                           geom=c("Longitude", "Latitude"), # col names of geometry
                           crs= "+proj=longlat +datum=WGS84 +no_defs",
                           # crs= "epsg:4326",
                           keepgeom=T)
# type="polygons")
i <- "All Soil classes WRB2015"
#################################################
# Create the plot and save it as a PNG image
png(paste0("../../output/plots/exploratory_spatial_distrib_soil_prop_top_bottom_depth/All_WRB2015_", paste(i, collapse = "_"), "_num_pnts_", nrow(md), ".png"), 
    width = 1200, height = 1200, res = 200)
title_ <- paste0("Soil prop.: ", paste(i, collapse = ", "))
terra::plot(main= title_, namibia_shp, axes= F)
terra::plot(sampl_point, add= T, cex= 0.1, axes= F, pch= 19, col= "blue")
# mtext(paste(i, collapse = ", "), side = 1, line = 1)
mtext(paste0("WRB2015 locations complete data, \n", " num points: ", nrow(md)), side = 1, line = 1)

dev.off()  # Close the PNG device to save
#################################################
### Export data table
write_xlsx(
  md,
  path = paste0("../../output/datasets/exploratory_spatial_distrib_soil_prop_top_bottom_depth/All_WRB2015_", paste(i, collapse = "_"), "_num_pnts_", nrow(md), ".xlsx"),
  col_names = TRUE,
  format_headers = TRUE#,
  # use_zip64 = FALSE
)

# Export as .shp / .gpkg
writeVector(sampl_point, paste0("../../output/shapes/All_WRB2015_", paste(i, collapse = "_"), "_num_pnts_", nrow(md), ".shp"), overwrite= TRUE)

################################
################################
################################

################################################################################
### Merge all data with complete properties
################################################################################

md

### To merge all dataframes, they must be the same datatype
md$Latitude <- as.character(md$Latitude)
md$Longitude <- as.character(md$Longitude)

df_full <- md %>%
  left_join(df1, by = c("PRID", "Latitude", "Longitude"), keep = F) %>% 
  left_join(df2, by = c("PRID", "Latitude", "Longitude"), keep = F) %>% 
  left_join(df3, by = c("PRID", "Latitude", "Longitude"), keep = F)
df_full

glimpse(df1)
glimpse(df2)
glimpse(df3)
glimpse(df_full)

df_full$WRB2015 |> unique()

df_full$WRB2015 |> table()

### Export data table
write_xlsx(
  df_full,
  path = paste0("../../output/datasets/exploratory_spatial_distrib_soil_prop_top_bottom_depth/Full_join_All_WRB2015_num_pnts_", nrow(md), ".xlsx"),
  col_names = TRUE,
  format_headers = TRUE#,
  # use_zip64 = FALSE
)
