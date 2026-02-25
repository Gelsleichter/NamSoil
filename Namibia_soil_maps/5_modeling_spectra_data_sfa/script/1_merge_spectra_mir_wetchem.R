###_____________________________________________________________________________
### Magyar Agrar- es Elettudomanyi Egyetem - MATE
### Modeling spectral data to predict soil properties, TIM database
### Nov 2025 
### Yuri Andrei Gelsleichter, Adam Csorba
###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨

###_____________________________________________________________________________
# +--- slant ::: https://www.askapache.com/online-tools/figlet-ascii/
#     __  ___________        _    ___            _   __________ 
#    /  |/  /  _/ __ \      | |  / (_)____      / | / /  _/ __ \
#   / /|_/ // // /_/ /      | | / / / ___/_____/  |/ // // /_/ /
#  / /  / // // _, _/       | |/ / (__  )_____/ /|  // // _, _/ 
# /_/  /_/___/_/ |_|        |___/_/____/     /_/ |_/___/_/ |_|  
###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨

### Set working directory automatically ----
library(this.path)
path_ <- this.dir()
setwd(path_)
getwd()
# clean memory
# gc(); rm(list=ls())

library(dplyr)
library(readxl)
library(readr)

### Loading Coordinates, MIR, Wet chem ----
wchem <- readxl::read_xlsx("../input/S4A_Wetchem_20240911_NA.xlsx", sheet=1, n_max=63)
coord <- readxl::read_xlsx("../input/s4a_submissions_Namibia_2023-09-28.xlsx", sheet=1)
spectra <- readr::read_csv("../input/spectral_data_Namibia_20250716.csv")

names(wchem)[1:3] <- c("lab_code", "qr_code", "layer_id")
wchem$plot_code <- gsub("[TS]$", "", wchem$layer_id)
wchem <- wchem |> 
  dplyr::relocate(plot_code, .after = layer_id)

# Correct layer_id typos in wetchem, from "Na026" to "NA026"
wchem$layer_id <- gsub("Na026", "NA026", wchem$layer_id)

# Check dimensions and names
dim(wchem)
dim(spectra)
names(wchem)
names(spectra)

wchem$layer_id
spectra$layer_id

### Merge wetchem and spectra ----
wchem_spc <- wchem |> 
  dplyr::select(-qr_code, -plot_code) |> # to avoid duplicated columns (.x and .y cols)
  dplyr::full_join(spectra, by = "layer_id", keep=NULL) |> as.data.frame()
# wchem_spc[1:5, c(1:70)]

### Merge wetchem_spectra and coordinates ----
# Rename column 
# colnames(coord)[colnames(coord) == "sampling_point_id"] <- "plot_code"
coord <- coord |> 
  dplyr::rename(plot_code = sampling_point_id)

wchem_spc <- wchem_spc |>
  dplyr::mutate(.rowid_coord = row_number())

sdb <- coord |>
  # Add a temporary ID to preserve original order of coord
  dplyr::select(plot_code, gps_latitude, gps_longitude, gps_altitude, gps_precision) |>
  # Make the join (will mess up the order)
  dplyr::full_join(wchem_spc, by = "plot_code", relationship = "many-to-many") |>
  # reboot the order with arrange
  # dplyr::arrange(.rowid_coord, layer_id) |>
  dplyr::arrange(.rowid_coord) |>
  dplyr::select(-.rowid_coord) |>           # remove o helper
  as.data.frame()

### Creat ID for Train_CV, Predict and Extra_points ----
sdb$data_type <- rep(c("Train_CV", "Pred", "Extra_points"), c(65, 128, 31))
sdb <- sdb |>
  dplyr::relocate(data_type, .after = layer_id)
sdb[1:5, c(1:70)]
names(sdb)[1:70]
colnames(sdb)[70:ncol(sdb)]
colnames(sdb)[70:ncol(sdb)] <- paste0("mir_", colnames(sdb)[70:ncol(sdb)])

### Percent of missing values in wetchem columns ----
names(sdb)[9:59] |> as.data.frame()
sort(round(sapply(sdb[,9:59], function(x) mean(x == 0, na.rm = TRUE)) * 100, 2), decreasing = T)

###_____________________________________________________________________________
### Organize columns ----
###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
meta_cols <- c("plot_code", "layer_id", "upper_depth", "lower_depth", "sample_depth", 
               "gps_latitude", "gps_longitude", "data_type")

chems <- c("Fe_ox", "Al_ox", "Al", "B", "Ca", "Cu", "Fe", "K", "Mg", "Mn", "Na", 
           "P", "S", "Zn", "coSa", "meSa", "fiSa", "vfiSa", "Sa", "coSi", 
           "fiSi", "Si", "Clay", "pH-H2O", "pH-KCL", "EC", "CaCO3", "inorgC", 
           "totC", "orgC", "totN", "exchCa", "exchMg", "exchK", "exchNa", 
           "CEC", "exchH", "exchAl", "BaseSat", "exchAcid", "totMo", "totCd", 
           "totPb", "totV", "totHg", "totCr", "totCo", "totNi", "totCu", 
           "totZn", "totAs", "totSb")

sdb <- sdb |> 
  dplyr::select(dplyr::all_of(c(meta_cols, chems)), dplyr::starts_with("mir_")) |> 
  dplyr::select(-exchAl, -exchNa, -totMo, -inorgC, -S, -totHg) # drop cols with more than 20% of zeros 

### Create an ID column ----
sdb$id <- 1:nrow(sdb)
sdb <- sdb |>
  dplyr::relocate(id, .before = plot_code)


### Write on disk ----
write.csv(sdb,
          file = "../output/dataset/merged_wc_mir.csv", 
          row.names = F)

###_____________________________________________________________________________
### Write notes ----
###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨

writeLines(
  "
Notes on data arrangement and issues:

1- QR-code has problems caused in database or excel, see below:

  layer_id  qr_code.wchem  qr_code.spectra
NA024-1-1T      NAM-HITNI       uNAM-HITNI
NA024-1-1S      NAM-UTD9i       uNAM-UTD9i
NA054-3-1T      NAM-nRsA0        NAM-nRsAO

2- The 'exchAl' has (100%) zero values for all, therefore will be skipped in modeling, together with:
exchNa: 78.5%
totMo: 66.2%
inorgC: 30.8%
S: 29.23%
totHg: 27.7%
CaCO3: 23.1%, from here onwards was keept to try modeling:
P: 18.5%
totCd: 12.3%
fiSi: 6.2%
exchH: 6.2%  
exchAcid: 6.15%
Zn: 3.1%
coSa: 1.5%
Clay: 1.5%
totCr: 1.5%

3- In layer_id typos in wetchem were fix, from 'Na026' to 'NA026'

4- These samples had no corrdinates and were attached from other source.
NA006-1-1, NA025-1-2, NA050-1-1, NA060-6-2

5- During the field work samples were taken from extra points, therefore they were not analyezed in lab.
So, the corrdiantes are present but no wetchem and spectral data are not.
The soil samples are still stored and can be analysed in future. They are: 
NA002-3-2, NA002-3-3, NA002-2-2, NA002-2-3, NA002-4-2, NA002-4-3, NA002-5-3, NA002-5-2, 
NA003-2-1, NA003-2-3, NA036-1-2, NA036-4-3, NA003-4-2, NA003-3-1, NA003-3-3, NA089-1-3, 
NA089-1-1, NA089-4-1, NA089-4-1, NA089-2-1, NA089-2-3, NA089-3-2, NA089-3-3, NA036-2-3, 
NA003-1-2, NA003-1-3, NA003-4-3, NA036-2-2, NA036-3-3, NA036-3-2, NA036-4-2

",
  "../output/metadata/Notes_qr_code_discrepancy_exchAl_coordinates.txt" 
)

###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
### End script ----
###_____________________________________________________________________________
