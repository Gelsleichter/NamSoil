###_____________________________________________________________________________
### Magyar Agrar- es Elettudomanyi Egyetem - MATE
### Modeling spectral data to predict soil properties, SFA Namibia
### Nov 2025 
### Yuri Andrei Gelsleichter, Marina Coetzee
###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨

### Set working directory automatically 
# Load library
library(this.path)
path_ <- this.dir()
setwd(path_)

# verify the working directory
getwd()

# clean memory and workspace
gc();rm(list=ls())
options(scipen = 0) # turn off scientific notation

###_____________________________________________________________________________
### Load Sampling points ----
###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
nam <- read.csv("../output/sfa_points_nam_maps.csv")
sfa <- read.csv("../output/s4a_wet_chem_mir_predicted_values.csv")
isda <- read.csv("../output/sfa_points_isda.csv")
sgri <- read.csv("../output/sfa_points_soilgrids.csv")
unts <- read.csv("../input/units_conversion.csv")[1:16, ]

unique(nam$layer_id) # the nam has 111 samples as layers are placed horizontally (side by side, per columsn)
unique(sfa$layer_id) # the sfa has 191 samples as the 0-20 and 20-50 cm layers are placed vertically (line by line)
unique(isda$layer_id) # the isda has 111 samples as layers are placed horizontally (side by side, per columsn)
unique(sgri$layer_id) # the Soil Grids has 111 samples as layers are placed horizontally (side by side, per columsn)

###_____________________________________________________________________________
### Convert S4A ----
###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
# sfa$BaseSat <- sfa$BaseSat  *1     # pct
sfa$Ca <- sfa$Ca            *200   # cmol(c)/kg	(dropped because the wet chem analyses used other method)
# sfa$CEC <- sfa$CEC          *1     # cmol(c)/kg
# sfa$Clay <- sfa$Clay        *1     # pct
# sfa$EC <- sfa$EC            *10    # mS/m (seems wrong unit, to match values *100)
sfa$EC <- sfa$EC            *100    # mS/m (seems wrong unit, to match values *100)
sfa$K <- sfa$K              *390   # cmol(c)/kg	(dropped because the wet chem analyses used other method)
sfa$Mg <- sfa$Mg            *122   # cmol(c)/kg	(dropped because the wet chem analyses used other method)
sfa$Na <- sfa$Na            *230   # cmol(c)/kg	(dropped because the wet chem analyses used other method)
# sfa$totN <- sfa$totN        *1000  # g/kg (seems wrong unit, to match values *10000)
sfa$totN <- sfa$totN        *10000  # g/kg (seems wrong unit, to match values *10000)
# sfa$orgC <- sfa$orgC        /10    # g/kg	(seems to be already in %)
# sfa$orgC <- sfa$orgC        /1    # g/kg	(seems to be already in %)
# sfa$pH.H2O <- sfa$pH.H2O    *1     # NULL
# sfa$P <- sfa$P              *1     # mg/kg
# sfa$Sa <- sfa$Sa            *1     # pct
# sfa$Si <- sfa$Si            *1     # pct
sfa_cnv_unts <- sfa

###_____________________________________________________________________________
### Convert iSDA ----
###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
isda[, grep("^sol_log.oc", names(isda))] <- (expm1(isda[, grep("^sol_log.oc", names(isda))] / 10))/10
# isda[, grep("^sol_sand_tot", names(isda))] <- isda[, grep("^sol_sand_tot", names(isda))] * 1
# isda[, grep("^sol_silt_tot", names(isda))] <- isda[, grep("^sol_silt_tot", names(isda))] * 1
# isda[, grep("^sol_clay_tot", names(isda))] <- isda[, grep("^sol_clay_tot", names(isda))] * 1
isda[, grep("^sol_ph_h2o", names(isda))] <- isda[, grep("^sol_ph_h2o", names(isda))] / 10
isda[, grep("^sol_log.n_tot", names(isda))] <- (expm1(isda[, grep("^sol_log.n_tot", names(isda))] / 100))*1000
isda[, grep("^sol_db_od", names(isda))] <- isda[, grep("^sol_db_od", names(isda))] /100
isda[, grep("^sol_log.k", names(isda))] <- expm1(isda[, grep("^sol_log.k", names(isda))] /10)
isda[, grep("^sol_log.mg", names(isda))] <- expm1(isda[, grep("^sol_log.mg", names(isda))] /10)
isda[, grep("^sol_log.ca", names(isda))] <- expm1(isda[, grep("^sol_log.ca", names(isda))] /10)
isda[, grep("^sol_log.p", names(isda))] <- expm1(isda[, grep("^sol_log.p", names(isda))] /10)

###_____________________________________________________________________________
### Convert Soil Grids ----
###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
sgri[, grep("^soc", names(sgri))] <- sgri[, grep("^soc", names(sgri))] /100
sgri[, grep("^sand", names(sgri))] <- sgri[, grep("^sand", names(sgri))] /10
sgri[, grep("^silt", names(sgri))] <- sgri[, grep("^silt", names(sgri))] /10
sgri[, grep("^clay", names(sgri))] <- sgri[, grep("^clay", names(sgri))] /10
sgri[, grep("^phh2o", names(sgri))] <- sgri[, grep("^phh2o", names(sgri))] /10
sgri[, grep("^nitrogen", names(sgri))] <- sgri[, grep("^nitrogen", names(sgri))] *10
sgri[, grep("^cec", names(sgri))] <- sgri[, grep("^cec", names(sgri))] /10
sgri[, grep("^bdod", names(sgri))] <- sgri[, grep("^bdod", names(sgri))] /100

###_____________________________________________________________________________
### Depth adjust ----
###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
library(tidyr)
library(dplyr)

# Namibia
nam_cnv_unts <- nam |> 
  tidyr::pivot_longer(cols = starts_with(c(
    "BDkgdm3", "BSpct", "Camgkg", "CECcmolkg", "Clay2umpct", "ECuScmEL25", "ECuScmELCO", 
    "Kmgkg", "Mgmgkg", "Namgkg", "Nmgkg", "OCpct", "pHwater", "Pmgkg", "Sand53umpct", "Silt253umpct")),
                      names_to = c("variable", "depth"),
                      names_pattern = "(.*)_(\\d+_\\d+)_cm_mean",
                      values_to = "value") |> 
  tidyr::pivot_wider(names_from = variable, values_from = value) |> as.data.frame()

### nam_cnv_unts$depth <- gsub("cm$", "", nam_cnv_unts$depth)

nam_cnv_unts <- nam_cnv_unts |> 
  tidyr::separate(depth, into = c("upper_depth", "lower_depth"), sep = "_", convert = TRUE)

# iSDA
isda_cnv_unts <- isda |> 
  tidyr::pivot_longer(cols = starts_with(c(
    "sol_log.oc", "sol_sand_tot", "sol_silt_tot", "sol_clay_tot", "sol_ph_h2o", 
    "sol_log.n_tot", "sol_db_od", "sol_log.k", "sol_log.mg", "sol_log.ca", "sol_log.p")),
                      names_to = c("variable", "depth"),
                      # names_pattern = "(.*)_(.*cm)_mean",
                      # names_pattern = "(.*)_(0\\.\\.20cm|20\\.\\.50cm|50\\.\\.100cm|100\\.\\.200cm|0\\.\\.5cm|5\\.\\.15cm|15\\.\\.30cm|30\\.\\.60cm|60\\.\\.100cm)_.*",
                      names_pattern = "(sol_[^_]+).*_(0\\.\\.\\d+cm|20\\.\\.50cm|50\\.\\.100cm|100\\.\\.200cm).*",
                      # names_pattern = "^(.*)_m_30m_(.*)", 
                      values_to = "value") |> 
  tidyr::pivot_wider(names_from = variable, values_from = value) |> as.data.frame()

isda_cnv_unts$depth <- gsub("cm$", "", isda_cnv_unts$depth)

isda_cnv_unts <- isda_cnv_unts |> 
  tidyr::separate(depth, into = c("upper_depth", "lower_depth"), sep = "\\..", convert = TRUE)

# Soil Grids
sgri_cnv_unts <- sgri |> 
  tidyr::pivot_longer(cols = starts_with(c("soc", "sand", "silt", "clay", "phh2o", 
                                           "nitrogen", "cec", "bdod")),
                      names_to = c("variable", "depth"),
                      names_pattern = "(.*)_(.*cm)_mean",
                      values_to = "value") |> 
  tidyr::pivot_wider(names_from = variable, values_from = value) |> as.data.frame()

sgri_cnv_unts$depth <- gsub("cm$", "", sgri_cnv_unts$depth)
 
sgri_cnv_unts <- sgri_cnv_unts |> 
  tidyr::separate(depth, into = c("upper_depth", "lower_depth"), sep = "\\.", convert = TRUE)

###_____________________________________________________________________________
### Rename cols ----
###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨

nam_cnv_unts <- nam_cnv_unts |> dplyr::rename(Soil.ID = layer_id,
                                              Upper.Boundary = upper_depth, 
                                              Lower.Boundary = lower_depth, 
                                              BD.kgdm3_nam = BDkgdm3, 
                                              BS.pct_nam = BSpct, 
                                              Ca.mgkg_nam = Camgkg, 
                                              CEC.cmolkg_nam = CECcmolkg, 
                                              Clay.pct_nam = Clay2umpct, 
                                              EC.uScm_nam = ECuScmEL25, 
                                              EC.uScmELCO_nam = ECuScmELCO, 
                                              K.mgkg_nam = Kmgkg, 
                                              Mg.mgkg_nam = Mgmgkg, 
                                              Na.mgkg_nam = Namgkg, 
                                              N.mgkg_nam = Nmgkg, 
                                              OC.pct_nam = OCpct, 
                                              pH.water_nam = pHwater, 
                                              P.mgkg_nam = Pmgkg, 
                                              Sand.pct_nam = Sand53umpct, 
                                              Silt.pct_nam = Silt253umpct) |> dplyr::select(Soil.ID,
                                                                                            Upper.Boundary,
                                                                                            Lower.Boundary,
                                                                                            BD.kgdm3_nam,
                                                                                            BS.pct_nam,
                                                                                            Ca.mgkg_nam,
                                                                                            CEC.cmolkg_nam,
                                                                                            Clay.pct_nam,
                                                                                            EC.uScm_nam,
                                                                                            EC.uScmELCO_nam,
                                                                                            K.mgkg_nam,
                                                                                            Mg.mgkg_nam,
                                                                                            Na.mgkg_nam,
                                                                                            N.mgkg_nam,
                                                                                            OC.pct_nam,
                                                                                            pH.water_nam,
                                                                                            P.mgkg_nam,
                                                                                            Sand.pct_nam,
                                                                                            Silt.pct_nam)

sfa_cnv_unts <- sfa_cnv_unts |> dplyr::rename(Soil.ID = layer_id,
                                              Upper.Boundary = upper_depth, 
                                              Lower.Boundary = lower_depth, 
                                              ### BD.kgdm3_sfa = BDkgdm3, # not present in this dataset
                                              BS.pct_sfa = BaseSat,
                                              Ca.mgkg_sfa = Ca,
                                              CEC.cmolkg_sfa = CEC,
                                              Clay.pct_sfa = Clay,
                                              EC.uScm_sfa = EC,
                                              ### EC.uScmELCO_sfa = ECuScmELCO, # not present in this dataset
                                              K.mgkg_sfa = K,
                                              Mg.mgkg_sfa = Mg,
                                              Na.mgkg_sfa = Na,
                                              N.mgkg_sfa = totN,
                                              OC.pct_sfa = orgC,
                                              pH.water_sfa = pH.H2O,
                                              P.mgkg_sfa = P,
                                              Sand.pct_sfa = Sa,
                                              Silt.pct_sfa = Si) |> dplyr::select(Soil.ID,
                                                                                  Upper.Boundary,
                                                                                  Lower.Boundary,
                                                                                  BS.pct_sfa,
                                                                                  Ca.mgkg_sfa,
                                                                                  CEC.cmolkg_sfa,
                                                                                  Clay.pct_sfa,
                                                                                  EC.uScm_sfa,
                                                                                  K.mgkg_sfa,
                                                                                  Mg.mgkg_sfa,
                                                                                  Na.mgkg_sfa,
                                                                                  N.mgkg_sfa,
                                                                                  OC.pct_sfa,
                                                                                  pH.water_sfa,
                                                                                  P.mgkg_sfa,
                                                                                  Sand.pct_sfa,
                                                                                  Silt.pct_sfa)

isda_cnv_unts <- isda_cnv_unts |> dplyr::rename(Soil.ID = layer_id,
                                                Upper.Boundary = upper_depth, 
                                                Lower.Boundary = lower_depth, 
                                                BD.kgdm3_isda = sol_db,
                                                ### BS.pct_isda = BaseSat, # not present in this dataset
                                                Ca.mgkg_isda = sol_log.ca,
                                                ### CEC.cmolkg_isda = CEC, # not present in this dataset
                                                Clay.pct_isda = sol_clay,
                                                K.mgkg_isda = sol_log.k,
                                                Mg.mgkg_isda = sol_log.mg,
                                                ### Na.mgkg_isda = Na, # not present in this dataset
                                                N.mgkg_isda = sol_log.n,
                                                OC.pct_isda = sol_log.oc,
                                                pH.water_isda = sol_ph,
                                                P.mgkg_isda = sol_log.p,
                                                Sand.pct_isda = sol_sand,
                                                Silt.pct_isda = sol_silt) |> dplyr::select(Soil.ID,
                                                                                           Upper.Boundary,
                                                                                           Lower.Boundary,
                                                                                           BD.kgdm3_isda,
                                                                                           Ca.mgkg_isda,
                                                                                           Clay.pct_isda,
                                                                                           K.mgkg_isda,
                                                                                           Mg.mgkg_isda,
                                                                                           N.mgkg_isda,
                                                                                           OC.pct_isda,
                                                                                           pH.water_isda,
                                                                                           P.mgkg_isda,
                                                                                           Sand.pct_isda,
                                                                                           Silt.pct_isda)

sgri_cnv_unts <- sgri_cnv_unts |> dplyr::rename(Soil.ID = layer_id,
                                                Upper.Boundary = upper_depth, 
                                                Lower.Boundary = lower_depth, 
                                                BD.kgdm3_sgri = bdod,
                                                ### BS.pct_sgri = BaseSat, # not present in this dataset
                                                ### Ca.mgkg_sgri = sol_log.ca, # not present in this dataset
                                                CEC.cmolkg_sgri = cec,
                                                Clay.pct_sgri = clay,
                                                ### K.mgkg_sgri = sol_log.k, # not present in this dataset
                                                ### Mg.mgkg_sgri = sol_log.mg, # not present in this dataset
                                                ### Na.mgkg_sgri = Na, # not present in this dataset
                                                N.mgkg_sgri = nitrogen,
                                                OC.pct_sgri = soc,
                                                pH.water_sgri = phh2o,
                                                ### P.mgkg_sgri = sol_log.p, # not present in this dataset
                                                Sand.pct_sgri = sand,
                                                Silt.pct_sgri = silt) |> dplyr::select(Soil.ID,
                                                                                       Upper.Boundary,
                                                                                       Lower.Boundary,
                                                                                       BD.kgdm3_sgri,
                                                                                       CEC.cmolkg_sgri,
                                                                                       Clay.pct_sgri,
                                                                                       N.mgkg_sgri,
                                                                                       OC.pct_sgri,
                                                                                       pH.water_sgri,
                                                                                       Sand.pct_sgri,
                                                                                       Silt.pct_sgri)


###_____________________________________________________________________________
### Depth interpolation - outlier limtis ----
###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
nam_cnv_unts # reference 0-30, 30-60 cm
sfa_cnv_unts # later had to make 0-20, 20-50 cm the reference, as the spline of 30-60 cm return -9999 when depth is less (20-50 cm) then the established ref depth limit (30-60 cm). 
isda_cnv_unts
sgri_cnv_unts

table(is.na(nam_cnv_unts))
table(is.na(sfa_cnv_unts))
table(is.na(isda_cnv_unts))
table(is.na(sgri_cnv_unts))

# df |> tidyr::drop_na()

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
nam_cnv_unts |> names() |> dput()
sfa_cnv_unts |> names() |> dput()
isda_cnv_unts |> names() |> dput()
sgri_cnv_unts |> names() |> dput()


limits_nam <- outliers_lims_df(nam_cnv_unts, c("Upper.Boundary", "Lower.Boundary", "BD.kgdm3_nam", 
                                               "BS.pct_nam", "Ca.mgkg_nam", "CEC.cmolkg_nam", "Clay.pct_nam", 
                                               "EC.uScm_nam", "EC.uScmELCO_nam", "K.mgkg_nam", "Mg.mgkg_nam", 
                                               "Na.mgkg_nam", "N.mgkg_nam", "OC.pct_nam", "pH.water_nam", "P.mgkg_nam", 
                                               "Sand.pct_nam", "Silt.pct_nam"))
limits_sfa <- outliers_lims_df(sfa_cnv_unts, c("Upper.Boundary", "Lower.Boundary", "BS.pct_sfa", 
                                               "Ca.mgkg_sfa", "CEC.cmolkg_sfa", "Clay.pct_sfa", "EC.uScm_sfa", 
                                               "K.mgkg_sfa", "Mg.mgkg_sfa", "Na.mgkg_sfa", "N.mgkg_sfa", "OC.pct_sfa", 
                                               "pH.water_sfa", "P.mgkg_sfa", "Sand.pct_sfa", "Silt.pct_sfa"))
limits_isda <- outliers_lims_df(isda_cnv_unts, c("Upper.Boundary", "Lower.Boundary", "BD.kgdm3_isda", 
                                                 "Ca.mgkg_isda", "Clay.pct_isda", "K.mgkg_isda", "Mg.mgkg_isda", 
                                                 "N.mgkg_isda", "OC.pct_isda", "pH.water_isda", "P.mgkg_isda", 
                                                 "Sand.pct_isda", "Silt.pct_isda"))
limits_sgri <- outliers_lims_df(sgri_cnv_unts, c("Upper.Boundary", "Lower.Boundary", "BD.kgdm3_sgri", 
                                                 "CEC.cmolkg_sgri", "Clay.pct_sgri", "N.mgkg_sgri", "OC.pct_sgri", 
                                                 "pH.water_sgri", "Sand.pct_sgri", "Silt.pct_sgri"))

limits_nam
limits_sfa
limits_isda
limits_sgri

################################################################################
# Simply the Min Max limits (turn better than the outlier limits above)

get_limits <- function(df) {
  df |> 
    dplyr::summarise(dplyr::across(dplyr::where(is.numeric), 
                     list(min = ~min(., na.rm = TRUE), 
                          max = ~max(., na.rm = TRUE)))) |> 
    t() |> 
    as.data.frame() |> 
    tibble::rownames_to_column(var = "full_name") |> 
    dplyr::mutate(soil_property = sub("_(min|max)$", "", full_name),
           stat = sub(".*_(min|max)$", "\\1", full_name),
           value = V1) |> 
    dplyr::select(soil_property, stat, value) |> 
    tidyr::pivot_wider(names_from = stat, values_from = value) |> 
    dplyr::rename(lower_limit = min, upper_limit = max) |> 
    dplyr::mutate(dplyr::across(c(lower_limit, upper_limit), ~round(., 4)))
}

limits_nam <- get_limits(nam_cnv_unts)
limits_sfa <- get_limits(sfa_cnv_unts)
limits_isda <- get_limits(isda_cnv_unts)
limits_sgri <- get_limits(sgri_cnv_unts)

limits_nam
limits_sfa
limits_isda
limits_sgri

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

dataset <- c("nam_df", "sfa_df", "isda_df", "sgri_df")

for (ds in dataset) {
  print(paste0("Processing dataset: ", ds))

###_____________________________________________________________________________
### Prepare settings to depths interpolation ----
###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨

if(ds == "nam_df") {
  # Namibia national maps
  df <- nam_cnv_unts
  limits_df <- limits_nam
  depths_interp_columns <- c("BD.kgdm3_nam",
                             "BS.pct_nam", "Ca.mgkg_nam", "CEC.cmolkg_nam", "Clay.pct_nam",
                             "EC.uScm_nam", "EC.uScmELCO_nam", "K.mgkg_nam", "Mg.mgkg_nam",
                             "Na.mgkg_nam", "N.mgkg_nam", "OC.pct_nam", "pH.water_nam", "P.mgkg_nam",
                             "Sand.pct_nam", "Silt.pct_nam")
  
} else if (ds == "sfa_df") {
  # sfa
  # sfa_cnv_unts$Soil.ID |> table()
  sfa_cnv_unts$Soil.ID <- gsub("S$", "T", sfa_cnv_unts$Soil.ID) # adjust the Soil.ID to profile, instead of horizon, otherwise the 20-50 cm retunr -9999.0, as each sample is treated as a soil profiles in ithir::ea_spline()
  df <- sfa_cnv_unts
  limits_df <- limits_sfa
  depths_interp_columns <- c("BS.pct_sfa",
                             "Ca.mgkg_sfa", "CEC.cmolkg_sfa", "Clay.pct_sfa", "EC.uScm_sfa",
                             "K.mgkg_sfa", "Mg.mgkg_sfa", "Na.mgkg_sfa", "N.mgkg_sfa", "OC.pct_sfa",
                             "pH.water_sfa", "P.mgkg_sfa", "Sand.pct_sfa", "Silt.pct_sfa")
  
} else if (ds == "isda_df") {
  # isda
  df <- isda_cnv_unts
  limits_df <- limits_isda
  depths_interp_columns <- c("BD.kgdm3_isda", 
                             "Ca.mgkg_isda", "Clay.pct_isda", "K.mgkg_isda", "Mg.mgkg_isda", 
                             "N.mgkg_isda", "OC.pct_isda", "pH.water_isda", "P.mgkg_isda", 
                             "Sand.pct_isda", "Silt.pct_isda")
  
} else if (ds == "sgri_df") {
  # sgri
  df <- sgri_cnv_unts
  df <- df |> tidyr::drop_na() # 3 points fell outside the SoilGrids coverage during the extraction retunring as NA
  limits_df <- limits_sgri
  depths_interp_columns <- c("BD.kgdm3_sgri", 
                             "CEC.cmolkg_sgri", "Clay.pct_sgri", "N.mgkg_sgri", "OC.pct_sgri", 
                             "pH.water_sgri", "Sand.pct_sgri", "Silt.pct_sgri")
}

sfa_cnv_unts |> head()
isda_cnv_unts |> head()
################################################################################
# User definitions
# Indicate the depths interpolated
depths_ <- c(0,20,50)

################################################################################
### Gather the profiles ID to iterate over it
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
      # ul <- ul + (ul * 0.2) # add 20% to the upper limit to be more flexible in the interpolation
      
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
    ### coords <- c(x= mode.vect(dfp[["x"]]), y= mode.vect(dfp[["y"]]))
    ### ea_spl_result[ea_spl_result$Soil.ID == mode.vect(dfp[["Soil.ID"]]), c("x", "y")] <- coords 
  }
})[3] # 5 min
# Call the results
ea_spl_result

ea_spl_result <- ea_spl_result |> 
  dplyr::select(-x, -y)



################################################################################
### Rectify values lower than zero
# Count the values lower than zero
sum(sapply(ea_spl_result, function(x) sum(x == -9999.000, na.rm = TRUE))) # has zero 
# Replace -9999.000 with NA
ea_spl_result[ea_spl_result == -9999.000] <- NA

# Replace other spurious values with NA 
# Identifying columns that end with '_cm'
# cm_columns <- grep("_cm$", names(ea_spl_result))
 
# Count the values lower than zero
sum(sapply(ea_spl_result, function(x) sum(x < 0, na.rm = TRUE))) # has zero
# Replace negative values with zero in these columns
# ea_spl_result[cm_columns] <- lapply(ea_spl_result[cm_columns], function(x) replace(x, x < 0, 0))

# ea_spl_result
# table(is.na(ea_spl_result))

glimpse(ea_spl_result)

if(ds == "nam_df") {
  # Namibia national maps
  write.csv(ea_spl_result, "../output/2_nam_0_20_50_cm.csv", row.names = FALSE)

} else if (ds == "sfa_df") {
  # sfa
  write.csv(ea_spl_result, "../output/2_sfa_0_20_50_cm.csv", row.names = FALSE)

} else if (ds == "isda_df") {
  # isda
  write.csv(ea_spl_result, "../output/2_isda_0_20_50_cm.csv", row.names = FALSE)

} else if (ds == "sgri_df") {
  # sgri
  write.csv(ea_spl_result, "../output/2_sgrid_0_20_50_cm.csv", row.names = FALSE)

}



} # end for loop dataset

################################################################################
### Check interpolated outside limits
options(max.print = 2500) # increase the print limit  
ea_spl_result[which(ea_spl_result$water_sfa_0_20_cm < 4), ]


###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
### End script ----
###_____________________________________________________________________________

