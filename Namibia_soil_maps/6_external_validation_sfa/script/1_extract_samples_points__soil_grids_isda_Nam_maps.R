#############################################################################################
### External validation Namibia maps
### Yuri Andrei Gelsleichter, Marina Coetzee
### April 2025
#############################################################################################

### Set working directory automatically 
# Load library
library(this.path)
path_ <- this.dir()
setwd(path_)

# verify the working directory
getwd()

# clean memory and workspace
gc();rm(list=ls())

# Source: https://www.pedometria.org/blog/descarregando-e-manuseando-dados-do-soilgrids-com-r-e-gdal/

library(terra)
library(dplyr)

options(scipen = 0) # turn off scientific notation

###_____________________________________________________________________________
### Load Sampling points ----
###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
# see what is in the working directory
dir("../../")
sdb <- read.csv("../../5_modeling_spectra_data_sfa/output/dataset/merged_wc_mir.csv")
# sdb[1:5, 1:60]
gc()

###_____________________________________________________________________________
### Drop Extra_samples (as they have only coordinates) ----
###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
sdb <- sdb |> 
  dplyr::filter(data_type != "Extra_points")

###_____________________________________________________________________________
### Drop subsoil layer as they have same coordinates as top soil ----
###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
sdb <- sdb |> 
  dplyr::select(id, plot_code, layer_id, upper_depth, lower_depth, 
                sample_depth, gps_latitude, gps_longitude, data_type) |> 
  dplyr::filter(upper_depth == 0 & lower_depth == 20) 

nam_points <- terra::vect(sdb, geom = c("gps_longitude", "gps_latitude"), crs = "EPSG:4326")
nam_points

sdb_out_isric <- sdb
sdb_out_isda <- sdb
sdb_out_nam_maps <- sdb

###_____________________________________________________________________________
### Soil grids maps ----
###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
# Product link (for SOC - isric)
# sg_url="/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/"
sg_url="https://files.isric.org/soilgrids/latest/data/"
# layers:
{
  soc_isric_0_5cm <- paste0(sg_url,'soc/soc_0-5cm_mean.vrt')
  soc_isric_5_15cm <- paste0(sg_url,'soc/soc_5-15cm_mean.vrt')
  soc_isric_15_30cm <- paste0(sg_url,'soc/soc_15-30cm_mean.vrt')
  soc_isric_30_60cm <- paste0(sg_url,'soc/soc_30-60cm_mean.vrt')
  soc_isric_60_100cm <- paste0(sg_url,'soc/soc_60-100cm_mean.vrt')
  soc_isric_100_200cm <- paste0(sg_url,'soc/soc_100-200cm_mean.vrt')
  sand_0_5cm <- paste0(sg_url,'sand/sand_0-5cm_mean.vrt')
  sand_5_15cm <- paste0(sg_url,'sand/sand_5-15cm_mean.vrt')
  sand_15_30cm <- paste0(sg_url,'sand/sand_15-30cm_mean.vrt')
  sand_30_60cm <- paste0(sg_url,'sand/sand_30-60cm_mean.vrt')
  sand_60_100cm <- paste0(sg_url,'sand/sand_60-100cm_mean.vrt')
  sand_100_200cm <- paste0(sg_url,'sand/sand_100-200cm_mean.vrt')
  silt_0_5cm <- paste0(sg_url,'silt/silt_0-5cm_mean.vrt')
  silt_5_15cm <- paste0(sg_url,'silt/silt_5-15cm_mean.vrt')
  silt_15_30cm <- paste0(sg_url,'silt/silt_15-30cm_mean.vrt')
  silt_30_60cm <- paste0(sg_url,'silt/silt_30-60cm_mean.vrt')
  silt_60_100cm <- paste0(sg_url,'silt/silt_60-100cm_mean.vrt')
  silt_100_200cm <- paste0(sg_url,'silt/silt_100-200cm_mean.vrt')
  clay_0_5cm <- paste0(sg_url,'clay/clay_0-5cm_mean.vrt')
  clay_5_15cm <- paste0(sg_url,'clay/clay_5-15cm_mean.vrt')
  clay_15_30cm <- paste0(sg_url,'clay/clay_15-30cm_mean.vrt')
  clay_30_60cm <- paste0(sg_url,'clay/clay_30-60cm_mean.vrt')
  clay_60_100cm <- paste0(sg_url,'clay/clay_60-100cm_mean.vrt')
  clay_100_200cm <- paste0(sg_url,'clay/clay_100-200cm_mean.vrt')
  phh2o_0_5cm <- paste0(sg_url,'phh2o/phh2o_0-5cm_mean.vrt')
  phh2o_5_15cm <- paste0(sg_url,'phh2o/phh2o_5-15cm_mean.vrt')
  phh2o_15_30cm <- paste0(sg_url,'phh2o/phh2o_15-30cm_mean.vrt')
  phh2o_30_60cm <- paste0(sg_url,'phh2o/phh2o_30-60cm_mean.vrt')
  phh2o_60_100cm <- paste0(sg_url,'phh2o/phh2o_60-100cm_mean.vrt')
  phh2o_100_200cm <- paste0(sg_url,'phh2o/phh2o_100-200cm_mean.vrt')
  nitrogen_0_5cm <- paste0(sg_url,'nitrogen/nitrogen_0-5cm_mean.vrt')
  nitrogen_5_15cm <- paste0(sg_url,'nitrogen/nitrogen_5-15cm_mean.vrt')
  nitrogen_15_30cm <- paste0(sg_url,'nitrogen/nitrogen_15-30cm_mean.vrt')
  nitrogen_30_60cm <- paste0(sg_url,'nitrogen/nitrogen_30-60cm_mean.vrt')
  nitrogen_60_100cm <- paste0(sg_url,'nitrogen/nitrogen_60-100cm_mean.vrt')
  nitrogen_100_200cm <- paste0(sg_url,'nitrogen/nitrogen_100-200cm_mean.vrt')
  cec_0_5cm <- paste0(sg_url,'cec/cec_0-5cm_mean.vrt')
  cec_5_15cm <- paste0(sg_url,'cec/cec_5-15cm_mean.vrt')
  cec_15_30cm <- paste0(sg_url,'cec/cec_15-30cm_mean.vrt')
  cec_30_60cm <- paste0(sg_url,'cec/cec_30-60cm_mean.vrt')
  cec_60_100cm <- paste0(sg_url,'cec/cec_60-100cm_mean.vrt')
  cec_100_200cm <- paste0(sg_url,'cec/cec_100-200cm_mean.vrt')
  bdod_0_5cm <- paste0(sg_url,'bdod/bdod_0-5cm_mean.vrt')
  bdod_5_15cm <- paste0(sg_url,'bdod/bdod_5-15cm_mean.vrt')
  bdod_15_30cm <- paste0(sg_url,'bdod/bdod_15-30cm_mean.vrt')
  bdod_30_60cm <- paste0(sg_url,'bdod/bdod_30-60cm_mean.vrt')
  bdod_60_100cm <- paste0(sg_url,'bdod/bdod_60-100cm_mean.vrt')
  bdod_100_200cm <- paste0(sg_url,'bdod/bdod_100-200cm_mean.vrt')
}
isric_layers <- list(soc_isric_0_5cm,
                     soc_isric_5_15cm,
                     soc_isric_15_30cm,
                     soc_isric_30_60cm,
                     soc_isric_60_100cm,
                     soc_isric_100_200cm,
                     sand_0_5cm,
                     sand_5_15cm,
                     sand_15_30cm,
                     sand_30_60cm,
                     sand_60_100cm,
                     sand_100_200cm,
                     silt_0_5cm,
                     silt_5_15cm,
                     silt_15_30cm,
                     silt_30_60cm,
                     silt_60_100cm,
                     silt_100_200cm,
                     clay_0_5cm,
                     clay_5_15cm,
                     clay_15_30cm,
                     clay_30_60cm,
                     clay_60_100cm,
                     clay_100_200cm,
                     phh2o_0_5cm,
                     phh2o_5_15cm,
                     phh2o_15_30cm,
                     phh2o_30_60cm,
                     phh2o_60_100cm,
                     phh2o_100_200cm,
                     nitrogen_0_5cm,
                     nitrogen_5_15cm,
                     nitrogen_15_30cm,
                     nitrogen_30_60cm,
                     nitrogen_60_100cm,
                     nitrogen_100_200cm,
                     cec_0_5cm,
                     cec_5_15cm,
                     cec_15_30cm,
                     cec_30_60cm,
                     cec_60_100cm,
                     cec_100_200cm,
                     bdod_0_5cm,
                     bdod_5_15cm,
                     bdod_15_30cm,
                     bdod_30_60cm,
                     bdod_60_100cm,
                     bdod_100_200cm)

for(i in isric_layers) {
  cat(paste0("\n ", "Layer ", match(i, isric_layers), " of ", length(isric_layers), "\n ", basename(i), "\n ", i, "\n"))
  
  curr_layer <- terra::rast(i, vsi= T)
  print(curr_layer)
  nm <- terra::names(curr_layer)
  
  nam_points_sg <- terra::project(nam_points, crs(curr_layer)) # reproject to Interrupted_Goode_Homolosine
  nam_sg_pts <- terra::extract(curr_layer, nam_points_sg, bind = TRUE)
  nam_sg_pts_wg84 <- terra::project(nam_sg_pts, "EPSG:4326") # reproject to WGS84
  nam_sg_pts_df <- terra::as.data.frame(nam_sg_pts_wg84)
  ### nam_sg_pts_df <- nam_sg_pts_df[complete.cases(nam_sg_pts_df), ]
  
  sdb_out_isric <- dplyr::inner_join(sdb_out_isric, nam_sg_pts_df, 
                               by = c("id", "plot_code", "layer_id", 
                                      "upper_depth", "lower_depth", "sample_depth", "data_type"))
  
  write.csv(sdb_out_isric, file = paste0("../output/sfa_points_soilgrids.csv"), row.names = FALSE)
}


###_____________________________________________________________________________
### ISDA maps ----
###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨

# Isda: Product link Source:
# https://zenodo.org/search?page=1&size=20&q=isdasoil
# https://www.nature.com/articles/s41598-021-85639-y
# https://gitlab.com/openlandmap/africa-soil-and-agronomy-data-cube
{
# SOC https://zenodo.org/records/4090927
soc_0_20 <- "https://zenodo.org/records/4090927/files/sol_log.oc_m_30m_0..20cm_2001..2017_v0.13_wgs84.tif"
soc_20_50 <- "https://zenodo.org/records/4090927/files/sol_log.oc_m_30m_20..50cm_2001..2017_v0.13_wgs84.tif"
# g/kg = expm1( y / 10 )

# Sand https://zenodo.org/records/4094607
sand_0_20 <- "https://zenodo.org/records/4094607/files/sol_sand_tot_psa_m_30m_0..20cm_2001..2017_v0.13_wgs84.tif"
sand_20_50 <- "https://zenodo.org/records/4094607/files/sol_sand_tot_psa_m_30m_20..50cm_2001..2017_v0.13_wgs84.tif"
# already in %

# Silt https://zenodo.org/records/4094610
silt_0_20 <- "https://zenodo.org/records/4094610/files/sol_silt_tot_psa_m_30m_0..20cm_2001..2017_v0.13_wgs84.tif"
silt_20_50 <- "https://zenodo.org/records/4094610/files/sol_silt_tot_psa_m_30m_20..50cm_2001..2017_v0.13_wgs84.tif"
# already in %

# Clay https://zenodo.org/records/4085160
clay_0_20 <- "https://zenodo.org/records/4085160/files/sol_clay_tot_psa_m_30m_0..20cm_2001..2017_v0.13_wgs84.tif"
clay_20_50 <- "https://zenodo.org/records/4085160/files/sol_clay_tot_psa_m_30m_20..50cm_2001..2017_v0.13_wgs84.tif"
# already in %

# pH https://zenodo.org/records/4220290
pH_0_20 <- "https://zenodo.org/records/4220290/files/sol_ph_h2o_m_30m_0..20cm_2001..2017_v0.13_wgs84.tif"
pH_20_50 <- "https://zenodo.org/records/4220290/files/sol_ph_h2o_m_30m_20..50cm_2001..2017_v0.13_wgs84.tif"
# index = y / 10

# N https://zenodo.org/records/4090386
N_0_20 <- "https://zenodo.org/records/4090386/files/sol_log.n_tot_ncs_m_30m_0..20cm_2001..2017_v0.13_wgs84.tif"
N_20_50 <- "https://zenodo.org/records/4090386/files/sol_log.n_tot_ncs_m_30m_20..50cm_2001..2017_v0.13_wgs84.tif"
# g/kg = expm1( y / 100 )

# Bd https://zenodo.org/records/4087905
bd_0_20 <- "https://zenodo.org/records/4087905/files/sol_db_od_m_30m_0..20cm_2001..2017_v0.13_wgs84.tif"
bd_20_50 <- "https://zenodo.org/records/4087905/files/sol_db_od_m_30m_20..50cm_2001..2017_v0.13_wgs84.tif"
# kg/m3 = y * 10

# K https://zenodo.org/records/4090298
k_0_20 <- "https://zenodo.org/records/4090298/files/sol_log.k_mehlich3_m_30m_0..20cm_2001..2017_v0.13_wgs84.tif"
k_20_50 <- "https://zenodo.org/records/4090298/files/sol_log.k_mehlich3_m_30m_20..50cm_2001..2017_v0.13_wgs84.tif"
# ppm = expm1( y / 10 )

# Mg https://zenodo.org/records/4090374
mg_0_20 <- "https://zenodo.org/records/4090374/files/sol_log.mg_mehlich3_m_30m_0..20cm_2001..2017_v0.13_wgs84.tif"
mg_20_50 <- "https://zenodo.org/records/4090374/files/sol_log.mg_mehlich3_m_30m_20..50cm_2001..2017_v0.13_wgs84.tif"
# ppm = expm1( y / 10 )

# Ca https://zenodo.org/records/4087972
ca_0_20 <- "https://zenodo.org/records/4087972/files/sol_log.ca_mehlich3_m_30m_0..20cm_2001..2017_v0.13_wgs84.tif"
ca_20_50 <- "https://zenodo.org/records/4087972/files/sol_log.ca_mehlich3_m_30m_20..50cm_2001..2017_v0.13_wgs84.tif"
# ppm = expm1( y / 10 )

# P https://zenodo.org/records/4090981
p_0_20 <- "https://zenodo.org/records/4090981/files/sol_log.p_mehlich3_m_30m_0..20cm_2001..2017_v0.13_wgs84.tif"
p_20_50 <- "https://zenodo.org/records/4090981/files/sol_log.p_mehlich3_m_30m_20..50cm_2001..2017_v0.13_wgs84.tif"
# ppm = expm1( y / 10 )
}

isda_layers <- list(soc_0_20,
                    soc_20_50,
                    sand_0_20,
                    sand_20_50,
                    silt_0_20,
                    silt_20_50,
                    clay_0_20,
                    clay_20_50,
                    pH_0_20,
                    pH_20_50,
                    N_0_20,
                    N_20_50,
                    bd_0_20,
                    bd_20_50,
                    k_0_20,
                    k_20_50,
                    mg_0_20,
                    mg_20_50,
                    ca_0_20,
                    ca_20_50,
                    p_0_20,
                    p_20_50)

for(i in isda_layers) {
  cat(paste0("\n ", "Layer ", match(i, isda_layers), " of ", length(isda_layers), "\n ", basename(i), "\n ", i, "\n"))
  
  curr_layer <- terra::rast(i, vsi= T)
  print(curr_layer)
  
  nam_sg_pts <- terra::extract(curr_layer, nam_points, bind = TRUE)
  nam_sg_pts_df <- terra::as.data.frame(nam_sg_pts)

  sdb_out_isda <- dplyr::inner_join(sdb_out_isda, nam_sg_pts_df, 
                                     by = c("id", "plot_code", "layer_id", 
                                            "upper_depth", "lower_depth", "sample_depth", "data_type"))
  
  write.csv(sdb_out_isda, file = paste0("../output/sfa_points_isda.csv"), row.names = FALSE)
}


###_____________________________________________________________________________
### Namibia maps ----
###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨

# List TIFF files, predicted (Maps)
rsts <- list.files("../../3_final_maps/input/", pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
rsts

for(i in rsts) {
  cat(paste0("\n ", "Layer ", match(i, rsts), " of ", length(rsts), "\n ", basename(i), "\n "))
  
  curr_layer <- terra::rast(i)[[1]]  # Load only the mean prediction layer
  print(curr_layer)
  
  nam_sg_pts <- terra::extract(curr_layer, nam_points, bind = TRUE)
  nam_sg_pts_df <- terra::as.data.frame(nam_sg_pts)
  
  sdb_out_nam_maps <- dplyr::inner_join(sdb_out_nam_maps, nam_sg_pts_df, 
                                    by = c("id", "plot_code", "layer_id", 
                                           "upper_depth", "lower_depth", "sample_depth", "data_type"))
  
  write.csv(sdb_out_nam_maps, file = paste0("../output/sfa_points_nam_maps.csv"), row.names = FALSE)
}

#¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
# End script ----
#_______________________________________________________________________________
