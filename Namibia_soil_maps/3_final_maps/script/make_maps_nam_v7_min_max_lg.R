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

# Load the Namibia shapefile
Nam <- terra::vect("../../1_data_treatement/input/National_boundary.shp")

library(stringr)
library(dplyr)
library(foreach)
library(doParallel)
library(RColorBrewer)
# brewer.pal.info
# par(mar=c(3,4,2,2))
# display.brewer.all()
# display.brewer.all(colorblindFriendly = TRUE)
# brewer.pal(n=8, name="Set2")
# brewer.pal(n=8, name="Set3")
# brewer.pal(n=8, name="Pastel1")
# brewer.pal(n=8, name="Pastel2")
# 
# brewer.pal(n=9, name="PRGn")[c(1:4, 8:11)]
# brewer.pal(n=11, name="PRGn")[c(1:4, 8:11)]
# brewer.pal(n=8, name="Set2")
# "#66C2A5" "#FC8D62" "#8DA0CB" "#E78AC3" "#A6D854" "#FFD92F" "#E5C494" "#B3B3B3"
# brewer.pal(n=8, name="Set3")
# "#8DD3C7" "#FFFFB3" "#BEBADA" "#FB8072" "#80B1D3" "#FDB462" "#B3DE69" "#FCCDE5"
# brewer.pal(n=8, name="Pastel1")
# "#FBB4AE" "#B3CDE3" "#CCEBC5" "#DECBE4" "#FED9A6" "#FFFFCC" "#E5D8BD" "#FDDAEC"
# brewer.pal(n=8, name="Pastel2")
# "#B3E2CD" "#FDCDAC" "#CBD5E8" "#F4CAE4" "#E6F5C9" "#FFF2AE" "#F1E2CC" "#CCCCCC"

# Detect number of available cores
num_cores <- detectCores() - 8 # More cores take too much RAM and break the flow
registerDoParallel(cores = num_cores)

###_____________________________________________________________________________
### Check if the reference values file exists, if not, run the code to create it
###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨

file.exists("../output/dataset/ref_vals.csv") # if TRUE, skip the code below

if (!file.exists("../output/dataset/ref_vals.csv")) {

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
    "ECuScmEL25", "EC",    "Electrical conductivity of 2:5 supernatant",     "µS cm-1",      "YlGnBu", # dS m-1
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
  
  ###_____________________________________________________________________________
  ### Prepare the dataframe with limits for each property
  ###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
  
  # Output dataframe for global limits
  prop.limits <- data.frame(
    prop.grep = character(),
    prop.title = character(),
    unit = character(),
    depth = character(),
    raster_min = numeric(),
    raster_max = numeric(),
    range_min = numeric(),
    range_max = numeric(),
    stringsAsFactors = F
  )
  
  # List TIFF files
  rsts <- list.files("../input", pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
  
  # Process each file
  # for (i in 1:2) { # debugging
  for (i in seq_along(rsts)) {
    # Extract property and depth from the file name
    file_name <- basename(rsts[i])
    prop_match <- ref_df$prop.grep[sapply(ref_df$prop.grep, function(x) grepl(x, file_name))]
    depth <- str_extract(file_name, "\\d+_\\d+_cm")
    depth <- gsub("_", "-", depth)
    
    if (length(prop_match) == 0) next  # If do not find property, skip
    
    # Load raster and compute boxplot stats
    pred_ras <- terra::rast(rsts[i])
    nc <- terra::ncell(pred_ras)*0.99
    rst.whs <- terra::boxplot(pred_ras[[1:3]], plot = F, maxcell = nc)$stats
    rng.whs <- terra::boxplot(pred_ras[[4]], plot = F, maxcell = nc)$stats
    
    # Extract global min and max values from bands 1:3
    raster_min <- min(rst.whs)  # Lower bound (Q1)
    raster_max <- max(rst.whs)  # Upper bound (Q3)
    range_min <- min(rng.whs)  # Lower bound (Q1)
    range_max <- max(rng.whs)  # Upper bound (Q3)
    
    rm(rst.whs) # Clean memory
    rm(rng.whs) # Clean memory
    gc()
    
    # Add to the global limits data frame
    prop.limits <- rbind(prop.limits, data.frame(
      prop.grep = prop_match,
      prop.title = ref_df$prop.title[ref_df$prop.grep == prop_match],
      unit = ref_df$unit[ref_df$prop.grep == prop_match],
      depth = depth,
      raster_min = raster_min,
      raster_max = raster_max,
      range_min = range_min,
      range_max = range_max,
      stringsAsFactors = FALSE
    ))
    cat("Loop:", i, "\n")
    cat("Processed file:", file_name, "\n")
    print(prop.limits)
  }
  
  # Convert to tibble
  # prop.limits <- as_tibble(prop.limits)
  
  # Correct extreme negative value in Ca
  prop.limits[prop.limits$raster_min < -117, ]
  prop.limits[prop.limits$raster_min < -117, "raster_min"]
  prop.limits[prop.limits$raster_min < -117, "raster_min"] <- 16.34
  
  # Summarize by property
  summary.limits <- prop.limits %>%
    group_by(prop.grep, prop.title) %>%
    summarise(raster_min = min(raster_min, na.rm = T), 
              raster_max = max(raster_max, na.rm = T), 
              range_min = min(range_min, na.rm = T), 
              range_max = max(range_max, na.rm = T), 
              .groups = "drop")
  
  # Merge the reference dataframe with the summary limits
  ref_vals <- merge(ref_df, summary.limits, by = c("prop.grep", "prop.title"), all = T, sort = F)
  
  # Save results
  write.csv(prop.limits, "../output/dataset/prop.limits.csv", row.names = F)
  write.csv(summary.limits, "../output/dataset/summary.limits.csv", row.names = F)
  write.csv(ref_vals, "../output/dataset/ref_vals.csv", row.names = F)
  

} # Close the if statement about reference values file 

###_____________________________________________________________________________
### Set terra options, ram control
###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
# terra::terraOptions(memfrac = 0.1) # Set memory fraction to 10% for terra

###_____________________________________________________________________________
### Re-load the reference values
###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
ref_df <- read.csv("../output/dataset/ref_vals.csv")

###_____________________________________________________________________________
### Re-load the tif images
###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
rsts <- list.files("../input", pattern = "\\.tif$", recursive = T, full.names = T)
# rsts <- rsts[10] # CEC
# rsts <- rsts[2] # BD
# rsts <- rsts[46:48] # Silt 0-100
# basename(rsts)
# i <- 1

# results <- foreach(i = 1:length(rsts), .packages = c("terra", "RColorBrewer")) %dopar% { # gave error and do not complete all
for (i in 1:length(rsts)) {

  cat("Iteration i: ", i, "of 48 \n")
  cat("Loading raster: ", basename(rsts[i]), "\n")
  pred_ras <- terra::rast(rsts[i])
  # terra::plot(pred_ras)
  ### If sequential run use this to get lager data boxplot
  # Set maxcell for boxplot
  nc <- terra::ncell(pred_ras)*.99 # Set maxcell to 5% of the total number of cells, this use ~50 GB of RAM (more than this crash the Rstudio)
  ### If parallel use this to avoid memory
  # nc <- terra::ncell(pred_ras)*.05 # Set maxcell to 5% of the total number of cells, this use ~50 GB of RAM (more than this crash the Rstudio)
  # 248473960: total number of pixels of Namibia, with map pixel size 90m
  #  12423698: 5% of total
  #    100000: default maxcell of terra::boxplot()
  
  # system.time(terra::boxplot(pred_ras[[1:3]], plot=F)$stats)[3] # 16 sec
  # system.time(terra::boxplot(pred_ras[[1:3]], plot=F, maxcell=nc)$stats)[3] # 90 sec
  
  # Calculate boxplot stats for legend range, and drop the outliers
  # This canot be inside the other loop, because for lower bounds may result in single values
  ###### Moved to outside
  # rst.whs <- terra::boxplot(pred_ras[[1:3]], plot=F, maxcell=nc)$stats # interval between lower and upper bounds
  # rng.whs <- terra::boxplot(pred_ras[[4]], plot=F, maxcell=nc)$stats # interval between lower and upper bounds
  
  # Extract the raster name from the file name to select the property and range limits
  b1_name <- names(pred_ras[[1]])  # e.g., "Silt253umpct_0_30_cm_mean"
  partes <- unlist(str_split(b1_name, "_"))

  # Extract property details
  soil_prop <- ref_df$prop.grep[sapply(ref_df$prop.grep, function(p) grepl(p, b1_name, fixed = TRUE))][1]
  
  # Define the soil property and its limits
  rst.whs <- ref_df[ref_df$prop.grep == soil_prop, c("raster_min", "raster_max")]
  rng.whs <- ref_df[ref_df$prop.grep == soil_prop, c("range_min", "range_max")]

  # Get and set the legend length
  # leg_length_1 <- round(rst.whs[5,2] - rst.whs[1,1], 0)
  # leg_length_1 <- c(ceiling(rst.whs[5,2]) - floor(rst.whs[1,1]))
  leg_length_1 <- ceiling(max(rst.whs)) - floor(min(rst.whs))
  leg_length_2 <- leg_length_1
  if (leg_length_1 < 6) leg_length_2 <- 10 # Set minimum legend length
  if (leg_length_1 > 24) leg_length_2 <- 20 # Set maximum legend length
  
  # Define legend breaks (position on number in the of legend)
  # att <- round(seq(rst.whs[1,1], rst.whs[5,2], length.out= leg_length_2), 3) 
  # att.rng <- round(seq(rng.whs[1,1], rng.whs[5,1], length.out= leg_length_2), 3) 
  att <- round(seq(min(rst.whs), max(rst.whs), length.out= leg_length_2), 3) 
  att.rng <- round(seq(min(rng.whs), max(rng.whs), length.out= leg_length_2), 3) 
  
  # Inner boundaries
  ib <- terra::ext(pred_ras)
  
  ### Plot and save in loop
  for (k in 1:4) {
    
    cat("Iteration k: ", k, "of 4 \n")
    cat("Map: ", names(pred_ras)[k], "\n")
    
    # Get the raster name and extract properties
    ras_name <- names(pred_ras)[k]  # e.g., "Silt253umpct_0_30_cm_mean"
    partes <- unlist(str_split(ras_name, "_"))
    depth <- paste0(partes[2], "-", partes[3], " ", "cm")
    interval <- partes[5]
    
    # Extract property details
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
             "µS cm-1" = mu*S ~ cm^{-1},
             "cmolc kg-1" = cmol[c] ~ kg^{-1},
             "kg dm-3" = kg ~ dm^{-3},
             "mg kg-1" = mg ~ kg^{-1},
             unit)  # Return unit as-is if no superscript needed
    }
    
    # Define color palette based on soil property
    col_pall <- colorRampPalette(brewer.pal(n = 9, name = pcol))(100)
    # col_pall2 <- colorRampPalette(c("green4", "yellow", "magenta", "red4"))(leg_length_2 - 1)
    col_pall2 <- colorRampPalette(c("#A6D854", "#FFFFB3", "#80B1D3", "#E78AC3"))(leg_length_2 - 1)
    
    # Save the plot    
    png(filename = paste0("../output/plots/", names(pred_ras[[k]]), "_soil_map.png"),
        width = 20, height = 20, units = "cm",
        pointsize = 12, bg = "transparent",  res = 300)
    
    # Set the graphical parameters
    def.par <- par(no.readonly = TRUE) # save default, for resetting...
    # Set the layout for the plot
    mtx <- matrix(c(1,0,
                    2,0), 
                  nrow= 2, 
                  ncol= 2, 
                  byrow = TRUE)
    nf <- layout(mtx, widths= c(6,1), heights= c(6,1), TRUE)
    # layout.show(nf) # Check the layout
    
    # Set range inteval for legend
    range_ <- if (k != 4) {
      c(c(min(rst.whs), max(rst.whs)))
    } else {
      c(c(min(rng.whs), max(rng.whs)))}
    
    # Run the plot
    # type: "continuous", "classes", or "interval". 
    terra::plot(pred_ras[[k]], # type= "interval", 
                main = if (unit %in% c("µS cm-1", "cmolc kg-1", "kg dm-3", "mg kg-1")) {
                  as.expression(bquote(.(soil_property) * " (" * .(unit_to_expression(unit)) * ") - " * .(depth) * " " * .(interval)))
                } else {
                  paste0(soil_property, " (", unit, ") - ", depth, " ", interval)
                },
                range= range_, # Legend length
                plg=list(         # settings for legend
                  x= 24,       # position on the map
                  y= -21,        # position on the map

                  # Rules for title 
                  title = if (k != 4) {
                    if (unit %in% c("µS cm-1", "cmolc kg-1", "kg dm-3", "mg kg-1")) {
                      as.expression(bquote(.(s_prop_lg_tll) * " (" * .(unit_to_expression(unit)) * ")"))
                    } else {
                      paste0(s_prop_lg_tll, " (", unit, ")")
                    }
                  } else {
                    if (unit %in% c("µS cm-1", "cmolc kg-1", "kg dm-3", "mg kg-1")) {
                      # Usar scriptstyle para manter tamanho
                      as.expression(bquote(atop(textstyle("Uncertainty:"), 
                                                atop(textstyle("90% Prediction"), 
                                                     atop(textstyle("Interval Width"), 
                                                          textstyle(paste(.(s_prop_lg_tll), " (", .(unit_to_expression(unit)), ")")))))))
                    } else {
                      as.expression(bquote(atop(textstyle("Uncertainty:"), 
                                                atop(textstyle("90% Prediction"), 
                                                     atop(textstyle("Interval Width"), 
                                                          textstyle(paste(.(s_prop_lg_tll), " (", .(unit), ")")))))))
                    }
                  },
                  
                  title.x= 24.45,  # title position on the map
                  title.y= -20.92,   # title position on the map
                  title.cex= 0.8,  # legend title font size
                  shrink= 0.8,     # legend size
                  leg.shrink= 0.9, # legend size
                  # leg.width=1,  # legend width (not working)
                  tic= "out",     # legend tic
                  # at = att, # position on number in the of legend
                  at = if (k != 4) {att} else {att.rng}, # position on number in the of legend
                  cex=0.9),       # legend font size
                mar= c(3.1, 4.1, 3.1, 7.1), # margins: l, b, t, r
                box= F, axes= F, smooth= T,
                # ext = ib+10000, # expand box
                # xlim = c(ib[1], ib[2]), ylim = c(ib[3], ib[4]), # cut for inner box
                # col = col_pall(100), # change for switch in each map
                col = switch(k, 
                             col_pall, 
                             col_pall, 
                             col_pall, # soil_col_pall, 
                             col_pall2)
    )
    # Add the Namibia shapefile
    terra::plot(Nam, add= T, border= "grey30", lwd= 0.7) 
    
    # Add axis 
    axis(1, line= 1.8, tck= -0.01) # x-axis
    axis(2, line= 2, tck= -0.01) # y-axis
    
    # Add north arrow and scale bar
    # if (k == 4) { # turn on north arrow and scale bar
    if (k == 4+1) { # turn off north arrow and scale bar
      terra::north(type= 2, cex = 1, xy=c(ib[1]+11, ib[3]+2.0), xpd= T) # in windows use type= 1
      terra::sbar(300, lonlat = T, xy=c(ib[2]-4, ib[3]-0.3), labels = c("0", " ", "      300 km"),
                  adj=c(0.5, -1.2), type= "bar", divs= 3, xpd= T) # xy="bottomright" lab = '1 km'
    }
    
    par(mar = c(5,3.1,0,10.1)) # c(bottom, left, top, right)
    btt.whs <- terra::boxplot(pred_ras[[k]], horizontal=T, plot=T, maxcell=nc,
                              # width=0.01, 
                              # ylim=c(ib[1], ib[2]), # 'width' control the width of the boxes, 'ylim' adjusts the length
                              border="grey30", col="transparent", 
                              add=F, axes=F, frame.plot=F)$stats
    axis(1, at= round(btt.whs, 2), labels= round(btt.whs, 2), gap.axis= 0.1, las=1) # x-axis
    
    par(def.par)  # reset to default
    
    dev.off()
    # rm(btt.whs) # clean memory
    gc() # clean memory
  }
  # rm(pred_ras) # clean memory
  gc() # clean memory
}

# stopImplicitCluster()


################################################################################
################################################################################
################################################################################
