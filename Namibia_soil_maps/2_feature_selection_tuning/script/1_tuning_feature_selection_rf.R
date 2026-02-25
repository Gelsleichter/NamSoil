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

#¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
# Load packges ----
#_______________________________________________________________________________
# Select features with Boruta ----
# install.packages("Boruta")
library(Boruta)

# For RFE ----
library(caret)
# library(parallel)
library(doParallel)
# Calculate the number of cores
# no_cores <- detectCores() - 1
no_cores <- detectCores()
# create the cluster for caret to use
# cl <- makePSOCKcluster(no_cores)
cl <- parallel::makeCluster(no_cores)
doParallel::registerDoParallel(cores = cl) # break sometimes

# Define the control parameters
# ctrl <- rfeControl(functions = rfFuncs, #lmFuncs,
#   method = "LOOCV",
#   #saveDetails = T,
#   #returnResamp = "all",
#   #number = 3,
#   verbose = T,
#   allowParallel = T)
ctrl <- rfeControl(functions = rfFuncs, #lmFuncs,
                   rerank = T,
                   # method = "cv",
                   method = "repeatedcv",
                   repeats = 10, # used with repeatedcv only
                   saveDetails = T,
                   returnResamp = "all",
                   number = 10, # number of folds or number of resampling iterations
                   verbose = T,
                   # savePredictions = "all", # "all", "final", or "none"
                   # returnData = TRUE,
                   allowParallel = T)


# Tuning the model parameters ----
# https://arxiv.org/abs/1804.03515
# https://github.com/PhilippPro/tuneRanger
# install.packages("mlr")
# install.packages("tuneRanger")
library(mlr)
library(tuneRanger)
library(tidyr)
library(dplyr)

#¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
# Load the data ----
#_______________________________________________________________________________
df <- read.csv("../input/extracted_points_Nam_0_30_60_100_cm.csv")
names(df) |> dput()
df |> str()

# Convert soil properties to numeric, (null values coerced to character)
df <- df |> 
  dplyr::mutate(dplyr::across(dplyr::ends_with("_cm"), as.numeric))

#¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
# Names for responses and predictors ----
# Follow the col order as in gee script
#_______________________________________________________________________________
responses <- c(#"SoilID", 
               "Sand53umpct_0_30_cm", "Sand53umpct_30_60_cm", "Sand53umpct_60_100_cm", 
               "Silt253umpct_0_30_cm", "Silt253umpct_30_60_cm", "Silt253umpct_60_100_cm", 
               "Clay2umpct_0_30_cm", "Clay2umpct_30_60_cm", "Clay2umpct_60_100_cm", 
               "pHwater_0_30_cm", "pHwater_30_60_cm", "pHwater_60_100_cm",
               "ECuScmEL25_0_30_cm", "ECuScmEL25_30_60_cm", "ECuScmEL25_60_100_cm", 
               "ECuScmELCO_0_30_cm", "ECuScmELCO_30_60_cm", "ECuScmELCO_60_100_cm", 
               "BSpct_0_30_cm", "BSpct_30_60_cm", "BSpct_60_100_cm", 
               "OCpct_0_30_cm", "OCpct_30_60_cm", "OCpct_60_100_cm", 
               "BDkgdm3_0_30_cm", "BDkgdm3_30_60_cm", "BDkgdm3_60_100_cm", 
               "CECcmolkg_0_30_cm", "CECcmolkg_30_60_cm", "CECcmolkg_60_100_cm", 
               "Camgkg_0_30_cm", "Camgkg_30_60_cm", "Camgkg_60_100_cm", 
               "Kmgkg_0_30_cm", "Kmgkg_30_60_cm", "Kmgkg_60_100_cm", 
               "Mgmgkg_0_30_cm", "Mgmgkg_30_60_cm", "Mgmgkg_60_100_cm", 
               "Namgkg_0_30_cm", "Namgkg_30_60_cm", "Namgkg_60_100_cm", 
               "Nmgkg_0_30_cm", "Nmgkg_30_60_cm", "Nmgkg_60_100_cm", 
               "Pmgkg_0_30_cm", "Pmgkg_30_60_cm", "Pmgkg_60_100_cm"
               )

preds <- c("dem", "tpi", "chili", "landform", "topo_diver", "flow_dir", "flow_accumul", 
           "landcover", "hand", "blue_w", "green_w", "red_w", "nir_w", "swir1_w", "swir2_w", 
           "ndvi_w", "savi_w", "msavi_w", "evi_w", "kndvi_w", "blue_s", "green_s", "red_s", 
           "nir_s", "swir1_s", "swir2_s", "ndvi_s", "savi_s", "msavi_s", "evi_s", "kndvi_s", 
           "curv_max", "river_dist", "flow_lend_d", "convex", "geology", "pet", "arid_ind", 
           "landform_iwa", "namsoil_13", "aspp", "aez", "veg_types", "aez_n", "cc", "Slope", 
           "Aspect", "Northness", "Eastness", "HorizontalCurvature", "VerticalCurvature", 
           "kaolinite", "calcite", "quartz", "carbonate", "mafic", "prec_wc2", "tavg_wc2", 
           "geology_a", "flow_len_up", "carb_diff", "clay_diff", "ferr_diff", "iron", "rock_out"
           ) 

# df[preds]

# Loop through each response variable ----
for (i in responses) {
# for (i in responses[22]) { #  to debug (22 = OC)
  # Drop rows with NA in the response variable and predictors
  dfl <- df |> 
    dplyr::select(dplyr::all_of(c(i, preds))) |> 
    tidyr::drop_na()

  # Data preparation
  X = dfl[preds]
  Y = dfl[[i]]
  
  #¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
  # Feature selection with Boruta ----
  #_______________________________________________________________________________
  # Run Boruta
  boruta = Boruta(x = X, y = Y, 
                  doTrace = 3,      # verbosity level
                  # maxRuns:
                  # using a more complex model (example: free parameters), more runs tend to approve more features, but with fixed parameters, 300 or 700 provide similar output
                  maxRuns = 100,    # maximal number of importance source runs
                  ntree = 150#,      # (optional, the algorithm can set it)
                  # mtry = 71,        # res$recommended.pars$mtry, # (optional)
                  # min.node.size = 6 # res$recommended.pars$min.node.size # (optional)
  )
  
  # Results
  # boruta
  
  # Get the selected features
  # selected_features_bor <- getSelectedAttributes(boruta, withTentative = T)
  selected_features_bor <- getSelectedAttributes(boruta, withTentative = F)
  
  # Plot boruta results
  png(paste0("../output/plots/boruta_", i, ".png"), 
      width = 50, height = 15, units = "cm", 
      pointsize = 12, bg = "transparent", res = 300)
  par(mfrow = c(1, 1))
  par(mar = c(7, 4, 2, 2), xpd = TRUE) # bottom, left, top, right; xpd allows text outside plot region
  plot(boruta, las=3, xlab="")
  dev.off()
  
  #¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
  # Feature selection with Recursive Feature Elimination ----
  #_______________________________________________________________________________
  # set.seed(2384)
  rfProfile <- rfe(x = X,
                   y = Y,
                   # sizes = (2^(2:5)), # for first approach
                   # sizes = c(2:42),
                   sizes = c(4, 8, 16, 24, 32, 40, 52),
                   rfeControl = ctrl)

  # stopCluster(cl)
  
  selected_features_rfe <- rfProfile$optVariables # best preditores ++++++

  # Plot
  ggplot(rfProfile) +
    labs(title = paste0("Recursive Feature Elimination Profile - ", i),
         x = "Number of Predictors") + #, y = "RMSE") +
    scale_x_continuous(breaks = c(0, 20, 40, 60)) +
    theme_minimal()

  ggsave(paste0("../output/plots/rfe_", i, ".png"),
         width = 35,
         height = 12,
         units = "cm",
         dpi = 300,
         bg = "transparent")
  
  # # Plot (not working inside this loop)
  # png(paste0("../output/plots/rfe_", i, "_rbase.png"),
  #     width = 2200, height = 1200, res = 300)
  # plot(rfProfile, type = c("g", "o"))
  # dev.off()
  
  # # Variable importance RFE
  # imp <- varImp(rfProfile)
  # imp_df <- data.frame(
  #   Variable = rownames(imp),
  #   Importance = imp$Overall
  # ) |> 
  #   # arrange(desc(Importance)) |> 
  #   arrange(Importance) |> 
  #   mutate(Variable = factor(Variable, levels = Variable))
  # 
  # ggplot(imp_df, aes(x = Variable, y = Importance)) +
  #   geom_col(fill = "steelblue", alpha = 0.7) +
  #   coord_flip() +  # Invert axes for better visualization
  #   theme_minimal() +
  #   labs(x = "Covariates",
  #        y = "Importance", #  (%)
  #        title = paste0("Importance of covariates - RFE - ", i)) +
  #   theme(
  #     axis.text.y = element_text(size = 8),  # Adjust the size as needed
  #     panel.grid.major.y = element_blank()
  #   )
  # 
  # # n_top <- 20  # Adjust to the desired number of variables
  # # # imp_df_top <- imp_df %>% head(n_top)
  # # imp_df_top <- imp_df |> tail(n_top)
  # # 
  # # ggplot(imp_df_top, aes(x = Variable, y = Importance)) +
  # #   geom_col(fill = "steelblue", alpha = 0.7) +
  # #   coord_flip() +
  # #   theme_minimal() +
  # #   labs(x = "Variables",
  # #        y = "Importance", #  (%)
  # #        title = paste("The", n_top, "most important variables")) +
  # #   theme(
  # #     axis.text.y = element_text(size = 10),
  # #     panel.grid.major.y = element_blank()
  # #   )
  # ggsave(paste0("../output/plots/rfProfile_cov_importance_", i, ".png"),
  #        width = 15, 
  #        height = 25, 
  #        units = "cm", 
  #        dpi = 300,
  #        bg = "transparent")

  #¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
  # Tuning the model parameters - Boruta ----
  #_______________________________________________________________________________
  # A mlr task has to be created in order to use the package
  # We make an mlr task with the train_cv dataset here 
  # (Classification task with makeRegrTask, Regression Task with makeRegrTask)
  dfl.task.bor = makeRegrTask(data = dfl[c(i, selected_features_bor)], target = i)
  
  # Rough Estimation of the Tuning time
  # estimateTimeTuneRanger(dfl.task)
  
  # Tuning process
  res.bor = tuneRanger(dfl.task.bor, 
                       num.trees = 150,    # default 1000
                       num.threads = 12,   # to run in parallel
                       iters.warmup = 30,  # default 30
                       iters = 70)#,       # default 70
  # parameters = list(replace = F, splitrule = "extratrees"),                                   # (the possibility of fixing the parameters such as splitrule or replace, for example), other possibilities are: "variance", "extratrees", "maxstat", "beta" or "poisson"; with variance as default in 'ranger'. (extratrees and maxstat can offer good benefits)
  # tune.parameters = c("mtry", "min.node.size", "sample.fraction", "importance", "max.depth"), # default: mtry, min.node.size, sample.fraction
  # save.file.path = "../output/tuning/models/tuneRanger.RData")
  
  # Mean of best 5 % of the results
  # res.bor$results
  # res.bor$recommended.pars
  
  # Model with the new tuned hyperparameters
  # res.bor$model
  
  #¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
  # Tuning the model parameters - RFE ----
  #_______________________________________________________________________________
  dfl.task.rfe = makeRegrTask(data = dfl[c(i, selected_features_rfe)], target = i)
  # Tuning process
  res.rfe = tuneRanger(dfl.task.rfe, 
                       num.trees = 150,    # default 1000
                       num.threads = 12,   # to run in parallel
                       iters.warmup = 30,  # default 30
                       iters = 70)         # default 70

  #¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
  # Export for GEE - Boruta ----
  #_______________________________________________________________________________
  # Format as a JavaScript
  bands_output_bor <- paste0(
    "exports.bands_sel = [\n  ",
    paste0('"', selected_features_bor, '"', collapse = ",\n  "),
    "\n];"
  )
  # Get the recommended parameters from the tuning and RFE
  params.bor <- res.bor$recommended.pars
  # Format as a JavaScript
  rfParams_output_bor <- sprintf(
    "exports.rfParams = {\n  ntree: 150,\n  mtry: %d,\n  nodesize: %d,\n  sampsize: %.2f\n  //max_Nodes: 120,\n  //seed_: 0\n};",
    params.bor$mtry,
    params.bor$min.node.size,
    params.bor$sample.fraction
  )
  # Combining the two sections with a blank line between them
  out_bor <- paste(bands_output_bor, rfParams_output_bor, sep = "\n\n")
  # Write the output to a text file
  writeLines(out_bor, paste0("../output/properties_settings/bor_", i, ".txt"))
  

  #¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
  # Export for GEE - RFE ----
  #_______________________________________________________________________________
  # Format as a JavaScript
  bands_output_rfe <- paste0(
    "exports.bands_sel = [\n  ",
    paste0('"', selected_features_rfe, '"', collapse = ",\n  "),
    "\n];"
  )
  # Get the recommended parameters from the tuning and RFE
  params.rfe <- res.rfe$recommended.pars
  # Format as a JavaScript
  rfParams_output_rfe <- sprintf(
    "exports.rfParams = {\n  ntree: 150,\n  mtry: %d,\n  nodesize: %d,\n  sampsize: %.2f\n  //max_Nodes: 120,\n  //seed_: 0\n};",
    params.rfe$mtry,
    params.rfe$min.node.size,
    params.rfe$sample.fraction
  )
  # Combining the two sections with a blank line between them
  out_rfe <- paste(bands_output_rfe, rfParams_output_rfe, sep = "\n\n")
  # Write the output to a text file
  writeLines(out_rfe, paste0("../output/properties_settings/rfe_", i, ".txt"))

}

### aprox 5 hours with 12 cores


#¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
# In case of ordering by alphabetical order
#_______________________________________________________________________________
# c("BDkgdm3_0_30_cm", "BDkgdm3_30_60_cm", "BDkgdm3_60_100_cm", 
#   "BSpct_0_30_cm", "BSpct_30_60_cm", "BSpct_60_100_cm", 
#   "CECcmolkg_0_30_cm", "CECcmolkg_30_60_cm", "CECcmolkg_60_100_cm", 
#   "Camgkg_0_30_cm", "Camgkg_30_60_cm", "Camgkg_60_100_cm", 
#   "Clay2umpct_0_30_cm", "Clay2umpct_30_60_cm", "Clay2umpct_60_100_cm", 
#   "ECuScmEL25_0_30_cm", "ECuScmEL25_30_60_cm", "ECuScmEL25_60_100_cm", 
#   "ECuScmELCO_0_30_cm", "ECuScmELCO_30_60_cm", "ECuScmELCO_60_100_cm", 
#   "Kmgkg_0_30_cm", "Kmgkg_30_60_cm", "Kmgkg_60_100_cm", 
#   "Mgmgkg_0_30_cm", "Mgmgkg_30_60_cm", "Mgmgkg_60_100_cm", 
#   "Namgkg_0_30_cm", "Namgkg_30_60_cm", "Namgkg_60_100_cm", 
#   "Nmgkg_0_30_cm", "Nmgkg_30_60_cm", "Nmgkg_60_100_cm", 
#   "OCpct_0_30_cm", "OCpct_30_60_cm", "OCpct_60_100_cm", 
#   "Pmgkg_0_30_cm", "Pmgkg_30_60_cm", "Pmgkg_60_100_cm", 
#   "Sand53umpct_0_30_cm", "Sand53umpct_30_60_cm", "Sand53umpct_60_100_cm", 
#   "Silt253umpct_0_30_cm", "Silt253umpct_30_60_cm", "Silt253umpct_60_100_cm", 
#   "pHwater_0_30_cm", "pHwater_30_60_cm", "pHwater_60_100_cm")
# c("Aspect", "Eastness", "HorizontalCurvature", "Northness", 
#   "Slope", "SoilID", "VerticalCurvature", "WRB2015", "aez", "aez_n", "arid_ind", "aspp", 
#   "blue_s", "blue_w", "calcite", "carb_diff", "carbonate", "cc", 
#   "chili", "clay_diff", "convex", "curv_max", "dem", "evi_s", "evi_w", 
#   "ferr_diff", "flow_accumul", "flow_dir", "flow_len_up", "flow_lend_d", 
#   "geology", "geology_a", "green_s", "green_w", "hand", "iron", 
#   "kaolinite", "kndvi_s", "kndvi_w", "landcover", "landform", "landform_iwa", 
#   "mafic", "msavi_s", "msavi_w", "namsoil_13", "ndvi_s", "ndvi_w", 
#   "nir_s", "nir_w",  "pet", "prec_wc2", "quartz", "red_s", "red_w", "river_dist", 
#   "rock_out", "savi_s", "savi_w", "swir1_s", "swir1_w", "swir2_s", 
#   "swir2_w", "tavg_wc2", "topo_diver", "tpi", "veg_types"
# )
