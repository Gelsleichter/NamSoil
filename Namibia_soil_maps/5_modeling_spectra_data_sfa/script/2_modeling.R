###_____________________________________________________________________________
### Magyar Agrar- es Elettudomanyi Egyetem - MATE
### Modeling spectral data to predict soil properties, SFA Namibia
### Nov 2025 
### Yuri Andrei Gelsleichter, Marina Coetzee
###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨

###_____________________________________________________________________________
# +--- slant ::: https://www.askapache.com/online-tools/figlet-ascii/
#     __  ___________   
#    /  |/  /  _/ __ \  
#   / /|_/ // // /_/ /  
#  / /  / // // _, _/   
# /_/  /_/___/_/ |_|    
###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨

### Set working directory automatically ----
library(this.path)
path_ <- this.dir()
setwd(path_)
getwd()
# clean memory
# gc(); rm(list=ls())

{
  library(dplyr) # for data handling
  # library(randomForest) # for random forest modeling
  library(ranger) # for fast random forest modeling
  library(ggplot2) # for plotting
  library(caret) # for createFolds()
  library(Cubist)
  library(pls) # for PLSR modeling
  library(tune) # for coord_obs_pred()
  library(ggplot2) # for ggplot()
  library(ggExtra) # for ggMarginal()
  library(parallel) # for detectCores()
  library(doParallel)
}

n_cores <- detectCores() # - 1 # leave 1 core for OS
registerDoParallel(cores = n_cores) # Use mclapply internally in Linux

### Loading MIR ----
sdb <- read.csv("../output/dataset/merged_wc_mir.csv")
# sdb[1:5, 1:60]
gc()

###_____________________________________________________________________________
### Debug mode (reduce 90% of data)
###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
# TEST_MODE <- T # short data to faster test
TEST_MODE <- F # full data

if (TEST_MODE) {
  cat("DEBUG MODE on: reduce 90% of data \n")
  
  mir_cols  <- grep("^mir_", names(sdb), value = TRUE)
  
  mir_keep  <- mir_cols[seq(1, length(mir_cols), by = 10)]
  
  sdb <- sdb %>%
    select(
      !all_of(setdiff(c(mir_cols), c(mir_keep)))
    )
  
  cat("Reduced for", ncol(sdb), "columuns (", 
      round(100 * ncol(sdb) / 3931, 1), "% of original)\n")
}
# ===========================================================================

###_____________________________________________________________________________
### Drop Extra_samples (as they have only coordinates) ----
###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
sdb <- sdb |> 
  dplyr::filter(data_type != "Extra_points")

###_____________________________________________________________________________
### Define target variables ----
###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
targets <- c("Fe_ox", "Al_ox", "Al", "B", "Ca", "Cu", "Fe", "K", "Mg", "Mn", 
             "Na", "P", "Zn", "coSa", "meSa", "fiSa", "vfiSa", "Sa", "coSi", 
             "fiSi", "Si", "Clay", "pH.H2O", "pH.KCL", "EC", "CaCO3", "totC", 
             "orgC", "totN", "exchCa", "exchMg", "exchK", "CEC", "exchH", 
             "BaseSat", "exchAcid", "totCd", "totPb", "totV", "totCr", "totCo", 
             "totNi", "totCu", "totZn", "totAs", "totSb")
# targets <- c("orgC", "Clay", "pH.H2O", "Ca") # debug
# targets <- c("Ca", "K", "Mg", "Na", "P", "Sa", "Si", "Clay", "pH.H2O", "EC", 
             # "orgC", "totN", "CEC", "BaseSat") # properties used in Nam Mapping
targets <- c("orgC", "totN", "CEC", "BaseSat") # recap after Rstudio crash

units <- c(
  Fe_ox = "mg/g",
  Al_ox = "mg/g",
  Al = "mg/kg",
  B = "mg/kg",
  Ca = "cmol(+)/kg",
  Cu = "mg/kg",
  Fe = "mg/kg",
  K = "mg/kg",
  Mg = "cmol(+)/kg",
  Mn = "mg/kg",
  Na = "cmol(+)/kg",
  P = "mg/kg",
  Zn = "mg/kg",
  coSa = "%",
  meSa = "%",
  fiSa = "%",
  vfiSa = "%",
  Sa = "%",
  coSi = "%",
  fiSi = "%",
  Si = "%",
  Clay = "%",
  pH.H2O = "-",
  pH.KCL = "-",
  EC = "mS/m",
  CaCO3 = "g/kg",
  totC = "g/kg",
  orgC = "g/kg",
  totN = "g/kg",
  exchCa = "cmol(+)/kg",
  exchMg = "cmol(+)/kg",
  exchK = "cmol(+)/kg",
  CEC = "cmol(+)/kg",
  exchH = "cmol(+)/kg",
  BaseSat = "%",
  exchAcid = "cmol(+)/kg",
  totCd = "mg/kg",
  totPb = "mg/kg",
  totV = "mg/kg",
  totCr = "mg/kg",
  totCo = "mg/kg",
  totNi = "mg/kg",
  totCu = "mg/kg",
  totZn = "mg/kg",
  totAs = "mg/kg",
  totSb = "mg/kg"
)

#_______________________________________________________________________________
# Configuration for the model ----
#¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
# Number of folds and repetitions
k <- 10 # 15 # https://doi.org/10.1016/j.geodrs.2024.e00901
repeats <- 3
total_iterations <- k * repeats # Total number of models (1000)
# Counter for resample naming
resample_counter <- 1 # start at 1
model_counter <- 1 # start at 1

# Load functions for running and save plots
source("./0_plot_fun_source.R") 

###_____________________________________________________________________________
### Loop across segmentation variables including all data and spectral types VNIR, MIR VNIR+MIR ----
###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨

for (target in targets) {
  ### Units ----
  unity <- units[target]
  # unity <- units[[target]]
  # print(paste(target, unity, "-", spec))
  
  ### Split data Train_CV - Test ----
  train_cv_df <- sdb |>
    dplyr::filter(data_type == "Train_CV") #|> tidyr::drop_na()
  
  pred_df <- sdb |>
    dplyr::filter(data_type == "Pred") #|> tidyr::drop_na()
  
  ### Split data target, predictors ----
  target_train <- train_cv_df[[target]]
  Y_tr <- target_train
  predictors_train <- train_cv_df |> dplyr::select(dplyr::starts_with("mir_"))
  
  predictors_pred <- pred_df |> dplyr::select(dplyr::starts_with("mir_"))
  
  ### Pre treatments ----
  # preproc_methods <- c("raw_spectra", "mov_avg", "sg", "snv", "differ1", "differ2")
  preproc_methods <- c("raw_spectra", "mov_avg", "sg", "differ1")
  
  for (pp in preproc_methods) {
    X_tr <- predictors_train
    X_pr <- predictors_pred
    
    if (pp == "raw_spectra") {
      X_tr <- X_tr
      X_pr <- X_pr
    }
    if (pp == "mov_avg") {
      X_tr <- as.data.frame(prospectr::movav(X_tr, 11))
      X_pr <- as.data.frame(prospectr::movav(X_pr, 11))
    }
    if (pp == "sg") {
      X_tr <- as.data.frame(prospectr::savitzkyGolay(X_tr, p=2, w=17, m=0))
      X_pr <- as.data.frame(prospectr::savitzkyGolay(X_pr, p=2, w=17, m=0))
    }
    # if (pp == "snv") {
    #   X_tr <- as.data.frame(prospectr::standardNormalVariate(X_tr))
    #   X_pr <- as.data.frame(prospectr::standardNormalVariate(X_pr))
    # }
    if (pp == "differ1") {
      X_tr <- as.data.frame(prospectr::savitzkyGolay(X_tr, p=2, w=51, m=1))
      X_pr <- as.data.frame(prospectr::savitzkyGolay(X_pr, p=2, w=51, m=1))
    }
    # if (pp == "differ2") {
    #   X_tr <- as.data.frame(prospectr::savitzkyGolay(X_tr, p=2, w=51, m=2))
    #   X_pr <- as.data.frame(prospectr::savitzkyGolay(X_pr, p=2, w=51, m=2))
    # }
    
    ### Info to console ----
    # cat("Soil property:", target,
    #     unity,
    #     "|", spec,
    #     "| Seg_col:", seg_var,
    #     "| Seg_var:", lvl,
    #     "| Pre-proc:", pp,
    #     "| Train:", nrow(train_cv_df),
    #     "| Test:", nrow(test), "\n")
    
    ### Split 10-folds CV ----
    # Loop for repetitions
    
    n <- nrow(X_tr) # Number of observations
    
    for (r in 1:repeats) {
      # Create indices for k-fold CV
      folds <- caret::createFolds(target_train, k = k, list = TRUE, returnTrain = FALSE)
      
      # Loop for each fold
      for (fold_idx in 1:k) {
        
        # Resample name (e.g., Resample0001, Resample0002, ...)
        resample_name <- sprintf("Resample%04d", resample_counter)
        # i <- resample_counter # call it 'i' for simplicity
        
        # Test and train indices
        valid_idx <- folds[[fold_idx]]
        train_idx <- setdiff(1:n, valid_idx)
        
        # Split data into training and validation sets
        train_predictors_int <- X_tr[train_idx, ]
        valid_predictors_int <- X_tr[valid_idx, ]
        train_response_int <- Y_tr[train_idx]
        valid_response_int <- Y_tr[valid_idx]
        
        ###_____________________________________________________________________________
        ### Models ----
        ###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
        # batch_results <- future_map_dfr(c("rf", "plsr", "cubist"), function(model) {
        # cat("Runing:", pp, "|", model, "\n")
        # fit <- NULL
        for (model in c("rf", "plsr", "cubist")) {
          cat("Runing:", pp, "|", model, "\n")
          
          # Model name (e.g., Model0001, Model0002, ...)
          model_itera <- sprintf("Model%04d", model_counter)

          # Train the model based on the selected algorithm
          if (model == "rf") {
            # rg_model <- ranger::ranger(x = train_predictors_int, y = train_response_int, num.trees = 250)
            fit <- ranger::ranger(x = train_predictors_int, y = train_response_int, num.trees = 250, num.threads = n_cores)
            # fit <- randomForest::randomForest(x = train_predictors_int, y = train_response_int, ntree = 250)
          }
          if (model == "plsr") {
            tmp <- pls::plsr(train_response_int ~ ., data = train_predictors_int, validation = "CV")
            nc  <- which.min(pls::RMSEP(tmp)$val[1,,]) - 1
            if (nc < 1) {nc <- 1} # at least 1 component (to avoid errors)
            fit <- pls::plsr(train_response_int ~ ., data = train_predictors_int, ncomp = nc)
            rm(tmp)
            gc()
          }
          if (model == "cubist") {
            # fit <- Cubist::cubist(x = train_predictors_int, y = train_response_int, committees = 100, neighbors = 9)
            fit <- Cubist::cubist(x = train_predictors_int, y = train_response_int) # faster to debug
          }
          
          # Predictions for calibration (training set)
          if (model == "plsr") {
            pred_cal <- drop(predict(fit, train_predictors_int, ncomp = nc))
            pred_val <- drop(predict(fit, valid_predictors_int, ncomp = nc))
            predicted_values <- drop(predict(fit, X_pr, ncomp = nc))
          } else if (model == "rf") {
            pred_cal <- predict(fit, train_predictors_int)$predictions
            pred_val <- predict(fit, valid_predictors_int)$predictions
            predicted_values <- predict(fit, X_pr)$predictions
          } else if (model == "cubist") {
            pred_cal <- predict(fit, train_predictors_int)
            pred_val <- predict(fit, valid_predictors_int)
            predicted_values <- predict(fit, X_pr)
          }
          
          # Calculate metrics for calibration (training set)
          rsq_cal <- round((cor(pred_cal, train_response_int))^2, 3)            # coefficient of determination (using correlation) - R²
          mse_cal <- round(mean((train_response_int - pred_cal)^2), 3)          # Mean Squared Error -	MSE
          rmse_cal <- round(sqrt(mean((train_response_int - pred_cal)^2)), 3)   # Root Mean Squared Error - RMSE
          mae_cal <- round(mean(abs(train_response_int - pred_cal)), 3)         # Mean Absolute Error - MAE
          # ae_cal <- round(abs(train_response_int - pred_cal), 3)              # Absolute Error - AE
          # rpd_cal <- round(sd(train_response_int) / rmse_val, 3)              # Ratio of performance to deviation - RPD
          
          # Calculate metrics for validation set
          rsq_val <- round((cor(pred_val, valid_response_int))^2, 3)
          mse_val <- round(mean((valid_response_int - pred_val)^2), 3)
          rmse_val <- round(sqrt(mean((valid_response_int - pred_val)^2)), 3)
          mae_val <- round(mean(abs(valid_response_int - pred_val)), 3)
          # ae_val <- round(abs(valid_response_int - pred_val), 3)
          # rpd_val <- round(sd(valid_response_int) / rmse_val, 3)
          
          ### Info to console ----
          cat("Soil property:", target,
              unity,
              "| Pre-proc:", pp,
              "| Model", model, 
              "| R2_internal_valid:", rsq_val, "\n")
          
          ### Create a folder based on the parameters ----
          dir_path <- file.path("../output/modeling_output",
                                target,
                                ### unity,
                                pp,
                                model)
          dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
          
          ### Save results ----
          result_df <- tibble(
            target = target,
            # unit = unity,
            preproc = pp,
            model = model,
            resample = resample_name,
            model_iteration = model_itera,
            fold_index = fold_idx,
            repeatition = r,
            ncomp_plsr = if(model == "plsr") nc else NA_integer_,
            R2_cal = rsq_cal,
            MSE_cal = mse_cal,
            RMSE_cal = rmse_cal,
            MAE_cal = mae_cal,
            R2_val = rsq_val,
            MSE_val = mse_val,
            RMSE_val = rmse_val,
            MAE_val = mae_val,
            timestamp = Sys.time()
          )
          
          # Export the metrics
          filename <- file.path(dir_path, paste(target, pp, model, resample_name, "fold_index", fold_idx, "repeat", r, sep = "_", "results_metrics.csv"))
          readr::write_csv(result_df, filename)
          
          # Export predicted values
          filename_pred <- file.path(dir_path, paste(target, pp, model, resample_name, "fold_index", fold_idx, "repeat", r, sep = "_", "predicted_values.csv"))
          pred_values_df <- data.frame(round(predicted_values, 3))
          names(pred_values_df) <- target
          readr::write_csv(pred_values_df, filename_pred)
          
          ### Save model (if usefull) ----
          if (!is.na(rsq_val) && rsq_val > 0.981 && rsq_val < 0.989) {
            model_filename <- file.path(dir_path, paste(target, pp, model, resample_name, "fold_index", fold_idx, "repeat", r, sep = "_", "model.rds"))
            saveRDS(fit, file = model_filename)
          } 
          
          ### Plots ----
          if (!is.na(rsq_val) && rsq_val > 0.981 && rsq_val < 0.989) {
            
            # Create a data frame containing both observed and predicted values
            plot_data_cal <- data.frame(Observed = train_response_int, Predicted = pred_cal)
            mxv_cal <- max(plot_data_cal)
            # write.csv(plot_data_cal, file = paste0(export_data_dir, plot_data_cal_name), row.names = F)
            
            plot_data_val <- data.frame(Observed = valid_response_int, Predicted = pred_val)
            mxv_val <- max(plot_data_val)
            # write.csv(plot_data_val, file = paste0(export_data_dir, plot_data_val_name), row.names = F)
            
            # Create plot names
            cal_plot_name <- paste0("_cal_", resample_name, "_fold_index_", fold_idx, "_repeat_", r, ".png")
            val_plot_name <- paste0("_val_", resample_name, "_fold_index_", fold_idx, "_repeat_", r, ".png")
            
            # Define export plot directory
            export_plot_dir <- file.path(dir_path, paste(target, pp, model, sep = "_"))
            
            # Call fun plot for Observed vs Predicted
            plt(data_obs_pred= plot_data_cal, 
                ttl= "Calibration", 
                lims= mxv_cal, 
                rsq= rsq_cal, 
                rmse_= rmse_cal, 
                plt_name= cal_plot_name, 
                plot_dir= export_plot_dir)
            
            plt(data_obs_pred= plot_data_val, 
                ttl= "Validation", 
                lims= mxv_val, 
                rsq= rsq_val, 
                rmse_= rmse_val, 
                plt_name= val_plot_name, 
                plot_dir= export_plot_dir)
            
          } # close plots 
          
          # Increment counter
          model_counter <- model_counter + 1
          
        } # end models
      } # end fold
      
      # Increment counter
      resample_counter <- resample_counter + 1
      
      rm(train_predictors_int, valid_predictors_int, train_response_int, valid_response_int)
      rm(plot_data_cal, plot_data_val, pred_cal, pred_val, fit)
      gc()
      
    } # end repeats
  } # end preproc
  
  rm(predictors_train, predictors_pred)
  gc()
  
} # end target


###¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
### End scrip ----
###_____________________________________________________________________________
