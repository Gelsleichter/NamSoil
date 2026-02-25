#¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
# Mapping NEON sites -- functions
# Yuri Andrei Gelsleichter
# May 2025 
#_______________________________________________________________________________

#¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
# Function to create and save the plot ----
#_______________________________________________________________________
library(tune) # for coord_obs_pred()
library(ggplot2) # for ggplot()
library(yardstick) # for ccc_vec()

#¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
# Calculate the PICP - prediction interval coverage probability ----
# https://www.r-bloggers.com/2023/08/calculating-the-prediction-interval-coverage-probability-picp/
#_______________________________________________________________________________

# library(tune)
calcPICP = function(data, response, pred, title_label, exp_plt_dir, plt_name){ # add exp_plt_dir, plt_name for case worflow
  
  # We first get the residuals of the model
  res = response - pred
  
  # Then we get the standard deviation of the residuals and combine with the data.
  data$stdev = sd(res)
  
  # We than make a series of quantiles from a normal cumulative distribution.
  qp <- qnorm(c(0.995, 0.9875, 0.975, 0.95, 0.9, 0.8, 0.7, 0.6, 0.55, 0.525))
  
  # Then make a matrix the with the row length of the data and columns of qp
  vMat <- matrix(NA, nrow = nrow(data), ncol = length(qp))
  
  # Now we must loop around the quantiles and multiply it by the standard deviation to get a series of standard errors with different prediction intervals. 
  for(i in 1:length(qp)){
    vMat[,  i] <- data$stdev * qp[i]
  }
  
  # Make another matrix same as before for the upper limits
  uMat <- matrix(NA, nrow = nrow(data), ncol = length(qp))
  
  # We calculate the upper limits by adding the series of standard errors to the predictions of the model. 
  for(i in 1:length(qp)) {
    uMat[,  i] <- pred + vMat[, i]
  }
  
  # We make another matrix for the lower limits
  lMat <- matrix(NA, nrow = nrow(data), ncol = length(qp))
  
  # We calculate the lower limits by subtracting the series from the predicted values.
  for(i in 1:length(qp)) {
    lMat[, i] <- pred - vMat[,  i]
  }
  
  # Now we want to see which prediction intervals cover the measured data creating a matrix of 1s and 0s. 
  bMat <- matrix(NA, nrow = nrow(data), ncol = length(qp))
  
  for(i in 1:ncol(bMat)){
    bMat[, i] <- as.numeric(response <= uMat[,  i]  &
                              response >= lMat[, i])
  }
  
  # To calculate the PICP we take the colsums/nrow*100 for the matrix of 0s and 1s
  picp <- colSums(bMat)/nrow(bMat)*100
  
  # Make a vector of confidence levels
  cl <- c(99, 97.5, 95, 90, 80, 60, 40, 20, 10, 5)
  
  # We put into a data frame for plotting
  results <- data.frame(picp = picp, cl = cl)
  
  # Since we want PICP to CI to be a 1:1 line we also calculate Lin’s concordance correlation coefficient (CCC) with the yardstick R package.
  ccc <- as.data.frame(yardstick::ccc_vec(results$picp, results$cl))
  
  # Make name correct
  names(ccc) = "CCC" #name
  
  # must add axis values for plotting
  ccc$x = 12 #x axis
  ccc$y = 90 #y axis
  
  # Now we can plot the PICP to CI, add the 1:1 line and the CCC
  p <- ggplot(data = results, aes(x= cl, y = picp)) + # add data
    geom_point() + # add points
    # geom_text(data = ccc, aes(x= x, y =y, label = paste("CCC = ", round(CCC, 2)))) + # add CCC value
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = 'red')+ # add 1:1 line
    # labs(x = 'Confidence level', y = 'PICP', title = 'PICP to confidence level') + # labels
    labs(x = 'PI level (%)', y = 'PICP (%)', title = title_label) + # labels
    # coord_fixed(ratio=1) +
    tune::coord_obs_pred() + # make the plot square # https://tune.tidymodels.org/reference/coord_obs_pred.html
    theme_bw() + # make it look good
    theme(text = element_text(size= 14), # increase base text size 
          plot.title = element_text(size = 14)) # increase text size on title
    
  
  # Save the plot
  ggsave(filename = paste0(exp_plt_dir, plt_name), 
         plot = p, 
         width = 6, 
         height = 6, 
         dpi = 300)
  
  # Now we want to return a list of the plot as well as a data frame of the total results.
  return(setNames(list(p, results, ccc), c("Plot", "Results", "CCC")))
}

# # Now we have the function giving us a plot of the PICP to CI and results. This is useful when running many models and now we can just plug in the data.
# 
# # get the values to input in the function
# # get predictions
# pred_test <- predict(qrf_model_f, data = test_predictors, 
#                      type = "quantiles", quantiles = 0.5)$predictions
# # dat <- cbind(test_predictors, pred = pred_test[, 1]) # combine the data
# dat <- as.data.frame(cbind(SOC= test_response, pred = pred_test[, 1])) # combine the data
# 
# # picp = picpCalc(dat, dat$clay, dat$pred)
# picp = calcPICP(dat, dat$SOC, dat$pred)
# 
# # now, plot the data.
# picp[1]
# picp[2]
# 
# ggsave(filename= paste0("../output/picp_plot.png"), 
#        plot = picp[[1]], 
#        width = 9, height = 9, dpi = 300)

# ################################################################################
# # Example data 
# dat_matrix <- matrix(c(
#   1.674, 1.915998,
#   3.262, 3.703449,
#   2.442, 2.340370,
#   1.842, 2.627656,
#   1.917, 2.039686,
#   1.072, 1.722476,
#   0.999, 1.421247,
#   1.499, 1.792844,
#   0.921, 1.795698,
#   1.747, 1.763379,
#   1.521, 2.149659,
#   2.323, 2.363802
# ), byrow = TRUE, ncol = 2)
# 
# dat <- as.data.frame(dat_matrix)
# colnames(dat) <- c("SOC", "pred")
# 
# dat
# picp = calcPICP(dat, dat$SOC, dat$pred)

#¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
# End script ----
#_______________________________________________________________________________

