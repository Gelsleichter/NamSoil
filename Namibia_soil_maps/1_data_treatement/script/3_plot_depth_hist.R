################################################################################
### Define working directory for input output files in correct places
################################################################################
#' ### Auto setwd
#' ### Define the working directory as the folder of the script
library(rstudioapi); current_path <- getActiveDocumentContext()$path; setwd(dirname(current_path)) ### https://eranraviv.com/r-tips-and-tricks-working-directory/
getwd()
gc(); rm(list=ls())


df <- read.csv("../output/datasets/dataset_for_splines/Dataset_for_splines_2025_03_03.csv")
table(df$Upper.Depth)
table(df$Lower.Depth)

hist(df$Upper.Depth, breaks=100)
hist(df$Upper.Depth[df$Upper.Depth <= 100], breaks=100)
hist(df$Lower.Depth[df$Lower.Depth <= 150], breaks=100)

{
png("../output/datasets/dataset_for_splines/Upper_Depth_histogram.png", 
    res= 300, units= "cm", width=22, height=22, pointsize = 12)
par(mfrow=c(2,1), mar=c(4, 4, 6, 1), cex.axis=0.8, cex.lab=0.8) # bottom, left, top, right margins
# Histogram for Upper.Depth (values <= 100)
hist_data_upper <- hist(df$Upper.Depth[df$Upper.Depth <= 100], breaks=100, 
                        main="Histogram of Upper Depth (<= 100)", 
                        xlab="Upper Depth", ylab="Frequency", ylim=c(0, 5500),  
                        col="lightblue", border="black", xaxt="n") # Hide X axis
# add x-axis labels at every 10 units
axis(1, at=seq(0, 100, by=10), labels=seq(0, 100, by=10))
text(hist_data_upper$mids, hist_data_upper$counts,
     labels=paste(hist_data_upper$counts, "\n", round(hist_data_upper$mids + 0.5, 2)), 
     pos=3, cex=0.6, col= "blue")

# Histogram for Lower.Depth (values <= 150)
hist_data_lower <- hist(df$Lower.Depth[df$Lower.Depth <= 150], breaks=100, 
                        main="Histogram of Lower Depth (<= 150)", 
                        xlab="Lower Depth", ylab="Frequency", ylim=c(0, 1650),  
                        col="lightblue", border="black", xaxt="n")
axis(1, at=seq(0, 150, by=10), labels=seq(0, 150, by=10))
text(hist_data_lower$mids, hist_data_lower$counts, 
     labels=paste(hist_data_lower$counts, "\n", round(hist_data_lower$mids + 1, 2)), 
     pos=3, cex=0.6, col= "blue")
dev.off()
}

