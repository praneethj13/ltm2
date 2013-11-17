# Loading Libraries
library(softImpute)
library(caret)
library(ggplot2)
library(gridExtra)
source("E:/Learn R/LTM/ltm/ImputeHelp.R")

# Loading full data
Data_Full <- read.csv("E:/Learn R/LTM/ltm/RatingData.csv")
Data_Full <- as.matrix(Data_Full[, -1]) # Removing ID

P <- ceiling(ncol(Data_Full)*.3)
cat(" Total Respondents", nrow(Data_Full), "\n", 
    "Total Variables  ", ncol(Data_Full), "\n",
    "Non-Impute Count", P)

# Creating Incomplete Matrix
Data_Incomplete <- IncMat(Data_Full, P)

# Imputing data
fit <- softImpute(Data_Incomplete, rank.max = P, lambda = 5)
Data_Impute <- complete(Data_Incomplete, fit, unscale = T)

Data_Full <- as.data.frame(Data_Full)
Data_Impute <- as.data.frame(Data_Impute)
rm(Data_Incomplete, P, fit)

k = 20
CF <- createFolds(seq(nrow(Data_Full)), k , list = TRUE)
doplot = function(i){
  Sub_Org <- Data_Full[CF[[i]], ]
  Sub_Imp <- Data_Impute[CF[[i]], ]
  MFull <- apply(Sub_Org, 2, mean)
  MImp <- apply(Sub_Imp, 2, mean)
  MFrame <- as.data.frame(cbind(MFull, MImp))
  MPlot(MFrame, i)
}

Plot2 = lapply(1:k, doplot)

# Creating Matrix Plots
multiplot <- do.call(marrangeGrob, c(Plot2, list(nrow = 2, ncol = 2)))
# Saving Matrix Plots in PDF
library(Cairo) #High Resolution Plotting
CairoPDF("E:/Learn R/LTM/ltm/SubCorr.pdf", 12, 6)
multiplot
dev.off()

