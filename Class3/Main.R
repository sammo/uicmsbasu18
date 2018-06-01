# Clean environment
rm(list=ls())

# Source
source("./Class3/RedTomato.R")

# Load Data file ####
reportData = read.csv(file = "./DataFrame.csv",
                      header = TRUE, quote = '"')

# Remove constant or identity variables
remVars = c("ReportPath", "InsertTimeStamp", "ReportYear")
remVarsInd = which(names(reportData) %in% remVars)
reportData = reportData[, -remVarsInd]

# Plots in a loop ####
# Get numeric features
allFeatures = sapply(reportData, class)
numFeatures = which(allFeatures %in% c("integer","numeric"))

# Generate all plots for all features

for (fName in names(allFeatures[numFeatures])){
  RedTomato(x = reportData[, fName], fName = fName)
}

# Apply function example
meanRD = apply(X = reportData[, numFeatures],
               MARGIN = 2,
               FUN = function(y){
                 return(mean(y)*mean(y)+30)
               })

# Sub apply for for loop in RedTomato example
mapply(FUN = RedTomato,
       x = reportData[, numFeatures],
       fName = names(reportData)[numFeatures]
       )
