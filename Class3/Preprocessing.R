# Clean environment
rm(list=ls())

# Load Data file ####
reportData = read.csv(file = "./DataFrame.csv",
											header = TRUE, quote = '"')

# Remove constant or identity variables
remVars = c("ReportPath", "InsertTimeStamp", "ReportYear")
remVarsInd = which(names(reportData) %in% remVars)
reportData = reportData[, -remVarsInd]

# Non-exhaustive Cross-Validation: Holdout ####
trainPerc = .75
nTrain = ceiling(dim(reportData)[1] * trainPerc)
nValid = dim(reportData)[1] - nTrain

# Split data into training / validation
indTrain = sample(x = 1:dim(reportData)[1], size = nTrain)

# Move ReportID to row names to exclude from features
rownames(reportData) = as.character(reportData$ReportID)
remVars = which(names(reportData)== "ReportID")
data.train = reportData[indTrain, -remVars]
data.valid = reportData[-indTrain, -remVars]

# Model training & Validation####
targetVar = "NextReport"
reportFormula = formula("NextReport ~ .")

# Linear Discriminant Analysis
library(MASS)
mod.lda = lda(formula = reportFormula, data = data.train)
valid.lda = predict(object = mod.lda, newdata = data.valid)

# Get highest probability into prob
valid.lda$prob = apply(X = valid.lda$posterior, MARGIN = 1, FUN = max)
valid.lda$sum = apply(X = valid.lda$posterior, MARGIN = 1, FUN = sum)

# Calculate the error rate
valid.lda$err = length(which(valid.lda$class!=data.valid$NextReport)) /
	dim(data.valid)[1]

# Variable transformations
# i.e. ReportHour
if(names(dev.cur()) != "null device"){
	dev.off()
}
rh = list()

# Original variable
library(psych)
rh$orig = reportData$ReportHour
rh$orig.skew = skew(rh$orig)
hist(rh$orig, main = 
		 	paste0("Skewness: ", as.character(round(rh$orig.skew, 3))))
box()

# Layout four graphs on the same graphic device
layout(mat = matrix(c(1,2,3,4), nrow = 2, ncol = 2))

# Set up margins
par(mai = c(.5,.8,.5,.5), mgp = c(2,.7,0))

# Plot Original variable
hist(rh$orig, cex.main = .9,
		 main = paste0("Skewness: ", as.character(round(rh$orig.skew, 3))),
		 xlab = "Original")
box()

# Calculate and plot inverse transformation
rh$inv = 1/rh$orig
rh$inv.skew = skew(rh$rh.inv)
hist(rh$inv, cex.main = .9,
		 main = paste0("Skewness: ", as.character(round(rh$inv.skew, 3))),
		 xlab = "Inverse")
box()

# Calculate and plot natural log transformation
rh$log = log(rh$orig)
rh$log.skew = skew(rh$log)
hist(rh$log, cex.main = .9,
		 main = paste0("Skewness: ", as.character(round(rh$log.skew, 3))),
		 xlab = "Natural Log")
box()

# Calculate and plot square root transformation
rh$sqrt = sqrt(rh$orig)
rh$sqrt.skew = skew(rh$sqrt)
hist(rh$sqrt, cex.main = .9,
		 main = paste0("Skewness: ", as.character(round(rh$sqrt.skew, 3))),
		 xlab = "Square Root")
box()


# Plots in a loop ####
# Get numeric features
allFeatures = sapply(reportData, class)
numFeatures = which(allFeatures %in% c("integer","numeric"))

# Create plots folder if it doesn't exist
if (!("Plots" %in% dir())){
	dir.create(path = "Plots")
}











