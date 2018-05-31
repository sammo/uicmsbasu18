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

# Distribution plots for numeric features
for (fName in names(allFeatures[numFeatures])){
	# Start graphic device
	png(filename=paste0("./Plots/", fName, ".png"),
			type="cairo", width=900, height=600)
	
	# Layout four graphs on the same graphic device
	layout(mat = matrix(c(1,2,3,4), nrow = 2, ncol = 2))
	
	# Set up margins
	par(mai = c(.5,.8,.5,.5), mgp = c(2,.7,0))
	
	# Variable list
	v = list()
	v$orig = reportData[, fName]
	v$orig.skew = skew(v$orig)
	
	# Plot Original variable
	hist(v$orig, cex.main = .9,
			 main = paste0("Skewness: ", as.character(round(v$orig.skew, 3))),
			 xlab = "Original")
	box()
	
	# Calculate and plot inverse transformation
	v$inv = 1/v$orig
	v$inv.skew = skew(v$inv)
	hist(v$inv, cex.main = .9,
			 main = paste0("Skewness: ", as.character(round(v$inv.skew, 3))),
			 xlab = "Inverse")
	box()
	
	# Calculate and plot natural log transformation
	v$log = log(v$orig)
	v$log.skew = skew(v$log)
	hist(v$log, cex.main = .9,
			 main = paste0("Skewness: ", as.character(round(v$log.skew, 3))),
			 xlab = "Natural Log")
	box()
	
	# Calculate and plot square root transformation
	v$sqrt = sqrt(v$orig)
	v$sqrt.skew = skew(v$sqrt)
	hist(v$sqrt, cex.main = .9,
			 main = paste0("Skewness: ", as.character(round(v$sqrt.skew, 3))),
			 xlab = "Square Root")
	box()
	
	# Close graphic device
	dev.off()
}

# use `supressWarnings` on skew

# EDA info ####
# Correlation matrix
cm = cor(reportData[, numFeatures])
write.csv(x = cm, file = "./Plots/Correlation.csv")

# EDA data
library(psych)
eda = describe(reportData[, numFeatures])
write.csv(x = eda, file = "./Plots/Describe.csv")

# Normalize variables
rd.norm = scale(x = reportData[, numFeatures], center = TRUE, scale = TRUE)

rd.test = scale(x = reportData[, numFeatures],
								center = attr(rd.norm, "scaled:center"),
								scale = attr(rd.norm, "scaled:scale"))

# Test if identical
identical(rd.norm, rd.test)


# Transform
reportData.tx = sqrt(reportData[, numFeatures])

# Can use `supressWarnings` on log
reportData.tx = cbind(reportData.tx,
											reportData[, names(allFeatures[-numFeatures])])

# Non-exhaustive Cross-Validation: Holdout ####
trainPerc = .75
nTrain = ceiling(dim(reportData.tx)[1] * trainPerc)
nValid = dim(reportData.tx)[1] - nTrain

# Split data into training / validation
indTrain = sample(x = 1:dim(reportData.tx)[1], size = nTrain)

# Move ReportID to row names to exclude from features
remVars = which(names(reportData.tx) %in% c("ReportID"))
data.train = reportData.tx[indTrain, -remVars]
data.valid = reportData.tx[-indTrain, -remVars]

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












