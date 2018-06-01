# Clean environment
rm(list=ls())

# Load Data file ####
reportData = read.csv(file = "DataFrame.csv",
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

# Random Forest
library(ranger)
mod.ranger = ranger(formula = reportFormula, data = data.train, num.trees = 100,
										importance = 'impurity', probability = TRUE)
valid.ranger = predict(object = mod.ranger, data = data.valid)

# Model importance
vi = mod.ranger$variable.importance[order(mod.ranger$variable.importance, 
																					decreasing = TRUE)]
barplot(vi, cex.names = .8, las = 2)
box()

# Get highest probability into prob
valid.ranger$prob = apply(X = valid.ranger$predictions, MARGIN = 1, FUN = max)
valid.ranger$pred = colnames(
	valid.ranger$predictions)[max.col(m = valid.ranger$predictions)]
valid.ranger$actual = as.character(data.valid$NextReport)

# Find optimal prediction threshold
pred = as.data.frame(valid.ranger[c("pred", "prob", "actual")],
										 stringsAsFactors = F)
threshM = data.frame(matrix(data = c(seq(from = 0, to = 1, by = .01), 
																		 rep(0, times = 505)),
														ncol = 6,
														nrow = 101,
														byrow = F,
														dimnames = list(c(), 
																						c("Thresh", "Total", "Count",
																							"Wrong", "PredPerc",
																							"ErrorRate"))))
threshM$Total = dim(pred)[1]

for (i in 1:101){
	threshM$Count[i] = length(which(pred$prob >= threshM$Thresh[i]))
	threshM$Wrong[i] = length(which(pred$prob >= threshM$Thresh[i] & 
																		pred$pred != pred$actual))
}
threshM$PredPerc = threshM$Count / threshM$Total
threshM$ErrorRate = threshM$Wrong / threshM$Count

# Graph threshold
par(mai = c(1, .8, .8, .8))
yLmarks = seq(from = 0, to = 1.1, by = .1)
yRmax = ceiling(max(threshM$ErrorRate * 10))/10
yRmarks = seq(from = 0, to = yRmax, by = yRmax/10)
xMarks = seq(from = 0, to = 1, by = 0.1)

plot(x = threshM$Thresh, y = threshM$PredPerc, col = "SteelBlue",
		 axes = FALSE, ylab = NA, xlab = "Threshold", main = "Prediction Threshold")
box()
axis(side = 1, at = xMarks, labels = TRUE)
axis(side = 2, at = yLmarks, labels = TRUE, col.axis = "SteelBlue", 
		 col.ticks = "SteelBlue")
mtext(side = 2, text = "Percent Predicted", col = "SteelBlue", line = 2)
par(new = TRUE)
plot(x = threshM$Thresh, y = threshM$ErrorRate, col = "Tomato",
		 axes = FALSE, ylab = NA, xlab = NA)
axis(side = 4, at = yRmarks, labels = TRUE, col.axis = "Tomato",
		 col.ticks = "Tomato")
mtext(side = 4, text = "Error Rate", col = "Tomato", line = 2)

library(plotly)
p = plot_ly(
	x = 100*threshM$Thresh, 
	y = 100*threshM$PredPerc, 
	type = "scatter",
	mode = "lines",
	name = "Percent Predicted",
	line = list(width = 4))

p = add_trace(
	p,
	x = 100*threshM$Thresh, 
	y = 100*threshM$ErrorRate, 
	mode = "lines",
	name = "Error Rate", 
	yaxis = "y2", 
	line = list(width = 4))

ay <- list(
	tickfont = list(color = "red"),
	overlaying = "y",
	side = "right",
	title = "second y axis"
)

p = layout(
	p,
	title = "Threshold Prediction",
	xaxis = list(
		title = "Probability Threshold"),
	yaxis2 = ay,
	legend = list(x = 1.06))

p










