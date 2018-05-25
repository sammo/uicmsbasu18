# Clean environment
rm(list=ls())

# Pull data flag
pullFromDB = TRUE


# Get data ####

if (pullFromDB){
  
  # Get data from database
  
  # db.connection = paste0("Driver={SQL Server};",
  # 											 "Server=;",
  # 											 "Database=UIC;",
  # 											 "Trusted_connection=Yes")
  # 
  # 
  # db.connection = paste0("Driver=/usr/local/lib/libtdsodbc.so;",
  # 											 "PORT=1433;",
  # 											 "Server=CHRPF36Z726\\SQLEXPRESS;",
  # 											 "Database=UIC;",
  # 											 "UID=myuser;",
  # 											 "PWD=mypwd")
  
  # Set database connection string
  db.connection = paste0(
    "Driver=/usr/lib/x86_64-linux-gnu/odbc/libtdsodbc.so;",
    "PORT=1433;",
    "Server=172.17.0.2;",
    "Database=UIC;",
    "UID=sa;",
    "PWD=ABcd12#$;")
  
  # Create connection
  library(RODBC)
  dbConn = odbcDriverConnect(connection=db.connection)
  
  # Pull data from DB
  reportData = sqlQuery(dbConn, "SELECT * FROM dbo.DataFrame")
  
  dimReportCode = sqlQuery(dbConn, "SELECT * FROM dbo.dimReportCode")
  
  dimUser = sqlQuery(dbConn, "SELECT * FROM dbo.dimUser")
  
  close(dbConn)
  
} else{
  
  # Load Data from file
  reportData = read.csv(file = "DataFrame.csv",
                        header = TRUE, quote = '"')
  
}

# Remove constant or identity variables
remVars = c("ReportID", "ReportPath", "InsertTimeStamp")
remVarsInd = which(names(reportData) %in% remVars)
reportData = reportData[, -remVarsInd]

# Non-exhaustive Cross-Validation: Holdout ####
trainPerc = .75
nTrain = ceiling(dim(reportData)[1] * trainPerc)
nValid = dim(reportData)[1] - nTrain

# Split data into training / validation
indTrain = sample(x = 1:dim(reportData)[1], size = nTrain)

# rownames(reportData) = reportData$ReportID
data.train = reportData[indTrain, ]
data.valid = reportData[-indTrain, ]

# Model training & Validation####
targetVar = "NextReport"
reportFormula = formula("NextReport ~ .")

# Linear Model
mod.lm = lm(formula = reportFormula, data = data.train)
pred.lm = predict(object = mod.lm, newdata = data.valid)

# Linear Discriminant Analysis
if(!("MASS" %in% row.names(installed.packages()))){
  install.packages("MASS")
}
library(MASS)
mod.lda = lda(formula = reportFormula, data = data.train)
pred.lda = predict(object = mod.lda, newdata = data.valid)

# ReportYear causing an error because it's constant. Remove it.
# Remove constant or identity variables
remVars = c("ReportYear")
remVarsInd = which(names(reportData) %in% remVars)
reportData = reportData[, -remVarsInd]

# Linear Discriminant Analysis, again
mod.lda = lda(formula = reportFormula, data = data.train)
pred.lda = predict(object = mod.lda, newdata = data.valid)

# Get highest probability into prob
pred.lda$prob = apply(X = pred.lda$posterior, MARGIN = 1, FUN = max)
pred.lda$sum = apply(X = pred.lda$posterior, MARGIN = 1, FUN = sum)

# # This line can be used to find out the predicted report
# # It already exist in "Class"
# pred.lda$pred = colnames(pred.lda$posterior)[max.col(m = pred.lda$posterior)]

# Calculate the error rate
err = length(which(pred.lda$class!=data.valid$NextReport)) /
	dim(data.valid)[1]


# Random Forest ####
if(!("randomForest" %in% row.names(installed.packages()))){
  install.packages("randomForest")
}
library(randomForest)
mod.rf = randomForest(formula = reportFormula, data = data.train)













