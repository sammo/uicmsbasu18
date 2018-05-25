# Summarize / top 5
View(summary(reportData))

# EDA with psych
if(!("psych" %in% row.names(installed.packages()))){
  install.packages("psych")
}
library(psych)

View(describe(reportData))

View(describeBy(reportData, group = "NextReport"))

# EDA with pastecs
if(!("pastecs" %in% row.names(installed.packages()))){
  install.packages("pastecs")
}
View(pastecs::stat.desc(reportData))

write.csv(pastecs::stat.desc(reportData),file = "./Class2/pastecs_statdesc.csv")
