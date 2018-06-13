# Purpose of this script is to showcase some ways interpt data both using the
#  matrix itself or a trained model.

# We will later use randomForest for our model.  For "cleaner" code the utility
#  functions have been moved to utils.R and loaded in.  Review these functions
#  and think how they could be improved for efficency.
library(randomForest)
source('utils.R')

# load data
#  stringsAsFactors=FALSE - prevents R automatically making factors
df = read.csv(
  file = '../data/data.csv',
  header = TRUE,
  quote = '"',
  stringsAsFactors=FALSE
)

# remove id vars
id.vars = c(
  'ReportID',
  'ReportPath',
  'InsertTimeStamp'
)
df = df[, -which(names(df) %in% id.vars)]

# First, we are going to one-hot encode and scale the matrix.  The function we
#  wrote to do this (in utils.R) requres the categorical features so we need to
#  make a list of them.
cat.vars = c(
  'ReportCode',
  'InsertUser',
  'NextReport'
)
df.onehot.and.scaled = PrepareDataFrameForReduction(
                         df,
                         var.names=cat.vars,
                         id.name=FALSE,
                         split.size=1e5
)

# Check the dimensions of the original matrix (df) and the prepped matrix
#  (df.onehot.and.scaled).  Why did they change the way they did?
#
# Remember, doing distance calculations, clustering, similarity, etc.. is very
#  computaionally expensive.  It is best to reduce our matrix and preserve as
#  much information as possible.
df.reduced = GetReducedMatrix(
               df.onehot.and.scaled,
               k=50,
               split.size=1e5
)

# Dump data.
#save(df, df.onehot.and.scaled , df.reduced, file='/work/data/data_50row.RData')
load('/work/data/data_50row.RData')

# Let's take observation '1' and find its distance from all other observations
#  using two distance metrics.  Please review the function in utils.R to see
#  what we are doing and how that code could be improved.
euclidean_1_vs_all = GetDistance(df.reduced, '1')
cosine_1_vs_all = GetDistance(df.reduced, '1', 'cosine')

# Dump data
#save(euclidean_1_vs_all, cosine_1_vs_all, file='/work/data/data_1vsall.RData')
load('/work/data/data_1vsall.RData')

# Let's order based on distance so the "closest" observations are on top.
euclidean_1_vs_all =
  euclidean_1_vs_all[order(euclidean_1_vs_all$dist), , drop=FALSE]
cosine_1_vs_all =
  cosine_1_vs_all[order(cosine_1_vs_all$dist), , drop=FALSE]

# Take this time to manually review observation pairs.  Do they look like they
#  are close?  Which distance metrics (in this specific example) seems better?

# This gives you the top n "closest" observations
n = 10
rownames(euclidean_1_vs_all)[1:n]
rownames(cosine_1_vs_all)[1:n]
#df[c('1', ???), ] - use this to observe pairs


# ##############################################################################
#                             Tree based similarity
# ##############################################################################

# So, randomForest hates the '\' in the column names so we need to remove them
#  from the values of InsertUser before one-hot encoding.
df[, 'InsertUser'] =
  as.vector(sapply(df[, 'InsertUser'], function(x) {sub('\\\\', '', x)}))
head(df)

# Prep the matrix, now we will tell the function which var is the resp.var so
#  that it is remvoed.  We will readd it later.
df.prepped = PrepareDataFrameForReduction(
               df,
               var.names=c('ReportCode', 'InsertUser'),
               id.name=FALSE,
               split.size=1e5,
               resp.var='NextReport'
)

# Let's build a training-valid split.  Note that sample(nrow(A)) returns a list
#  of numbers 1 through A in random order.  We then access that random list and
#  take the first "train.ratio" percent.  This gives us a random subset of the
#  size we want.  There are other ways of doing this!
train.ratio = 0.8
train.inds = sample(nrow(df.prepped))[1:round(nrow(df.prepped)*train.ratio)]

# Make the splits.
df.train = df.prepped[train.inds, ]
df.valid = df.prepped[-train.inds, ]

# Now, we can re add our response variable AND make it into a factor as
#  randomForest requires that.  the rownmanes(df.train) makes sure we are
#  pulling thr response for only to rows in df.train AND they are in the new
#  random order.
df.train$NextReport = as.factor(df[rownames(df.train), 'NextReport'])

# Train a very simple model
rf = randomForest(NextReport ~ ., data=df.train)

# Dump data
#save(rf, file='/work/data/rf.RData')
load('/work/data/rf.RData')

# What does predict.all do?  How could we leverage this information to find
#  predictions that are "more" similar?
rf.pred = predict(rf, df.valid, predict.all=TRUE)
