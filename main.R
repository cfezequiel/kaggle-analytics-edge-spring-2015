# ============
# Load scripts
# ============
source('dataprep.R')
source('util.R')

# ============
# Extract data
# ============
news.train = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
news.test = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

# ==============
# Transform data
# ==============
# Combine train and test data
out = combine(news.train, news.test)
news = out$df
Popular = out$y

# Transform variables
news = transform(news)
Popular = as.factor(Popular)

# Add Bag-of-Words variables
news = bow(news)

# Split train and test data
o = splitTrainTest(news, Popular, "Popular")
news.train = o$train
news.test = o$test

# ==========
# Models
# ==========
# --------
# Baseline
# --------
t = table(news.train$Popular)
accuracy(t) # accuracy = 0.8326699 (predict all FALSE)

# -------------------
# Logistic Regression
# -------------------
# Build LR model and check summary
log = glm(Popular ~. - UniqueID, data=news.train, family=binomial)

# Check training performance
verify(log, news.train$Popular)

# Make predictions on the test set
pred.test = predict(log, newdata=news.test, type="response")

# Make submission (LOG)
makeSubmitFile(news.test$UniqueID, pred.test, "lm8.csv", "submit")

# -------------
# Random Forest
# -------------
library(randomForest)

# Tune
library(caret)
set.seed(69)
#rf.model = train(Popular ~. - UniqueID, data=news.train, method="rf", trControl=trainControl(method="cv", number=5), prox=TRUE)
numFolds <- trainControl(method="cv", number=5)
rf.caret <- train(Popular ~. - UniqueID, data=news.train, method='rf', trControl=numFolds)
rf.caret

# Build model
rf = randomForest(Popular ~ . - UniqueID, data=news.train, ntree=500, nodesize=20, mtry=35)

# Get accuracy of RF on training set
verify(rf, news.train$Popular, type="prob")

# Make a submission 
pred.test = predict(rf, newdata=news.test, type="prob")
makeSubmitFile(news.test$UniqueID, pred.test[,2], "rf8.csv", "submit")
# Last score: 0.91764 (no improv)

# Tune RF
Popular = NewsTrain$Popular
UniqueID = NewsTrain$UniqueID
NewsTrain$Popular = NULL
NewsTrain$UniqueID = NULL
NewsRFTune = tuneRF(NewsTrain, Popular)
NewsTrain$Popular = Popular
NewsTrain$UniqueID = UniqueID

# Use cross validation
library(caret)
library(e1071)
set.seed(69)
numFolds <- trainControl(method="cv", number=10)
grid <- expand.grid(mtry=12)
train(Popular ~ . - UniqueID, data=NewsWordsTrain, method="rf", trControl=numFolds, tuneGrid=grid)


# ----------
# CART model
# ----------
library(rpart)
library(rpart.plot)

# Use cross validation
library(caret)
library(e1071)
set.seed(69)
numFolds <- trainControl(method="cv", number=10)
grid <- expand.grid(.cp = seq(0.002,0.1,0.002))
train(Popular ~ . - UniqueID, data=NewsWordsTrain, method="rpart", trControl=numFolds, tuneGrid=grid)

# Build model
NewsCART = rpart(Popular ~ . - UniqueID, data=NewsWordsTrain, cp=0.002)

# Check
prp(NewsCART)

# Training performance
NewsTrainCARTPred = predict(NewsCART, type="prob")
t = table(NewsTrain$Popular, NewsTrainCARTPred[,2] > 0.5)
accuracy(t)
aucroc(NewsTrainCARTPred[,2], NewsTrain$Popular)

# Make a submission 
NewsTestCARTPred = predict(NewsCART, newdata=NewsWordsTest, type="prob")
makeSubmitFile(NewsWordsTest$UniqueID, NewsTestCARTPred[,2], "MySubmissionCART.csv", "submit")
