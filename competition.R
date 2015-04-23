# ============
# Load scripts
# ============
source('dataprep.R')
source('util.R')

# ==========
# Run models
# ==========
# --------
# Baseline
# --------
t = table(NewsWordsTrain$Popular)
accuracy(t) # accuracy = 0.8326699 (predict all FALSE)

# -------------------
# Logistic Regression
# -------------------
# Build LR model and check summary
NewsLog = glm(Popular ~. - UniqueID, data=NewsWordsTrain, family=binomial)

# Check training performance
NewsTrainLogPred = predict(NewsLog)
t = table(NewsWordsTrain$Popular, NewsTrainLogPred > 0.5)
accuracy(t)
aucroc(NewsTrainLogPred, NewsWordsTrain$Popular)

# Make predictions on the test set
NewsTestLogPred = predict(NewsLog, newdata=NewsWordsTest, type="response")

# Make submission (LOG)
makeSubmitFile(NewsWordsTest$UniqueID, NewsTestLogPred, "MySubmissionLog5.csv", "submit")

# ----------
# CART model
# ----------
library(rpart)
library(rpart.plot)

# Build model
UniqueID = NewsWordsTrain$UniqueID
NewsWordsTrain$UniqueID = NULL
NewsCART = rpart(Popular ~ ., data=NewsWordsTrain)
NewsWordsTrain$UniqueID = UniqueID

# Check
prp(NewsCART)

# Training performance
NewsTrainCARTPred = predict(NewsCART)
t = table(NewsTrain$Popular, NewsTrainCARTPred[,2] > 0.5)
accuracy(t)
aucroc(NewsTrainCARTPred[,2], NewsTrain$Popular)

# -------------
# Random Forest
# -------------
library(randomForest)

# Get tuning parameter/s
# TODO

set.seed(69)
NewsRF = randomForest(Popular ~ . - UniqueID, data=NewsWordsTrain, ntree=500, nodesize=20, mtry=6, method="class")

# Get accuracy of RF on training set
NewsTrainRFPred = predict(NewsRF)
t = table(NewsWordsTrain$Popular, NewsTrainRFPred > 0.5)
accuracy(t)

# Make a submission 
NewsTestRFPred = predict(NewsRF, newdata=NewsTest, type="response")
makeSubmitFile(NewsWordsTest$UniqueID, NewsTestRFPred, "MySubmissionRF5.csv", "submit")

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
