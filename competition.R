NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

# Clean the data
# - Clean training data
NewsTrain$NewsDesk[NewsTrain$NewsDesk == ''] = 'None'
NewsTrain$SectionName[NewsTrain$SectionName == ''] = 'None'
NewsTrain$NewsDesk = as.factor(NewsTrain$NewsDesk)
NewsTrain$SectionName = as.factor(NewsTrain$SectionName)
NewsTrain$Popular = as.factor(NewsTrain$Popular)

# - Clean test data
NewsTest$NewsDesk[NewsTest$NewsDesk == ''] = 'None'
NewsTest$SectionName[NewsTest$SectionName == ''] = 'None'
NewsTest$NewsDesk = as.factor(NewsTest$NewsDesk)
NewsTest$SectionName = as.factor(NewsTest$SectionName)

# Check the data
summary(NewsTrain)
summary(NewsTest)

# Build LR model and check summary
NewsLog = glm(Popular ~ WordCount + NewsDesk + SectionName, data=NewsTrain, family=binomial)
summary(NewsLog)

# Check predictions on the training set
NewsTrainPred = predict(NewsLog)
table(NewsTrain$Popular, NewsTrainPred > 0.5)
(5282 + 612) / nrow(NewsTrain) # accuracy = 0.902327

# Check the baseline
table(NewsTrain$Popular)
(5439) / nrow(NewsTrain) # accuracy = 0.8326699 (predict all FALSE)

# Make predictions on the test set
NewsTestPred = predict(NewsLog, newdata=NewsTest, type="response")

# Make submission (LOG)
submission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = NewsTestPred)
write.csv(submission, "MySubmissionLog.csv", row.names=FALSE)
# Result: 0.88892, 610th place as of Mon, 20 Apr 2015 14:31:50
# Not bad.

# Try a CART model
library(rpart)
library(rpart.plot)
NewsCART = rpart(Popular ~ WordCount + NewsDesk + SectionName, data=NewsTrain)
prp(NewsCART)

# Get accuracy of training set
NewsTrainPred = predict(NewsCART)
table(NewsTrain$Popular, NewsTrainPred > 0.5)
(5291 + 608) / nrow(NewsTrain) # 0.9030925 (very slightly better than LOG)

# Now try a RF model
library(randomForest)
set.seed(69)
NewsRF = randomForest(Popular ~ WordCount + NewsDesk + SectionName, data=NewsTrain)

# Get accuracy of RF on trainng set
NewsTrainPred = predict(NewsRF)
table(NewsTrain$Popular, NewsTrainPred > 0.5)
(5248 + 662) / nrow(NewsTrain) # 0.9047765 (another slighly better improvement vs. CART)

# Make a submission 
NewsTestPred = predict(NewsRF, newdata=NewsTest)
