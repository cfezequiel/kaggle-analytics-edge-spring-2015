NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

# Clean the data
# - Clean training data
NewsTrain$NewsDesk = as.factor(NewsTrain$NewsDesk)
NewsTrain$SectionName = as.factor(NewsTrain$SectionName)
# - Clean test data
NewsTest$NewsDesk = as.factor(NewsTest$NewsDesk)
NewsTest$SectionName = as.factor(NewsTest$SectionName)

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

# Make submission
submission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = NewsTestPred)
write.csv(submission, "MySubmissionLog.csv", row.names=FALSE)
# Result: 0.88892, 610th place as of Mon, 20 Apr 2015 14:31:50
# Not bad.

