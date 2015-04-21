# ============
# Extract data
# ============
NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

# ==============
# Transform data
# ==============
# -- Combine the training and test data
News = NewsTrain
News$Popular = NULL
News = rbind(News, NewsTest)

# -- Use log of WordCount
News$WordCountLog = log(1 + News$WordCount)
News$WordCount = NULL

# -- Change some variables to factors
News$NewsDesk = as.factor(News$NewsDesk)
News$SectionName = as.factor(News$SectionName)
News$SubsectionName = as.factor(News$SubsectionName)
Popular = as.factor(NewsTrain$Popular)

# -- Change PubDate to Year Month Day
PubDate = strptime(News$PubDate, "%Y-%m-%d %H:%M:%S")
News$PubDate = NULL
#News$PubYear = PubDate$year # It's all 2014!
#News$PubMonth = PubDate$mon
#News$PubDay = PubDate$mday
News$PubWeekday = PubDate$wday
News$PubHour = PubDate$hour

# -- Create function to apply BofW to text
library(tm)
corpufy = function(v, prefix, sparsity=0.99) {
  # Then create a corpus from the headline variable. You can use other variables in the dataset for text analytics, but we will just show you how to use this particular variable. 
  # Note that we are creating a corpus out of the training and testing data.
  
  corpus = Corpus(VectorSource(v))
  corpus = tm_map(corpus, tolower)  
  corpus = tm_map(corpus, PlainTextDocument)  
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, stopwords("english"))  
  corpus = tm_map(corpus, stemDocument)
  dtm = DocumentTermMatrix(corpus)
  sparse = removeSparseTerms(dtm, sparsity)
  words = as.data.frame(as.matrix(sparse))
  
  # Let's make sure our variable names are okay for R
  colnames(words) = make.names(colnames(words))
  colnames(words) = paste(prefix, colnames(words), sep='_') 
  return(words)
}

# -- Transform text variables to BofW
HeadlineWords = corpufy(News$Headline, 'H', 0.98)
HeadlineWords$UniqueID = News$UniqueID
SnippetWords = corpufy(News$Snippet, 'S', 0.96)
SnippetWords$UniqueID = News$UniqueID
AbstractWords = corpufy(News$Abstract, 'A', 0.96)
AbstractWords$UniqueID = News$UniqueID

# -- Combine the BoW data frames to original df
# --- Headline
NewsWords = News
NewsWords = merge(NewsWords, HeadlineWords, by="UniqueID")
Headline = News$Headline
NewsWords$Headline = NULL
# --- Snippet
NewsWords = merge(NewsWords, SnippetWords, by="UniqueID")
Snippet = News$Snippet
NewsWords$Snippet = NULL
# --- Abstact
NewsWords = merge(NewsWords, AbstractWords, by="UniqueID")
Abstract = News$Abstract
NewsWords$Abstract = NULL

# -- Split the data again into training and test sets
NewsTrain = head(NewsWords, nrow(NewsTrain))
NewsTrain$WordCount = NULL
NewsTrain$Popular = Popular
NewsTest = tail(NewsWords, nrow(NewsTest))

# =========
# Load data (to various models)
# =========
# Check the baseline
table(NewsTrain$Popular)
(5439) / nrow(NewsTrain) # accuracy = 0.8326699 (predict all FALSE)

####  Logistic Regression ####
# Build LR model and check summary
NewsLog = glm(Popular ~. - UniqueID, data=NewsTrain, family=binomial)
summary(NewsLog)

# Check predictions on the training set
NewsTrainPred = predict(NewsLog)
table(NewsTrain$Popular, NewsTrainPred > 0.5)
accuracy = (5290 + 636) / nrow(NewsTrain)
accuracy

# Make predictions on the test set
NewsTestPred = predict(NewsLog, newdata=NewsTest, type="response")

# Make submission (LOG)
submission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = NewsTestPred)
write.csv(submission, "MySubmissionLog3.csv", row.names=FALSE)

###### CART model #####
# Try a CART model
library(rpart)
library(rpart.plot)
NewsTrain$UniqueID = NULL
NewsCART = rpart(Popular ~ ., data=NewsTrain)
prp(NewsCART)

# Get accuracy of training set
NewsTrainPred = predict(NewsCART)
table(NewsTrain$Popular, NewsTrainPred[,2] > 0.5)
(5288 + 621) / nrow(NewsTrain) # 0.9046234

###### Random Forest ######
# Now try a RF model
library(randomForest)
set.seed(69)
NewsRF = randomForest(Popular ~ ., data=NewsTrain, ntree=500, nodesize=20)

# Get accuracy of RF on trainng set
NewsTrainPred = predict(NewsRF)
table(NewsTrain$Popular, NewsTrainPred)
accuracy = (5242 + 728) / nrow(NewsTrain)
accuracy

# Make a submission 
NewsTestPred = predict(NewsRF, newdata=NewsTest)
submission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = NewsTestPred)
write.csv(submission, "MySubmissionRF3.csv", row.names=FALSE) #0.82473
# Probably overfit the training set

#### Clustering ####
spl = split(NewsTrain, NewsTrain$NewsDesk)
str(spl)
lapply(spl, nrow)
