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

# -- Change some variables to factors
News$NewsDesk = as.factor(News$NewsDesk)
News$SectionName = as.factor(News$SectionName)
News$SubsectionName = as.factor(News$SubsectionName)
Popular = as.factor(NewsTrain$Popular)

# -- Extract info from Headline
News$Headline = tolower(News$Headline)

# --- Is the headline a question?
News$HeadlineQuestion = grepl('\\?', News$Headline)

# -- Use log of WordCount
News$WordCountLog = log(1 + News$WordCount)
News$WordCount = NULL

# -- Change PubDate to Year Month Day
PubDate = strptime(News$PubDate, "%Y-%m-%d %H:%M:%S")
News$PubDate = NULL
#News$PubYear = PubDate$year # It's all 2014!
#News$PubMonth = PubDate$mon
#News$PubDay = PubDate$mday
News$PubWeekday = PubDate$wday
News$PubHour = PubDate$hour

# -- Split the data  into training and test sets
NewsTrain = head(News, nrow(NewsTrain))
NewsTrain$Popular = Popular 
NewsTest = tail(NewsWords, nrow(NewsTest))

# ============
# Bag of Words
# ============
NewsWords = News

# --- Create function to apply BofW to text
library(tm)
corpufy = function(v, prefix="", sparsity=0.99) {
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
  colnames(words) = paste(prefix, colnames(words), sep=ifelse(prefix == '', '', '_')) 
  return(words)
}

# -- Combine all text variables to one
News$Text = paste(News$Headline, News$Snippet, News$Abstract)
News$Headline = NULL
News$Snippet = NULL
News$Abstract = NULL

# --- Transform text variables to BofW
TextWords = corpufy(News$Text, sparsity=0.96)


# --- Combine the BoW data frames to original data frame
NewsWords = News
TextWords$UniqueID = News$UniqueID
NewsWords = merge(NewsWords, TextWords, by="UniqueID")
NewsWords$Text = NULL

# -- Split the data  into training and test sets
NewsWordsTrain = head(NewsWords, nrow(NewsTrain))
NewsWordsTrain$Popular = Popular
NewsWordsTest = tail(NewsWords, nrow(NewsTest))