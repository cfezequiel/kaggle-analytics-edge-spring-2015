# Combine the training and test data
combine = function(train, test) {
  df = train
  ytrain = df$Popular
  df$Popular = NULL
  df = rbind(df, test)
  return(list("df"=df, "ytrain"=ytrain))
}

transform = function(df_) {
  df = df_
  
  # Change some variables to factors
  df$NewsDesk = as.factor(df$NewsDesk)
  df$SectionName = as.factor(df$SectionName)
  df$SubsectionName = as.factor(df$SubsectionName)

  # Add a variable if headline is a question or not
  df$Headline = tolower(df$Headline)
  df$HeadlineQuestion = grepl('\\?', df$Headline)
  
  # Use log of WordCount
  df$WordCountLog = log(1 + df$WordCount)
  df$WordCount = NULL
  
  # -- Change PubDate to Year Month Day
  PubDate = strptime(df$PubDate, "%Y-%m-%d %H:%M:%S")
  df$PubDate = NULL
  df$PubWeekday = PubDate$wday
  df$PubHour = PubDate$hour
  
  return(df)
}

# ============
# Bag of Words
# ============

# Apply BofW to text vector
library(tm)
corpufy = function(v, prefix, sparsity=0.99) {
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


bow = function(df_) {
  df = df_
  
  # --- Transform text variables to BofW
  HeadlineWords = corpufy(df$Headline, 'H', 0.98)
  SnippetWords = corpufy(df$Snippet, 'S', 0.96)
  AbstractWords = corpufy(df$Abstract, 'A', 0.96)
  
  # --- Combine the BoW data frames to original data frame 
  # ---- Headline
  HeadlineWords$UniqueID = df$UniqueID
  df = merge(df, HeadlineWords, by="UniqueID")
  df$Headline = NULL
  
  # ---- Snippet
  SnippetWords$UniqueID = df$UniqueID
  df = merge(df, SnippetWords, by="UniqueID")
  df$Snippet = NULL
  
  # ---- Abstact
  AbstractWords$UniqueID = df$UniqueID
  df = merge(df, AbstractWords, by="UniqueID")
  df$Abstract = NULL
  
  return(df)
}

splitTrainTest = function(df, y, yname='y') {
  train = head(df, length(y))
  train[, yname] = y
  test = tail(df, nrow(df) - length(y))
  return(list("train"=train, "test"=test))
}
