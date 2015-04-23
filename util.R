library(ROCR)

# Compute accuracy
accuracy = function(table_) {
 return(table_[[1]] / (table_[[1]] + table_[[2]]))
}

# Compute Area-Under-Curve of ROC
aucroc = function(y_pred, y_exp) {
  ROCRpred = prediction(y_pred, y_exp)
  return(as.numeric(performance(ROCRpred, "auc")@y.values))
}

# Prepare submission file
makeSubmitFile = function(x, y, filename, dir='.') {
  submission = data.frame(UniqueID = x, Probability1 = y)
  filepath = paste(dir, "/", filename, sep="")
  write.csv(submission, filepath, row.names=FALSE)
}