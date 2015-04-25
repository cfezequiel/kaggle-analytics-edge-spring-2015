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

# Check training predictions
verify = function(model, y, threshold=0.5, type="response") {
  pred = predict(model, type=type)
  if (type == "prob") {
    pred = pred[,2] 
  }
  if (is.factor(pred)) {
    t = table(y, pred)
  }
  else {
    t = table(y, pred > threshold)
  }
  acc = accuracy(t)
  auc = aucroc(pred, y)
  return(list("accuracy"=acc, "auc"=auc))
}
