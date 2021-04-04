model_evaluation<-function(p,class, y){
  roc<-roc(y~as.numeric(p), quiet = TRUE)
  auc<-roc$auc
  ci<-ci(roc)
  table<-table(class, y)
  tp <- table[2, 2]
  tn <- table[1, 1]
  fp <- table[2, 1]
  fn <- table[1, 2]
  accuracy<-(tp + tn)/(tp + tn + fp + fn)
  sensitivity <- tp/(tp + fn)
  specificity <- tn/(tn + fp)
  balanced_accuracy<-(sensitivity+specificity)/2
  precision<-tp/(tp+fp)
  F1_score<-2*precision*sensitivity/(precision+sensitivity)
  ppv<-tp/(tp+fp)
  npv<-tn/(tn+fn)
  print(table)
  print(sprintf('AUC: %.2f',auc))
  print(ci)
  print(sprintf('accuracy: %.2f',accuracy))
  print(sprintf('sensitivity: %.2f',sensitivity))
  print(sprintf('specificity: %.2f',specificity))
  print(sprintf('npv: %.2f',npv))
  print(sprintf('ppv: %.2f',ppv))
  print(sprintf('balanced_accuracy: %.2f',balanced_accuracy))
  print(sprintf('precision: %.2f',precision))
  print(sprintf('F1-score: %.2f',F1_score))
}