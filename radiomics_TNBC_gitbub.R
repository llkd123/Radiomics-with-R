setwd('your work directory')
library(glmnet)
library(rms)
library(pROC)
library(caret)
library(openxlsx)
library(psych)
library(MASS)
library(ResourceSelection)
library(dplyr)
library(devtools)
library(rmda)
library(PredictABEL)
library(ggplot2)
source('directory of model_evaluation.R')
library(readr)
library(caret)
library(table1)
library(lubridate)
library(dca)
library(car)
blank<-read.xlsx('blank.xlsx')#a excel without any data
#data
train_cc<-read.csv('directory of data of training set of CC')
test_cc<-read.csv('directory of data of testing set of CC')
train_mlo<-read.csv('directory of data of training set of MLO')
test_mlo<-read.csv('directory of data of testing set of MLO')
train_com<-read.csv('directory of data of traing set of Combined')
test_com<-read.csv('directory of data of test set of Combined')

#CC
y_train <- as.factor(train_cc[,1])#first column of data is target
y_test <- as.factor(test_cc[,1])#first column of data is target
x_train<- as.matrix(data.frame(train_cc[,-1]))#others are features
x_test<- as.matrix(data.frame(test_cc[,-1]))#others are features
col_names<-colnames(x_train)
#cancel fearures with same data
range<-blank
for(i in col_names){
  rangei<- max(x_train[,i])-min(x_train[,i])
  if(rangei==0){
    x_train<-x_train[,colnames(x_train) != i]
    x_test<-x_test[,colnames(x_test) != i]
  }
}
col_names<-colnames(x_train)
#scale
for(i in col_names){
  x_train[,i]<-(x_train[,i]-min(x_train[,i]))/(max(x_train[,i])-min(x_train[,i]))
  x_test[,i]<-(x_test[,i]-min(x_train[,i]))/(max(x_train[,i])-min(x_train[,i]))
}
col_names<-colnames(x_train)
#Univariate analysis
data_levene<-data.frame(cbind(y_train,x_train))
data_levene$y_train<-as.character(data_levene$y_train)
for (i in col_names) {
  p_1 <- shapiro.test(x_train[,i])$p.value
  p_2<-leveneTest(data_levene[,i]~y_train,data=data_levene)$`Pr(>F)`[1]
  if(p_1>0.05 && p_2>0.05){
    pvalue<-summary(aov(data_levene[,i]~y_train,data=data_levene))[[1]][,"Pr(>F)"][1]
    if(pvalue>0.05){
      x_train<-x_train[,colnames(x_train) != i]
      x_test<-x_test[,colnames(x_test) != i]
    }
    }else{pvalue <- wilcox.test(as.numeric(x_train[,i])~y_train)$p.value
  if(pvalue>0.05){
    x_train<-x_train[,colnames(x_train) != i]
    x_test<-x_test[,colnames(x_test) != i]
  }
  }
}
para1_cc<-dim(x_train)[2]
#lasso
fit<-glmnet(x_train,y_train,alpha=1,family='binomial')
cv.fit <- cv.glmnet(x_train,y_train,family = "binomial",alpha=1)
para2_cc<-cv.fit$lambda.1se
Coefficients <- coef(fit, s = cv.fit$lambda.1se)
Active.Index <- which(Coefficients != 0)
para3_cc <- Coefficients[Active.Index]
para4_cc<- row.names(Coefficients)[Active.Index]
p_test<-predict(fit,type="response",newx=x_test,newy=y_test,s=cv.fit$lambda.1se)
p_train<-predict(fit,type="response",newx=x_train,newy=y_train,s=cv.fit$lambda.1se)
RS_test<-predict(fit,type="link",newx=x_test,newy=y_test,s=cv.fit$lambda.1se)
RS_train<-predict(fit,type="link",newx=x_train,newy=y_train,s=cv.fit$lambda.1se)
train_cc$p<-p_train
train_cc$RS<-RS_train
test_cc$p<-p_test
test_cc$RS<-RS_test
#evaluation
roc_test_cc<-roc(test_cc$TNBC~test_cc$p)
roc_train_cc<-roc(train_cc$TNBC~train_cc$p)
#plot(roc_test)
auc(roc_test_cc)
auc(roc_train_cc)
#评价_cc
thres_train<-coords(roc_train_cc,"b",best.method = "youden")
class_train<-ifelse(p_train>thres_train$threshold,1,0)
class_test<-ifelse(p_test>thres_train$threshold,1,0)
train_cc$class<-class_train
test_cc$class<-class_test
eva_test_cc<-model_evaluation(test_cc$p,test_cc$class,test_cc$TNBC)
eva_train_cc<-model_evaluation(train_cc$p,train_cc$class,train_cc$TNBC)
thres_cc<-thres_train

#MLO
y_train <- as.factor(train_mlo[,1])
y_test <- as.factor(test_mlo[,1])
x_train<- as.matrix(data.frame(train_mlo[,-1]))
x_test<- as.matrix(data.frame(test_mlo[,-1]))
col_names<-colnames(x_train)
#cancel fearures with same data
range<-blank
for(i in col_names){
  rangei<- max(x_train[,i])-min(x_train[,i]);
  if(rangei==0){
    x_train<-x_train[,colnames(x_train) != i]
    x_test<-x_test[,colnames(x_test) != i]
  }
}
col_names<-colnames(x_train)
#scale
for(i in col_names){
  x_train[,i]<-(x_train[,i]-min(x_train[,i]))/(max(x_train[,i])-min(x_train[,i]))
  x_test[,i]<-(x_test[,i]-min(x_train[,i]))/(max(x_train[,i])-min(x_train[,i]))
}
col_names<-colnames(x_train)
#
data_levene<-data.frame(cbind(y_train,x_train))
data_levene$y_train<-as.character(data_levene$y_train)
for (i in col_names) {
  p_1 <- shapiro.test(x_train[,i])$p.value
  p_2<-leveneTest(data_levene[,i]~y_train,data=data_levene)$`Pr(>F)`[1]
  if(p_1>0.05 && p_2>0.05){
    pvalue<-summary(aov(data_levene[,i]~y_train,data=data_levene))[[1]][,"Pr(>F)"][1]
    if(pvalue>0.05){
      x_train<-x_train[,colnames(x_train) != i]
      x_test<-x_test[,colnames(x_test) != i]
    }
  }else{pvalue <- wilcox.test(as.numeric(x_train[,i])~y_train)$p.value
  if(pvalue>0.05){
    x_train<-x_train[,colnames(x_train) != i]
    x_test<-x_test[,colnames(x_test) != i]
  }
  }
}
para1_mlo<-dim(x_train)[2]
#lasso
fit<-glmnet(x_train,y_train,alpha=1,family='binomial')

cv.fit <- cv.glmnet(x_train,y_train,family = "binomial",alpha=1)

para2_mlo<-cv.fit$lambda.1se
Coefficients <- coef(fit, s = cv.fit$lambda.1se)
Active.Index <- which(Coefficients != 0)
para3_mlo <- Coefficients[Active.Index]
para4_mlo<- row.names(Coefficients)[Active.Index]
p_test<-predict(fit,type="response",newx=x_test,newy=y_test,s=cv.fit$lambda.1se)
p_train<-predict(fit,type="response",newx=x_train,newy=y_train,s=cv.fit$lambda.1se)
RS_test<-predict(fit,type="link",newx=x_test,newy=y_test,s=cv.fit$lambda.1se)
RS_train<-predict(fit,type="link",newx=x_train,newy=y_train,s=cv.fit$lambda.1se)
train_mlo$p<-p_train
train_mlo$RS<-RS_train
test_mlo$p<-p_test
test_mlo$RS<-RS_test
#evaluation
roc_test_mlo<-roc(test_mlo$TNBC~test_mlo$p)
roc_train_mlo<-roc(train_mlo$TNBC~train_mlo$p)
#plot(roc_test)
auc(roc_test_mlo)
auc(roc_train_mlo)
#评价_mlo
thres_train<-coords(roc_train_mlo,"b",best.method = "youden")
class_train<-ifelse(p_train>thres_train$threshold,1,0)
class_test<-ifelse(p_test>thres_train$threshold,1,0)
train_mlo$class<-class_train
test_mlo$class<-class_test
eva_test_mlo<-model_evaluation(test_mlo$p,test_mlo$class,test_com$TNBC)
eva_train_mlo<-model_evaluation(train_mlo$p,train_mlo$class,train_mlo$TNBC)
thres_mlo<-thres_train




##COM
y_train <- as.factor(train_com[,1])
y_test <- as.factor(test_com[,1])
x_train<- as.matrix(data.frame(train_com[,-1]))
x_test<- as.matrix(data.frame(test_com[,-1]))
col_names<-colnames(x_train)
#cancel fearures with same data
range<-blank
for(i in col_names){
  rangei<- max(x_train[,i])-min(x_train[,i]);
  if(rangei==0){
    x_train<-x_train[,colnames(x_train) != i]
    x_test<-x_test[,colnames(x_test) != i]
  }
}
col_names<-colnames(x_train)
#scale
for(i in col_names){
  x_train[,i]<-(x_train[,i]-min(x_train[,i]))/(max(x_train[,i])-min(x_train[,i]))
  x_test[,i]<-(x_test[,i]-min(x_train[,i]))/(max(x_train[,i])-min(x_train[,i]))
}
col_names<-colnames(x_train)
#
data_levene<-data.frame(cbind(y_train,x_train))
data_levene$y_train<-as.character(data_levene$y_train)
for (i in col_names) {
  p_1 <- shapiro.test(x_train[,i])$p.value
  p_2<-leveneTest(data_levene[,i]~y_train,data=data_levene)$`Pr(>F)`[1]
  if(p_1>0.05 && p_2>0.05){
    pvalue<-summary(aov(data_levene[,i]~y_train,data=data_levene))[[1]][,"Pr(>F)"][1]
    if(pvalue>0.05){
      x_train<-x_train[,colnames(x_train) != i]
      x_test<-x_test[,colnames(x_test) != i]
    }
  }else{pvalue <- wilcox.test(as.numeric(x_train[,i])~y_train)$p.value
  if(pvalue>0.05){
    x_train<-x_train[,colnames(x_train) != i]
    x_test<-x_test[,colnames(x_test) != i]
  }
  }
}
para1_com<-dim(x_train)[2]
#lasso
fit<-glmnet(x_train,y_train,alpha=1,family='binomial')

cv.fit <- cv.glmnet(x_train,y_train,family = "binomial",alpha=1)

para2_com<-cv.fit$lambda.1se
Coefficients <- coef(fit, s = cv.fit$lambda.1se)
Active.Index <- which(Coefficients != 0)
para3_com <- Coefficients[Active.Index]
para4_com<- row.names(Coefficients)[Active.Index]
p_test<-predict(fit,type="response",newx=x_test,newy=y_test,s=cv.fit$lambda.1se)
p_train<-predict(fit,type="response",newx=x_train,newy=y_train,s=cv.fit$lambda.1se)
RS_test<-predict(fit,type="link",newx=x_test,newy=y_test,s=cv.fit$lambda.1se)
RS_train<-predict(fit,type="link",newx=x_train,newy=y_train,s=cv.fit$lambda.1se)

train_com$p<-p_train
train_com$RS<-RS_train
test_com$p<-p_test
test_com$RS<-RS_test
#evaluation
roc_test_com<-roc(test_com$TNBC~test_com$p)
roc_train_com<-roc(train_com$TNBC~train_com$p)
#plot(roc_test)
auc(roc_test_com)
auc(roc_train_com)
#评价_com
thres_train<-coords(roc_train_com,"b",best.method = "youden")
class_train<-ifelse(p_train>thres_train$threshold,1,0)
class_test<-ifelse(p_test>thres_train$threshold,1,0)
train_com$class<-class_train
test_com$class<-class_test
eva_test_com<-model_evaluation(test_com$p,test_com$class,test_com$TNBC)
eva_train_com<-model_evaluation(train_com$p,train_com$class,train_com$TNBC)
thres_com<-thres_train
#eva
model_evaluation(test_cc$p,test_cc$class,test_cc$TNBC)
model_evaluation(train_cc$p,train_cc$class,train_cc$TNBC)
model_evaluation(test_mlo$p,test_mlo$class,test_com$TNBC)
model_evaluation(train_mlo$p,train_mlo$class,train_mlo$TNBC)
model_evaluation(test_com$p,test_com$class,test_com$TNBC)
model_evaluation(train_com$p,train_com$class,train_com$TNBC)




#print
roc.test(roc_test_mlo,roc_test_cc)
roc.test(roc_test_mlo,roc_test_com)
roc.test(roc_test_cc,roc_test_com)
#para
para1_cc
para1_mlo
para1_com
para2_cc
para2_mlo
para2_com
para3_cc
para3_mlo
para3_com
para4_cc
para4_mlo
para4_com
#ROC
#CC
pdf(file="ROC_CC.pdf",width = 4,height = 4)
plot(roc_train_cc,col='red',legacy.axes=TRUE)
par(new=TRUE)
plot(roc_test_mlo,col='blue',legacy.axes=TRUE)
legend(0.6,0.15, c(sprintf('training AUC: %.2f',auc(roc_train_cc)), sprintf('testing AUC: %.2f',auc(roc_test_cc))), lty = c(1,1),lwd=c(2,2),col=c("red","blue"),cex = 0.8,bty = "n")
dev.off()
#MLO
pdf(file="ROC_MLO.pdf",width = 4,height = 4)
plot(roc_train_mlo,col='red',legacy.axes=TRUE)
par(new=TRUE)
plot(roc_test_mlo,col='blue',legacy.axes=TRUE)
legend(0.6,0.15, c(sprintf('training AUC: %.2f',auc(roc_train_mlo)), sprintf('testing AUC: %.2f',auc(roc_test_mlo))), lty = c(1,1),lwd=c(2,2),col=c("red","blue"),cex = 0.8,bty = "n")
dev.off()
#COM
pdf(file="ROC_COM.pdf",width = 4,height = 4)
plot(roc_train_com,col='red',legacy.axes=TRUE)
par(new=TRUE)
plot(roc_test_com,col='blue',legacy.axes=TRUE)
legend(0.6,0.15, c(sprintf('training AUC: %.2f',auc(roc_train_com)), sprintf('testing AUC: %.2f',auc(roc_test_com))), lty = c(1,1),lwd=c(2,2),col=c("red","blue"),cex = 0.8,bty = "n")
dev.off()
#DCA
#CC
data_dca<-data.frame(cbind(as.numeric(y_test)-1,test_cc$p,test_mlo$p,test_com$p))
results_dca = dca(data=data_dca, outcome="V1", predictors=c("X1","X1.1","X1.2"), probability=c("FALSE","FALSE","FALSE"))
netbenifet=results_dca$net.benefit
pdf(file="DCA_CC.pdf",width = 4,height = 4)
plot(c(1:99),as.matrix(netbenifet$X1),type = "l",lwd=c(2),xlab = "Threshold(%)",ylab ="Net Benifit",col="deepskyblue",ylim = c(0, .2))
par(new=TRUE)
plot(c(1:99),as.matrix(netbenifet$all),type = "l",lwd=c(2),xlab = "Threshold(%)",ylab ="Net Benifit",col="darkorange" ,ylim = c(0, .2))
par(new=TRUE)
plot(c(1:99),as.matrix(netbenifet$none),type = "l",lwd=c(2),xlab = "Threshold(%)",ylab ="Net Benifit",col="black" ,ylim = c(0, .2))
legend(58,0.21, c("Model-CC", "Treat-all","Treat-none"), lty = c(1,1,1),lwd=c(2,2,2),col=c("deepskyblue","darkorange","black"),cex = 0.75,bty = "n")
dev.off()
netbenifet$X1-netbenifet$all#5
netbenifet$X1-netbenifet$none#67
#MLO
pdf(file="DCA_MLO.pdf",width = 4,height = 4)
plot(c(1:99),as.matrix(netbenifet$X1.1),type = "l",lwd=c(2),xlab = "Threshold(%)",ylab ="Net Benifit",col="deepskyblue",ylim = c(0, .2))
par(new=TRUE)
plot(c(1:99),as.matrix(netbenifet$all),type = "l",lwd=c(2),xlab = "Threshold(%)",ylab ="Net Benifit",col="darkorange" ,ylim = c(0, .2))
par(new=TRUE)
plot(c(1:99),as.matrix(netbenifet$none),type = "l",lwd=c(2),xlab = "Threshold(%)",ylab ="Net Benifit",col="black" ,ylim = c(0, .2))
legend(58,0.21, c("Model-MLO", "Treat-all","Treat-none"), lty = c(1,1,1),lwd=c(2,2,2),col=c("deepskyblue","darkorange","black"),cex = 0.75,bty = "n")
dev.off()
netbenifet$X1.1-netbenifet$all#3
netbenifet$X1.1-netbenifet$none#74
#com
pdf(file="DCA_COM.pdf",width = 4,height = 4)
plot(c(1:99),as.matrix(netbenifet$X1.2),type = "l",lwd=c(2),xlab = "Threshold(%)",ylab ="Net Benifit",col="deepskyblue",ylim = c(0, .2))
par(new=TRUE)
plot(c(1:99),as.matrix(netbenifet$all),type = "l",lwd=c(2),xlab = "Threshold(%)",ylab ="Net Benifit",col="darkorange" ,ylim = c(0, .2))
par(new=TRUE)
plot(c(1:99),as.matrix(netbenifet$none),type = "l",lwd=c(2),xlab = "Threshold(%)",ylab ="Net Benifit",col="black" ,ylim = c(0, .2))
legend(58,0.21, c("Model-COM", "Treat-all","Treat-none"), lty = c(1,1,1),lwd=c(2,2,2),col=c("deepskyblue","darkorange","black"),cex = 0.75,bty = "n")
dev.off()
netbenifet$X1.2-netbenifet$all#2
netbenifet$X1.2-netbenifet$none#81
#HL test
#CC
for(n in c(2:200)){
  a<- hoslem.test(test_cc$TNBC,test_cc$p,g=n);
  print(n)
  print(a$p.value)
}
fit.nomogram <- lrm(test_cc$TNBC ~ test_cc$p,x=TRUE,y=TRUE)#（true）
cal_cc<-mycalcur1(fit.nomogram,  method = "boot", B = 1000)
plot(cal_cc, xlab = "Nomogram Predicted TNBC", ylab = "Actual TNBC",title= F,riskdist=F,legend = F,xlim = c(0,1),ylim = c(0,1))
legend(0.6,0.17, c("Predicted", "Actual"), lty = c(1, 2),lwd=c(2,1),col=c("deepskyblue","black"),cex = 0.9,bty = "n")
#MLO
for(n in c(2:200)){
  a<- hoslem.test(test_mlo$TNBC,test_mlo$p,g=n);
  print(n)
  print(a$p.value)
}
fit.nomogram <- lrm(test_mlo$TNBC ~ test_mlo$p,x=TRUE,y=TRUE)#（true）
cal_mlo<-mycalcur1(fit.nomogram,  method = "boot", B = 1000)
plot(cal_mlo, xlab = "Predicted TNBC rate", ylab = "Actual TNBC rate",title= F,riskdist=F,legend = F,xlim = c(0,1),ylim = c(0,1))
legend(0.6,0.17, c("Predicted", "Actual"), lty = c(1, 2),lwd=c(2,1),col=c("deepskyblue","black"),cex = 0.9,bty = "n")
#COM
for(n in c(2:200)){
  a<- hoslem.test(test_com$TNBC,test_com$p,g=n);
  print(n)
  print(a$p.value)
}
fit.nomogram <- lrm(test_com$TNBC ~ test_com$p,x=TRUE,y=TRUE)#（true）
cal_com<-mycalcur1(fit.nomogram,  method = "boot", B = 1000)
plot(cal_com, xlab = "Nomogram Predicted TNBC", ylab = "Actual TNBC",title= F,riskdist=F,legend = F,xlim = c(0,1),ylim = c(0,1))
legend(0.6,0.17, c("Predicted", "Actual"), lty = c(1, 2),lwd=c(2,1),col=c("deepskyblue","black"),cex = 0.9,bty = "n")
