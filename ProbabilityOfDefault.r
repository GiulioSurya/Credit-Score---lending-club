
####LOGISTIC######
library(caret)
library(pROC)
library(car)
library(leaps)

# #this data are used to create traoin test and validation with 100k obs
# save("dati",file ="dati_clean.RData" )
# load("dati_clean.RData")

#i want to select a sub sample of the data of 300k observation
dati_fit<-dati[sample(1:nrow(dati),300000),]
# now i want to split data in train, test and validation, each set will have 100k observation.
train<-sample(1:nrow(dati_fit),(1/3)*nrow(dati_fit))
test<-sample(setdiff(1:nrow(dati_fit),train),(1/3)*nrow(dati_fit))
validation<-setdiff(setdiff(1:nrow(dati_fit),train),test)

#i want to do stepwise forward seleciton using leaps
features<-regsubsets(default~.,data=dati_fit,nvmax=11,method="forward")
summary(features)
# calculat the logistic regression model
fit<-glm(default~.,data=dati_fit[train,],family="binomial")
vif(fit)
# calculate the probability of the loan to be in default on train data
pred<-predict(fit,newdata=dati_fit[train,],type="response")
# calculate the ROC curve
trainRoc<-roc(dati_fit[train,]$default ~ pred, plot=T, print.auc=T)
# calculate the best treeshold
threshold<-coords(trainRoc, x="best", ret="threshold")
#now predict on test data
pred2<-predict(fit,newdata=dati_fit[test,],type="response")
#set the treeshold
pred2<-ifelse(pred2>threshold$threshold,1,0)
# create confusion matrix
table(pred2,dati_fit[test,]$default)
#evaluate accuracy
sum(diag(table(pred2,dati_fit[test,]$default)))/sum(table(pred2,dati_fit[test,]$default))



#i want to try to find the best threshold using accuracy
threshold<-seq(0.01,0.99,0.01)
accuracy<-rep(0,length(threshold))
for(i in 1:length(threshold)){
  pred_thre<-predict(fit,newdata=dati_fit[validation,],type="response")
  pred2<-ifelse(pred_thre>threshold[i],1,0)
  accuracy[i]<-sum(diag(table(pred2,dati_fit[validation,]$default)))/sum(table(pred2,dati_fit[validation,]$default))
}
plot(threshold,accuracy,type="l")
max(accuracy)
threshold[which(accuracy==max(accuracy))]


#now i try to calculate the accuracy on test data using this threshold
pred2<-predict(fit,newdata=dati_fit[test,],type="response")
#set the treeshold
pred2<-ifelse(pred2>0.20,1,0)
# create confusion matrix
table(pred2,dati_fit[test,]$default)
#evaluate accuracy
sum(diag(table(pred2,dati_fit[test,]$default)))/sum(table(pred2,dati_fit[test,]$default))
#i want to calculate the roc curve
testRoc<-roc(dati_fit[test,]$default ~ pred2, plot=T, print.auc=T)
# i want to calculate specificity and sensitivity
testRoc$specificities
testRoc$sensitivities