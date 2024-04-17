remove(list = ls())
####LOGISTIC######

library(pROC)
library(car)
library(leaps)
library(corrplot)

load("dati.RData")

#featuring selection
#creating a subset:
#dati_fit<-subset(dati, select = c("funded_amnt","term","int_rate","home_ownership",
 #                                 "annual_inc","purpose","dti","fico_range_low","open_acc","revol_util","out_prncp",
  #                                "total_pymnt","total_rec_int","application_type","tot_coll_amt", "tot_cur_bal",
   #                               "open_il_24m","avg_cur_bal","bc_util","chargeoff_within_12_mths","delinq_amnt","mort_acc",
    #                              "num_rev_accts","num_tl_90g_dpd_24m","pct_tl_nvr_dlq",
     #                             "pub_rec_bankruptcies","tax_liens","tot_hi_cred_lim","total_bc_limit","default"))

#dati_fit<-subset(dati, select = c("funded_amnt","term","int_rate",
   #                               "annual_inc","fico_range_low","open_acc","revol_util","out_prncp",
     #                             "total_pymnt","total_rec_int", "tot_cur_bal",
       #                           "open_il_24m","avg_cur_bal","bc_util","chargeoff_within_12_mths","mort_acc","default"))
#deleted for elena and jack --> "dti" ,"application_type","tot_coll_amt","pct_tl_nvr_dlq","pub_rec_bankruptcies","tax_liens",,"total_bc_limit"
#"home_ownership","tot_hi_cred_lim","num_tl_90g_dpd_24m"

#i try to eliminate non-significative variables in the logit model with 17 variables
#i deleted from giulio's model: annual inc, open_il_24m, bc_util, chargeoff_within_12_mths, mort_acc,"avg_cur_bal", "tot_cur_bal","open_acc"
#here i got 7 variables
dati_fit<-subset(dati, select = c("funded_amnt","int_rate",
                                  "fico_range_low","revol_util","out_prncp",
                                  "total_pymnt","total_rec_int","default"))
cor<-cor(dati_fit[, sapply(dati_fit, is.numeric)])
corrplot(cor(dati_fit[, sapply(dati_fit, is.numeric)]), method = "circle", type = "lower", tl.col = "black", tl.srt = 45, diag = FALSE)

#this data are used to create train test and validation with 100k obs

set.seed(1)
#i want to select a sub sample of the data of 300k observation
dati_fit<-dati_fit[sample(1:nrow(dati_fit),300000),]
# now i want to split data in train, test and validation, each set will have 100k observation.
train<-sample(1:nrow(dati_fit),(1/3)*nrow(dati_fit))
test<-sample(setdiff(1:nrow(dati_fit),train),(1/3)*nrow(dati_fit))
validation<-setdiff(setdiff(1:nrow(dati_fit),train),test)


# calculat the logistic regression model
fit<-glm(default~.,data=dati_fit[train,],family="binomial")
vif(fit)
summary(fit)
adjR2(fit)

#####THRESHOOLD SELECTION######

#######method 1: using the best threshold from the ROC curve########

# calculate the probability of the loan to be in default on train data
#pred<-predict(fit,newdata=dati_fit[train,],type="response")
# calculate the ROC curve
#trainRoc<-roc(dati_fit[train,]$default ~ pred, plot=T, print.auc=T)
# calculate the best treeshold
#threshold<-coords(trainRoc, x="best", ret="threshold")
#now predict on test data
#pred2<-predict(fit,newdata=dati_fit[test,],type="response")
#set the treeshold
#pred2<-ifelse(pred2>threshold$threshold,1,0)
# create confusion matrix
#table(pred2,dati_fit[test,]$default)
#evaluate accuracy
#sum(diag(table(pred2,dati_fit[test,]$default)))/sum(table(pred2,dati_fit[test,]$default))
#table(pred2, dati_fit[test, ]$default)[2,2] / sum(table(pred2, dati_fit[test, ]$default)[,2])



#######method 2: using the best threshold from the accuracy########

#i want to try to find the best threshold using accuracy
#threshold<-seq(0.01,0.99,0.01)
#accuracy<-rep(0,length(threshold))
#for(i in 1:length(threshold)){
#  pred_thre<-predict(fit,newdata=dati_fit[validation,],type="response")
#  pred2<-ifelse(pred_thre>threshold[i],1,0)
#  accuracy[i]<-sum(diag(table(pred2,dati_fit[validation,]$default)))/sum(table(pred2,dati_fit[validation,]$default))
#}
#plot(threshold,accuracy,type="l")
#max(accuracy)
#threshold[which(accuracy==max(accuracy))]



########method 3: using the best threshold from the sensitivity########

#i want to find a threshold but using sensitivity
#threshold<-seq(0.01,0.99,0.01)
#sensitivity<-rep(0,length(threshold))
#for (i in seq_along(threshold)) {
#  pred_thre <- predict(fit, newdata = dati_fit[validation, ], type = "response")
#  pred2 <- ifelse(pred_thre > threshold[i], 1, 0)
#  sensitivity[i] <- table(pred2, dati_fit[validation, ]$default)[2,2] / sum(table(pred2, dati_fit[validation, ]$default)[,2])
#}
#plot(threshold,sensitivity,type="l")
#max(sensitivity)
#threshold[which(sensitivity==max(sensitivity))]
#table(pred2,dati_fit[validation,]$default)


########method 4: using the best threshold from the f1 score########
{
#i want to find the threshold that optimize f1 score
threshold<-seq(0.01,0.30,0.01)
f1<-rep(0,length(threshold))
sensitivity<-rep(0,length(threshold))
precision<-rep(0,length(threshold))
for (i in seq_along(threshold)) {
  pred_thre <- predict(fit, newdata = dati_fit[validation, ], type = "response")
  pred2 <- ifelse(pred_thre > threshold[i], 1, 0)
  sensitivity[i] <- table(pred2, dati_fit[validation, ]$default)[2,2] / sum(table(pred2, dati_fit[validation, ]$default)[,2])
  precision[i]<-table(pred2,dati_fit[validation,]$default)[2,2]/sum(table(pred2,dati_fit[validation,]$default)[2,])
  f1[i]<-2*(precision[i]*sensitivity[i])/(precision[i]+sensitivity[i])
}
max(f1)
threshold[which(f1==max(f1))]
}


#MODEL VALIDATION ON TEST DATA

pred2<-predict(fit,newdata=dati_fit[test,],type="response")
#set the treeshold
pred2<-ifelse(pred2>0.12,1,0)
# create confusion matrix
table(pred2,dati_fit[test,]$default)
#evaluate accuracy
accuracy<-sum(diag(table(pred2,dati_fit[test,]$default)))/sum(table(pred2,dati_fit[test,]$default))
sensitivity<-table(pred2, dati_fit[test, ]$default)[2,2] / sum(table(pred2, dati_fit[test, ]$default)[,2])
#i want to calculate the roc curve
testRoc<-roc(dati_fit[test,]$default ~ pred2, plot=T, print.auc=T)
# i want to calculate specificity and sensitivity
testRoc$specificities
testRoc$sensitivities
