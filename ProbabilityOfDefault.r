remove(list = ls())
####LOGISTIC######
library(caret)
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

dati_fit<-subset(dati, select = c("funded_amnt","term","int_rate","home_ownership",
                                  "annual_inc","dti","fico_range_low","open_acc","revol_util","out_prncp",
                                  "total_pymnt","total_rec_int","application_type","tot_coll_amt", "tot_cur_bal",
                                  "open_il_24m","avg_cur_bal","bc_util","chargeoff_within_12_mths","mort_acc",
                                  "num_tl_90g_dpd_24m","pct_tl_nvr_dlq",
                                  "pub_rec_bankruptcies","tax_liens","tot_hi_cred_lim","total_bc_limit","default"))
cor<-cor(dati_fit[, sapply(dati_fit, is.numeric)])
corrplot(cor(dati_fit[, sapply(dati_fit, is.numeric)]), method = "circle", type = "lower", tl.col = "black", tl.srt = 45, diag = FALSE)




# #this data are used to create traoin test and validation with 100k obs

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

#i want to find a threshold but using sensitivity
threshold<-seq(0.01,0.99,0.01)
sensitivity<-rep(0,length(threshold))
for (i in seq_along(threshold)) {
  pred_thre <- predict(fit, newdata = dati_fit[validation, ], type = "response")
  pred2 <- ifelse(pred_thre > threshold[i], 1, 0)
  sensitivity[i] <- table(pred2, dati_fit[validation, ]$default)[2,2] / sum(table(pred2, dati_fit[validation, ]$default)[,2])
}
plot(threshold,sensitivity,type="l")
max(sensitivity)
threshold[which(sensitivity==max(sensitivity))]

table(pred2,dati_fit[validation,]$default)



#now i try to calculate the accuracy on test data using this threshold
pred2<-predict(fit,newdata=dati_fit[test,],type="response")
#set the treeshold
pred2<-ifelse(pred2>0.1,1,0)
# create confusion matrix
table(pred2,dati_fit[test,]$default)
#evaluate accuracy
sum(diag(table(pred2,dati_fit[test,]$default)))/sum(table(pred2,dati_fit[test,]$default))
table(pred2, dati_fit[test, ]$default)[2,2] / sum(table(pred2, dati_fit[test, ]$default)[,2])
#i want to calculate the roc curve
testRoc<-roc(dati_fit[test,]$default ~ pred2, plot=T, print.auc=T)
# i want to calculate specificity and sensitivity
testRoc$specificities
testRoc$sensitivities
