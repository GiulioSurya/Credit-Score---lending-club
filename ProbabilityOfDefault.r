remove(list = ls())

{
library(pROC)
library(car)
library(leaps)
library(corrplot)
library(glmtoolbox)
}

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


set.seed(1)
#######TRAIN AND VALIDATION BALANCED FOR DEFAULT

# i want to create a train and validation balance but with balance number of defauult==1 and default==0
#train<-c(sample(which(dati_fit$default==1),50000),sample(which(dati_fit$default==0),50000))
#validation<-c(sample(setdiff(which(dati_fit$default==1),train),50000),sample(setdiff(which(dati_fit$default==0),train),50000))
#test<-setdiff(1:nrow(dati_fit),c(train,validation))



#######TRAIN AND VALIDATION NOT BALANCED FOR DEFAULT
{
#i want to select a sub sample of the data of 300k observation
dati_fit<-dati_fit[sample(1:nrow(dati_fit),300000),]
# now i want to split data in train, test and validation, each set will have 100k observation.
train<-sample(1:nrow(dati_fit),(1/3)*nrow(dati_fit))
test<-sample(setdiff(1:nrow(dati_fit),train),(1/3)*nrow(dati_fit))
validation<-setdiff(setdiff(1:nrow(dati_fit),train),test)
}



######PROBIT########
fit<-glm(default~.,data=dati_fit[train,],family=binomial(link="probit"))

####LOGISTIC######
fit<-glm(default~.,data=dati_fit[train,],family=binomial(link="logit"))

#####LINEAR MODEL#########
fit<-lm(default~.,data=dati_fit[train,])

######SUMMARY OF THE MODEL######
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
threshold<-seq(0.01,0.40,0.01)
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
#nb: remember to set the threshoold according to the model used

pred2<-predict(fit,newdata=dati_fit[test,],type="response")
#set the treeshold
pred2<-ifelse(pred2>0.14,1,0)
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
accuracy
sensitivity

#variable importance plot with shuffling method
#change the treshold if needed 
{
  accuracy <- rep(0, ncol(dati_fit) - 1)
  for (i in seq_len(ncol(dati_fit) - 1)) {
    for (j in seq_len(100)) {
      dati_fit_shuffle <- dati_fit
      dati_fit_shuffle[, i] <- sample(dati_fit_shuffle[, i])
      pred2 <- predict(fit, newdata = dati_fit_shuffle[test, ], type = "response")
      pred2 <- ifelse(pred2 > 0.12, 1, 0)
      accuracy[i] <- accuracy[i] + sum(diag(table(pred2, dati_fit_shuffle[test, ]$default))) / sum(table(pred2, dati_fit_shuffle[test, ]$default))
    }
  }
  accuracy <- accuracy / 100
  barplot(accuracy, names.arg = colnames(dati_fit)[-ncol(dati_fit)], las = 2)
}

#I want to compute Brier Score or Squared Error for my three models: logit, probit and linear
#The function takes as input the predicted probabilities and the observed binary outcomes.
#The Brier Score is a measure of the accuracy of a probabilistic prediction.
#It is calculated as the mean squared difference between the predicted probabilities and the observed binary outcomes.
#The Brier Score ranges from 0 to 1, with lower values indicating better predictions.
#The Squared Error is the square of the Brier Score.
brier_score(dati_fit[test,]$default, pred2)


#I want to do a Hosmer-Lemeshow test on my logit model
#Computed with hoslem.test() function in "ResourceSelection" package.
#The function takes as input the predicted probabilities and the observed binary outcomes.
install.packages("ResourceSelection")
library(ResourceSelection)
hl <- hoslem.test(dati_fit[test,]$default, pred2, g=15)
hl

#i want to compute MSE
MSE<-mean((dati_fit[test,]$default-pred2)^2)
MSE
