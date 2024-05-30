remove(list = ls())

{
library(pROC)
library(car)
library(leaps)
library(corrplot)
library(glmtoolbox)
library(DescTools)
library(ResourceSelection)
}

load("dati.RData")

#FEATURING SELECTION
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

#deleted "dti" ,"application_type","tot_coll_amt","pct_tl_nvr_dlq","pub_rec_bankruptcies","tax_liens",,"total_bc_limit"
#"home_ownership","tot_hi_cred_lim","num_tl_90g_dpd_24m"

#After some trials, we decided to keep the following variables:
#We kept 7 variables + default so 8 variables in total
dati_fit<-subset(dati, select = c("funded_amnt","int_rate",
                                  "fico_range_low","revol_util","out_prncp",
                                  "total_pymnt","total_rec_int","default"))

#We computed the correlation matrix
cor<-cor(dati_fit[, sapply(dati_fit, is.numeric)])
corrplot(cor(dati_fit[, sapply(dati_fit, is.numeric)]), method = "circle", type = "lower", tl.col = "black", tl.srt = 45, diag = FALSE)


set.seed(1)
#######TRAIN, VALIDATION AND TEST FOR DEFAULT
#Now we want to split data in train, test and validation: each set will have 1/3 of the total observations.
train<-sample(1:nrow(dati_fit),(1/3)*nrow(dati_fit))
test<-sample(setdiff(1:nrow(dati_fit),train),(1/3)*nrow(dati_fit))
validation<-setdiff(setdiff(1:nrow(dati_fit),train),test)

#MODEL FITTING: choose the model you want to fit (linear, probit or logit)
#####LINEAR MODEL#########
fit<-lm(default~.,data=dati_fit[train,])

######PROBIT########
fit<-glm(default~.,data=dati_fit[train,],family=binomial(link="probit"))

####LOGISTIC######
fit<-glm(default~.,data=dati_fit[train,],family=binomial(link="logit"))


######SUMMARY OF THE MODEL######
vif(fit)
# We notice some high VIF values, but we can ignore them because we are interested in a prediction, we are not doing inference.
summary(fit)
PseudoR2(fit, which = "McFadden") #only for Logit and Probit

#####THRESHOLD SELECTION######

#######Method 1: using the best threshold from the ROC curve, maximizing the sum of sensitivity and specificity########
#Predict on validation data
#pred<-predict(fit,newdata=dati_fit[validation,],type="response")
#Compute ROC curve and see AUC
#validationRoc<-roc(dati_fit[validation,]$default ~ pred, plot=T, print.auc=T)
#Calculate the best threshold
#threshold<-coords(validationRoc, x="best", ret="threshold")
#Prediction on test data
#pred2<-predict(fit,newdata=dati_fit[test,],type="response")
#set the threshold
#pred2<-ifelse(pred2>threshold$threshold,1,0)
#create confusion matrix
#table(pred2,dati_fit[test,]$default)
#Compute accuracy
#sum(diag(table(pred2,dati_fit[test,]$default)))/sum(table(pred2,dati_fit[test,]$default))

#From now on, we choose the best threshold between 0.01 and 0.40, to ease the computation.
#######Method 2: using the best threshold maximizing accuracy########
#threshold<-seq(0.01,0.40,0.01)
#accuracy<-rep(0,length(threshold))
#pred_thre<-predict(fit,newdata=dati_fit[validation,],type="response")
#for(i in 1:length(threshold)){
#  pred2<-ifelse(pred_thre>threshold[i],1,0)
#  accuracy[i]<-sum(diag(table(pred2,dati_fit[validation,]$default)))/sum(table(pred2,dati_fit[validation,]$default))
#}
#plot(threshold,accuracy,type="l")
#max(accuracy)
#threshold[which(accuracy==max(accuracy))]

########Method 3: using the best threshold maximizing sensitivity########
#threshold<-seq(0.01,0.40,0.01)
#sensitivity<-rep(0,length(threshold))
#pred_thre <- predict(fit, newdata = dati_fit[validation, ], type = "response")
#for (i in seq_along(threshold)) {
#  pred2 <- ifelse(pred_thre > threshold[i], 1, 0)
#  sensitivity[i] <- table(pred2, dati_fit[validation, ]$default)[2,2] / sum(table(pred2, dati_fit[validation, ]$default)[,2])
#}
#plot(threshold,sensitivity,type="l")
#max(sensitivity)
#threshold[which(sensitivity==max(sensitivity))]
#table(pred2,dati_fit[validation,]$default)


########Method 4: using the best threshold maximizing F1 score########
{
threshold<-seq(0.01,0.40,0.01)
f1<-rep(0,length(threshold))
sensitivity<-rep(0,length(threshold))
precision<-rep(0,length(threshold))
pred_thre <- predict(fit, newdata = dati_fit[validation, ], type = "response")
for (i in seq_along(threshold)) {
  pred2 <- ifelse(pred_thre > threshold[i], 1, 0)
  sensitivity[i] <- table(pred2, dati_fit[validation, ]$default)[2,2] / sum(table(pred2, dati_fit[validation, ]$default)[,2])
  precision[i]<-table(pred2,dati_fit[validation,]$default)[2,2]/sum(table(pred2,dati_fit[validation,]$default)[2,])
  f1[i]<-2*(precision[i]*sensitivity[i])/(precision[i]+sensitivity[i])
}
max(f1)
best_thres <- threshold[which(f1==max(f1))]
}

#We choose method 4, because we want to maximize the F1 score, which is a good measure of the model's performance.


#MODEL VALIDATION ON TEST DATA
pred2<-predict(fit,newdata=dati_fit[test,],type="response")
#Set the threshold
pred2<-ifelse(pred2>best_thres,1,0)
#Create confusion matrix
table(pred2,dati_fit[test,]$default)
#Evaluate accuracy
accuracy<-sum(diag(table(pred2,dati_fit[test,]$default)))/sum(table(pred2,dati_fit[test,]$default))
#Compute ROC curve
testRoc<-roc(dati_fit[test,]$default ~ pred2, plot=T, print.auc=T)
#calculate specificity and sensitivity
testRoc$specificities
testRoc$sensitivities
accuracy


#We compute Brier Score for the three models in order to compare them.
#Brier Score is calculated as the mean squared difference between predicted probabilities and observed binary outcomes.
#Brier Score ranges from 0 to 1, with lower values indicating better predictions.
{
  brier_score <- function(pred, obs) {
    brier_score <- mean((pred - obs)^2)
    return(brier_score)
  }
  #Logit
  fit_logit <- glm(default ~ ., data = dati_fit[train, ], family = binomial(link = "logit"))
  pred_logit <- predict(fit_logit, newdata = dati_fit[test, ], type = "response")
  brier_score_logit <- brier_score(pred_logit, dati_fit[test, ]$default)
  #Probit
  fit_probit <- glm(default ~ ., data = dati_fit[train, ], family = binomial(link = "probit"))
  pred_probit <- predict(fit_probit, newdata = dati_fit[test, ], type = "response")
  brier_score_probit <- brier_score(pred_probit, dati_fit[test, ]$default)
  #Linear
  fit_linear <- lm(default ~ ., data = dati_fit[train, ])
  pred_linear <- predict(fit_linear, newdata = dati_fit[test, ])
  brier_score_linear <- brier_score(pred_linear, dati_fit[test, ]$default)
  #Results
  brier_score_logit
  brier_score_probit
  brier_score_linear
}
which.min (c(brier_score_logit, brier_score_probit, brier_score_linear))
#The best model is the Logit, because it has the lowest Brier Score.





#Variable importance plot in the model chosen with shuffling method 
#We shuffle the values of each variable and compute the accuracy of the model on the test set.
{
  accuracy <- rep(0, ncol(dati_fit) - 1)
  for (i in seq_len(ncol(dati_fit) - 1)) {
    for (j in seq_len(100)) {
      dati_fit_shuffle <- dati_fit
      dati_fit_shuffle[, i] <- sample(dati_fit_shuffle[, i])
      pred2 <- predict(fit, newdata = dati_fit_shuffle[test, ], type = "response")
      pred2 <- ifelse(pred2 > best_thres, 1, 0)
      accuracy[i] <- accuracy[i] + sum(diag(table(pred2, dati_fit_shuffle[test, ]$default))) / sum(table(pred2, dati_fit_shuffle[test, ]$default))
    }
  }
  accuracy <- accuracy / 100
  barplot(accuracy, names.arg = colnames(dati_fit)[-ncol(dati_fit)], las = 2)
}
# We can see from the variable importance plot that the most important variables in discriminating between default and non-default loans are
# int_rate, fico_range_low and revol_util.

