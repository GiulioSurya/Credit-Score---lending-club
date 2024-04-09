remove(list = ls())
{
dati<-read.csv("loan_data_2007_2014.csv", na.strings = "")

#delete all the variable not correlated (logicaly) with the target or too correlated with other variable
dati <- subset(dati, select = -c(member_id, emp_title,issue_d,pymnt_plan,url,desc,zip_code,
earliest_cr_line,mths_since_last_delinq,mths_since_last_record,annual_inc_joint,application_type,
collection_recovery_fee,recoveries,dti_joint,last_credit_pull_d,last_pymnt_d,mths_since_last_major_derog,policy_code,
revol_bal,revol_util,total_acc,open_acc_6m,
open_il_6m,open_il_12m,open_il_24m,mths_since_rcnt_il,total_bal_il,open_rv_12m,open_rv_24m,max_bal_bc,
all_util,total_rev_hi_lim,inq_fi,total_cu_tl,inq_last_12m,acc_now_delinq,tot_coll_amt,tot_cur_bal,X,next_pymnt_d))

#delete all the variable with more than 50% of NA
dati<-dati[,colMeans(is.na(dati))<0.5]
View(head(dati))

library("corrplot")
#correlation matrix
dati_cor<-dati[,sapply(dati,is.numeric)]
M <- cor(dati_cor,use="complete.obs")
corrplot(M, type="upper", order="hclust",method="number",number.cex=0.5,addCoef.col = "black",tl.cex = 0.5)

#delete all variable with correlation > 0.9
dati<-subset(dati, select = -c(funded_amnt_inv,
out_prncp_inv,total_pymnt_inv, total_rec_prncp, loan_amnt))
dati_cor<-dati[,sapply(dati,is.numeric)]
M <- cor(dati_cor,use="complete.obs")
corrplot(M, type="upper", order="hclust",method="number",number.cex=0.5,addCoef.col = "black",tl.cex = 0.5)

save(dati, file = "dati.RData")
write.csv(dati, file = "dati.csv")
}

#delete observation with NA
sum(!complete.cases(dati)) #number of NA: 21171
dati<-dati[complete.cases(dati),]

dati<-read.csv("dati.csv")

#transform the target categorical variable in a binary variable, 1 if the loan is in "Charged Off", "Default", "Does not meet the credit policy. Status:Charged Off",
#"In Grace Period", "Late (16-30 days)", "Late (31-120 days)", 0 otherwise
dati$loan_status_binary<-ifelse(dati$loan_status %in% c("Charged Off", "Default", "Does not meet the credit policy. Status:Charged Off",
"In Grace Period", "Late (16-30 days)", "Late (31-120 days)"),1,0)

#check
sum(dati$loan_status %in% c("Charged Off", "Default", "Does not meet the credit policy. Status:Charged Off",
"In Grace Period", "Late (16-30 days)", "Late (31-120 days)"))
table(dati$loan_status_binary)


#linear model
train<-sample(1:nrow(dati),0.7*nrow(dati))
test<-setdiff(1:nrow(dati),train)
fit<-lm(loan_status_binary~annual_inc+funded_amnt+grade,data=dati[train,])
summary(fit)
#i want to set a treeshold for the prediction, i.e. if the probability of the loan to be in default is >0.5 then the loan is in default
pred<-predict(fit,newdata=dati[test,],type="response")
pred<-ifelse(pred>0.14,1,0)
#i want a confusion matrix
table(pred,dati[test,]$loan_status_binary)
#i want the accuracy
sum(diag(table(pred,dati[test,]$loan_status_binary)))/sum(table(pred,dati[test,]$loan_status_binary))

# want to optimize accuracy with roc curve
library(pROC)
testRoc<-roc(dati$loan_status_binary ~ dati$annual_inc+dati$funded_amnt, plot=T, print.auc=T)
coords(testRoc, x="best")

summary(dati$annual_inc)
View(head(dati))

