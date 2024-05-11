remove(list = ls())
{
library(tidyverse)
library(leaps)
library(dplyr)
library(corrplot)
library(car)
library(ppcor)
library(caret)
}

load("dati_raw.RData")

{
#we want to know for  which modality of loan_status the recoveries are done
dati$recoveries_bin <- cut(dati$recoveries, breaks = c(0, 1000, 5000, 10000, 20000, 50000, 100000, Inf), labels = c("0-1k", "1k-5k", "5k-10k", "10k-20k", "20k-50k", "50k-100k", "100k+"))
table(dati$recoveries_bin, dati$loan_status)
#only for loan_status="Charged Off" the recoveries are done, we will use only this modality

dati <- dati[dati$loan_status == "Charged Off",]
table(dati$loan_status)
#remove column with NA > 50%
dati <- dati[, colMeans(is.na(dati)) < 0.5]
# we don't need the recoveries_bin variable an loan_status
dati<-subset(dati, select=-c(recoveries_bin,loan_status))
#remove NA
dati <- dati[complete.cases(dati),]
}
save(dati, file = "dati_clean.RData")


###DATA TRANSFORMATION

{
#transform "revol_util" and "int_rate" in a numeric variable
dati$revol_util<-as.numeric(gsub("%","",dati$revol_util))
dati$int_rate<-as.numeric(gsub("%","",dati$int_rate))
#transform all character variable in factor
dati<-dati %>% mutate_if(is.character, as.factor)
#MUTATE policy_code in factor
dati$policy_code<-as.factor(dati$policy_code)
#winzorize the variable recoveries to 95 percentile
recoveries_95<-quantile(dati$recoveries,0.95)
dati$recoveries[dati$recoveries>recoveries_95]<-recoveries_95
#winzorize revol_bal to 95 percentile
revol_bal_95<-quantile(dati$revol_bal,0.99)
dati$revol_bal[dati$revol_bal>revol_bal_95]<-revol_bal_95
#winzorize total_cur_bal to 99 percentile
total_cur_bal_99<-quantile(dati$tot_cur_bal,0.99)
dati$tot_cur_bal[dati$tot_cur_bal>total_cur_bal_99]<-total_cur_bal_99
}



{
#i want to see the variables and their correlation with recoveries, only for variable with correlation between -0.1 and 0.1
#dati_cor <- dati[, sapply(dati, is.numeric)]
#correlation_matrix<- cor(dati_cor, use = "complete.obs")
#View(correlation_matrix["recoveries",])
#correlation with recoveries
#recoveries_cor<-correlation_matrix["recoveries",]
#absolute values
#recoveries_cor<-abs(recoveries_cor)
#now keep only the variable with correlation <0.05
#recoveries_cor<-recoveries_cor[recoveries_cor<0.05]
#removed_Var<-names(recoveries_cor)

#keep correlated and factor variable
#dati<-dati[,!names(dati) %in% removed_Var]

#delete useless variable
dati<-subset(dati, select=-c(X,id,issue_d,grade,sub_grade,emp_title,url,
purpose,zip_code,fico_range_high,initial_list_status,last_fico_range_high,
last_fico_range_low,out_prncp_inv,out_prncp))


####################PARTIAL CORRELATION
####################
#now we look for partial correlation
recoveries_cor<-pcor(dati[,sapply(dati,is.numeric)], method = "pearson")
recoveries_cor<-recoveries_cor$estimate
View(head(dati[,sapply(dati,is.numeric)]))
which(colnames(dati[,sapply(dati,is.numeric)])=="recoveries")
which(colnames(dati[,sapply(dati,is.numeric)])=="total_pymnt")
#RECOVERIES HAS A CORRELATION HIGHER THAN 0.85 WITH VAR NUMBER 1 2 3 16 17  18 19 20 22 24  I WILL TRY TO REMOVE THEM
valiables_rem_nam<-names(dati[,sapply(dati,is.numeric)])[c(2,3,17, 18, 19, 20, 22, 24)]
dati<-dati[,!names(dati) %in% valiables_rem_nam]
}


{


#dati<-subset(dati, select=-c(X,id,loan_amnt,funded_amnt_inv,term,issue_d,grade,sub_grade,emp_title,pymnt_plan,url,
#purpose,zip_code,addr_state,earliest_cr_line,fico_range_high,initial_list_status,total_pymnt_inv,last_pymnt_d,last_credit_pull_d,last_fico_range_high,
#last_fico_range_low,policy_code,hardship_flag,debt_settlement_flag))

#dati<-subset(dati, select=-c(emp_length,verification_status,loan_status,title))
####step wise forward selection
#correlation matrix
dati_cor<-dati[,sapply(dati,is.numeric)]
M <- cor(dati_cor,use="complete.obs")
corrplot(M, type="upper",method="number",number.cex=0.5,addCoef.col = "black",tl.cex = 0.5)

#delete correlated variable
#dati<-subset(dati, select = -c(num_sats,tot_hi_cred_lim,total_bal_ex_mort,total_il_high_credit_limit,num_actv_bc_tl))


dati<-subset(dati, select = -c(policy_code, application_type,pymnt_plan, earliest_cr_line,last_pymnt_d, last_credit_pull_d,addr_state))# these are factor variable with only one modality or date
}


#step wise forward selection
#split the data in train and test
set.seed(12)
train<-sample(1:nrow(dati),0.7*nrow(dati))
test<-setdiff(1:nrow(dati),train)

stepwise_fit <- regsubsets(recoveries ~ ., data = dati[train,], nvmax = 200, really.big = TRUE, method="forward")
stepwise_fit<-regsubsets(recoveries ~ ., data = dati[train,], nvmax = 200, really.big = TRUE,method="backward")
summary(stepwise_fit)$bic
#best model bic
best_model <- which.min(summary(stepwise_fit)$bic)
best_model
best_model_predictors <- names(coef(stepwise_fit, best_model))
View(best_model_predictors)
#remove intercept
best_model_predictors <- best_model_predictors[!best_model_predictors %in% "(Intercept)"]
# add recoveries to the best model predictors
#fit the lm with the best model variables-forward
lm_fit <- lm(recoveries ~ loan_amnt+term+int_rate+verification_status+dti+fico_range_low+pub_rec+total_pymnt+
last_pymnt_amnt+tot_cur_bal+open_il_24m+mo_sin_old_rev_tl_op+mo_sin_rcnt_rev_tl_op+
mort_acc+num_actv_rev_tl+num_bc_sats+num_bc_tl+debt_settlement_flag,data = dati[train,])
#fit with backward covariates
lm_fit <- lm(recoveries ~ loan_amnt+installment+home_ownership+verification_status+dti+delinq_2yrs+fico_range_low+revol_bal+total_pymnt+last_pymnt_amnt+
total_rev_hi_lim+acc_open_past_24mths+mo_sin_old_rev_tl_op+mort_acc+num_actv_rev_tl+num_bc_sats+num_bc_tl+debt_settlement_flag+hardship_flag,data = dati[train,])
#my fit with logical variable
lm_fit<-lm(recoveries ~ loan_amnt+fico_range_low+log(revol_bal+1)+log(total_pymnt+1)+tot_cur_bal, data = dati[train,])
summary(lm_fit)
vif(lm_fit)
predict_recoveries <- predict(lm_fit, newdata = dati[test,])
rmse <- sqrt(mean((predict_recoveries - dati[test, "recoveries"])^2))
rmse
predict_recoveries <- predict(lm_fit, newdata = dati[test,])
#plot scatter  of recoveries and revol_bal+1 with the regression line
plot(log(dati$tot_cur_bal),dati$recoveries, xlim = c(0, 20), ylim = c(0, 20000), xlab = "log(tot_cur_bal)", ylab = "recoveries")
summary(dati$total_pymnt)
View(head(dati))
qqPlot(lm_fit, main = "QQ plot")

#plot density of recoveries
plot(density(log(dati$recoveries+1)), main = "Density of recoveries", xlab = "Recoveries")

#annotazioni: allora il problema ora è loan_amnt+ tipo log(revol_bal+1) diventa significativo e il coefficiente ha senso quando lo tolgo, ora vedo
# cosa succede con altre variabili
log(revol_bal+1)

summary(dati$loan_amnt)
#plot recovieries

#rmse


#correlation between dti

#annotazione, inserendo il log di loan amount MSE è aumentato,

vif(lm_fit)
#diagnotic plot
#qq plot
qqPlot(lm_fit, main = "QQ plot")
plot(lm_fit, which=1)

#try with lasso
library(glmnet)
x <- model.matrix(recoveries ~ ., data = dati[train,])[,-1]
y <- dati[train, "recoveries"]
cv_fit <- cv.glmnet(x, y, alpha = 1)
best_lambda <- cv_fit$lambda.min
lasso_fit <- glmnet(x, y, alpha = 1, lambda = best_lambda)
predict_recoveries_lasso <- predict(lasso_fit, newx = model.matrix(recoveries ~ ., data = dati[test,][,-1]))
rmse_lasso <- sqrt(mean((predict_recoveries_lasso - dati[test, "recoveries"])^2))
rmse_lasso

#specificare distribuzione su glm e vedere con aic o bic
#provare lasso
#provare ridge

#i want to plot the density only on train
plot(density(dati[train, "recoveries"]), main = "Density of recoveries", xlab = "Recoveries")


png("pairs.png")
pairs(test1)
#i wanto to download the plot
dev.off()


