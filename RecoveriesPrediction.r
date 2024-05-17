remove(list = ls())
{
library(tidyverse)
library(leaps)
library(corrplot)
library(car)
library(ppcor)
library(caret)
library(glmnet)
}

load("dati_raw.RData")

#EDA

#CHECK FOR WICH MODALITY OF LOAN_STATUS WE HAVE RECOVERIES
dati$recoveries_bin <- cut(dati$recoveries, breaks = c(0, 1000, 5000, 10000, 20000, 50000, 100000, Inf), labels = c("0-1k", "1k-5k", "5k-10k", "10k-20k", "20k-50k", "50k-100k", "100k+"))
table(dati$recoveries_bin, dati$loan_status)
#only for loan_status="Charged Off" WE HAVE RECOVERIES
{
dati <- dati[dati$loan_status == "Charged Off",]
table(dati$loan_status)
#remove column with NA > 50%
dati <- dati[, colMeans(is.na(dati)) < 0.5]
# we don't need the recoveries_bin variable an loan_status
dati<-subset(dati, select=-c(recoveries_bin,loan_status))
#remove NA
dati <- dati[complete.cases(dati),]
#remove negative observation from last_pymnt_amnt
dati <- dati[dati$last_pymnt_amnt >= 0,]
}


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
#REMOVE REDUNDANT AND UNNECESSARY VARIABLES
dati<-subset(dati, select=-c(X,id,issue_d,grade,sub_grade,emp_title,url,
purpose,zip_code,fico_range_high,initial_list_status,last_fico_range_high,
last_fico_range_low,out_prncp_inv,out_prncp,addr_state))

#REMOVE VARIABLES WITH HIGH PARTIAL CORRELATION
recoveries_cor<-pcor(dati[,sapply(dati,is.numeric)], method = "pearson")
recoveries_cor<-recoveries_cor$estimate
View(head(dati[,sapply(dati,is.numeric)]))
which(colnames(dati[,sapply(dati,is.numeric)])=="recoveries")
#RECOVERIES HAS A CORRELATION HIGHER THAN 0.95 WITH VAR NUMBER 2,3,22 SO WE REMOVE THEM
valiables_rem_nam<-names(dati[,sapply(dati,is.numeric)])[c(2,3,22)]
dati<-dati[,!names(dati) %in% valiables_rem_nam]
}

#REMOVE VARIABLE WITH ONE MODALITY
dati<-subset(dati, select = -c(policy_code, application_type,pymnt_plan, earliest_cr_line,last_pymnt_d, last_credit_pull_d))# these are factor variable with only one modality or date


#FITTING THE MODEL


#BEST SUBSET SELECTION AND LINEAR MODEL
set.seed(12)
train<-sample(1:nrow(dati),0.7*nrow(dati))
test<-setdiff(1:nrow(dati),train)

stepwise_fit <- regsubsets(recoveries ~ ., data = dati[train,], nvmax = 200, really.big = TRUE, method="forward")
summary(stepwise_fit)$bic
#best model bic
best_model <- which.min(summary(stepwise_fit)$bic)
best_model
best_model_predictors <- names(coef(stepwise_fit, best_model))
View(best_model_predictors)
lm_fit<-lm(recoveries ~ loan_amnt+term+installment+verification_status+delinq_2yrs+fico_range_low+total_pymnt_inv+
total_rec_prncp+total_rec_int+total_rec_late_fee+open_rv_24m+total_rev_hi_lim+debt_settlement_flag,data = dati[train,])
summary(lm_fit)
vif(lm_fit)
#plot density of residuals
plot(density(lm_fit$residuals))


#LINEAR MODEL
lm_fit <- lm(recoveries ~ loan_amnt+term+int_rate+fico_range_low+log(total_pymnt+1)+tot_cur_bal+mort_acc+num_bc_sats+num_bc_tl+
debt_settlement_flag+total_rec_prncp+total_rec_int,data = dati[train,])
 summary(lm_fit)
vif(lm_fit)


predict_recoveries <- predict(lm_fit, newdata = dati[test,])
rmse <- sqrt(mean((predict_recoveries - dati[test, "recoveries"])^2))
rmse

#DIAGNOSTIC
qqPlot(lm_fit, main = "QQ plot")
plot(lm_fit, which=1)
varImp(lm_fit)

# K-FOLD CROSS-VALIDATION
set.seed(12)
cv_model <- train(recoveries ~ loan_amnt+term+int_rate+fico_range_low+log(total_pymnt+1)+tot_cur_bal+mort_acc+num_bc_sats+num_bc_tl+
debt_settlement_flag+total_rec_prncp+total_rec_int, data = dati[train,], method = "lm", trControl = trainControl(method = "cv", number = 10))
cv_model$results
cv_model$finalModel
predict_recoveries <- predict(cv_model, newdata = dati[test,])
rmse <- sqrt(mean((predict_recoveries - dati[test, "recoveries"])^2))
rmse


#LASSO REGRESSION
x <- model.matrix(recoveries ~ ., data = dati[train,])
y <- dati[train, "recoveries"]
lasso_fit <- cv.glmnet(x, y, alpha = 1, nfolds = 10)
plot(lasso_fit)
best_lambda <- lasso_fit$lambda.min
best_lambda
lasso_fit <- glmnet(x, y, alpha = 1, lambda = best_lambda)
predict_recoveries <- predict(lasso_fit, newx = model.matrix(recoveries ~ ., data = dati[test,]))
rmse <- sqrt(mean((predict_recoveries - dati[test, "recoveries"])^2))
rmse
coef(lasso_fit)

#RIDGE REGRESSION
x <- model.matrix(recoveries ~ ., data = dati[train,])
y <- dati[train, "recoveries"]
ridge_fit <- cv.glmnet(x, y, alpha = 0, nfolds = 10)
plot(ridge_fit)
cv.err <- cv.glmnet(x, y, alpha = 0)
best_lambda <- cv.err$lambda.1se
best_lambda
ridge_fit <- glmnet(x, y, alpha = 0, lambda = best_lambda)
predict_recoveries <- predict(ridge_fit, newx = model.matrix(recoveries ~ ., data = dati[test,]))
rmse <- sqrt(mean((predict_recoveries - dati[test, "recoveries"])^2))
rmse
coef(ridge_fit)

