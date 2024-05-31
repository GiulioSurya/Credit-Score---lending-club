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
dati <- dati[dati$loan_status %in% c("Charged Off", "Does not meet the credit policy. Status:Charged Off"), ]
table(dati$loan_status)
#remove column with NA > 50%
dati <- dati[, colMeans(is.na(dati)) < 0.5]
# we don't need the recoveries_bin variable an loan_status
dati<-subset(dati, select=-c(recoveries_bin,loan_status))
#remove NA
dati <- dati[complete.cases(dati),]
#remove negative observation from last_pymnt_amnt
dati <- dati[dati$last_pymnt_amnt >= 0,]
#keep only values of recoveries > 0
dati <- dati[dati$recoveries > 0,]
}


###DATA TRANSFORMATION

{
#transform "revol_util" and "int_rate" in a numeric variable
dati$revol_util<-as.numeric(gsub("%","",dati$revol_util))
dati$int_rate<-as.numeric(gsub("%","",dati$int_rate))
#transform all character variable in factor
dati<-dati %>% mutate_if(is.character, as.factor)
}


{
#REMOVE REDUNDANT AND UNNECESSARY VARIABLES
dati<-subset(dati, select=-c(X,id,issue_d,grade,sub_grade,emp_title,url,
purpose,zip_code,fico_range_high,initial_list_status,last_fico_range_high,
last_fico_range_low,out_prncp_inv,out_prncp,addr_state, funded_amnt,collection_recovery_fee,total_pymnt_inv,funded_amnt_inv))
#REMOVE VARIABLE WITH ONE MODALITY
dati<-subset(dati, select = -c(policy_code, application_type,pymnt_plan, earliest_cr_line,last_pymnt_d, last_credit_pull_d, hardship_flag))# these are factor variable with only one modality or date

}


#FITTING THE MODEL


#BEST SUBSET SELECTION AND LINEAR MODEL
set.seed(12)
train<-sample(1:nrow(dati),0.7*nrow(dati))
test<-setdiff(1:nrow(dati),train)



stepwise_fit <- regsubsets(log(recoveries) ~ ., data = dati[train,], nvmax = 200, really.big = TRUE, method="forward")
summary(stepwise_fit)$bic
#best model bic
best_model <- which.min(summary(stepwise_fit)$bic)
best_model
best_model_predictors <- names(coef(stepwise_fit, best_model))
View(best_model_predictors)

lm_fit <- lm(log(recoveries) ~ loan_amnt+ term+ 
             int_rate + installment + verification_status+ 
             title+ fico_range_low + log(total_pymnt)+ 
             total_rec_prncp + total_rec_int + total_rec_late_fee + last_pymnt_amnt + 
             total_rev_hi_lim + num_rev_accts + debt_settlement_flag, data = dati[train,])
summary(lm_fit)

vif(lm_fit)
#plot density of residuals
plot(density(lm_fit$residuals))
plot(lm_fit, which=1)
# test for heteroskedasticity
ncvTest(lm_fit)
#compute confidence interval with robust variance estimator
confint(lm_fit, vcov = vcovHC(lm_fit, type = "HC1"))

predict_recoveries<- predict(lm_fit, newdata = dati[test,], interval = "confidence", 
                                   vcov = vcovHC(lm_fit, type = "HC1"))

#rmse 
rmse <- sqrt(mean((exp(predict_recoveries) - dati[test, "recoveries"]))^2)
rmse



#LINEAR MODEL
lm_fit <- lm(log(recoveries) ~ loan_amnt+term+int_rate+fico_range_low+log(total_pymnt)+tot_cur_bal+mort_acc+num_bc_sats+num_bc_tl+
debt_settlement_flag+total_rec_prncp+total_rec_int+total_rec_late_fee,data = dati[train,])
summary(lm_fit)
vif(lm_fit)
ncvTest(lm_fit)
#compute confidence interval with robust variance estimator
confint(lm_fit, vcov = vcovHC(lm_fit, type = "HC1"))
plot(density(lm_fit$residuals))
qqPlot(lm_fit, main = "QQ plot")
plot(lm_fit, which=1)

plot(density(lm_fit$residuals))

predict_recoveries<- predict(lm_fit, newdata = dati[test,], interval = "confidence", 
                                  vcov = vcovHC(lm_fit, type = "HC1"))
rmse <- sqrt(mean((exp(predict_recoveries) - dati[test, "recoveries"]))^2)
rmse


#try to fit a gamma distribution
#log likelihood function
llk_gamma <- function(params, data) {
  alpha <- params[1]
  beta <- params[2]
  n <- length(data)
  log_likelihood <- n * (alpha * log(beta) - log(gamma(alpha))) + 
                    (alpha - 1) * sum(log(data)) - beta * sum(data)
  return(log_likelihood)
}

#optim and estimate of parameters
fit <- optim(c(1, 2), function(params) -llk_gamma(params, dati$recoveries),
             method = "L-BFGS-B", lower = c(0.001, 0.001))
fit$par
alpha_est <- fit$par[1]
beta_est <- fit$par[2]

#check distribution
plot(density(dati$recoveries), main = "Density of recoveries")
curve(dgamma(x, shape = alpha_est, rate = beta_est), add = TRUE, col = "red")

#fit a glm with gamma distribution
gamma_fit <- glm(recoveries ~ loan_amnt+ term+ 
             int_rate + installment + verification_status+ 
             title+ fico_range_low + log(total_pymnt)+ 
             total_rec_prncp + total_rec_int + total_rec_late_fee + last_pymnt_amnt
             + debt_settlement_flag, data = dati[train,], family = Gamma(link = "log"))
summary(gamma_fit)
vif(gamma_fit)
predict_recoveries <- predict(gamma_fit, newdata = dati[test,], type = "response")
rmse <- sqrt(mean((predict_recoveries - dati[test, "recoveries"])^2))
rmse
deviance_residuals <- residuals(gamma_fit, type = "deviance")
plot(density(deviance_residuals))
lines(seq(-5, 5, 0.1), dnorm(seq(-5, 5, 0.1),0,1), col = "red")

plot(gamma_fit, which = 1)
plot(gamma_fit, which = 2)
plot(gamma_fit, which = 3)


x <- model.matrix(log(recoveries) ~ ., data = dati[train,])[,-1]
y <- dati[train, "recoveries"]
lasso_fit <- cv.glmnet(x, y, alpha = 1, nfolds = 10)
plot(lasso_fit)
best_lambda <- lasso_fit$lambda.min
best_lambda
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)
#predict recoveris with lasso_fit
predict_recoveries <- predict(best_model, newx = model.matrix(log(recoveries) ~ ., data = dati[test,])[,-1])
rmse <- sqrt(mean((predict_recoveries - dati[test, "recoveries"])^2))
rmse

#plot residuals vs fitted
plot(predict_recoveries, dati[test, "recoveries"] - predict_recoveries, xlab = "Fitted values", ylab = "Residuals")





