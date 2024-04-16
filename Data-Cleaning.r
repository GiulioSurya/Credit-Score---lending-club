remove(list = ls())
library(tidyverse)
library(leaps)
library(dplyr)
library(corrplot)

data<-read.csv("Loan_status_2007-2020Q3.gzip", na.strings = "")

#CLEANING
{
#removing  irrelevant variables for our analysis and those with a significant amount of NA
dati<-data[,colMeans(is.na(data))<0.5] 
dati <- subset(dati, select = -c(X, sub_grade,issue_d, url, title, zip_code, pymnt_plan, earliest_cr_line, fico_range_high, inq_last_6mths, pub_rec, revol_bal,
                                 initial_list_status, total_rec_late_fee, recoveries, collection_recovery_fee, 
                                 last_pymnt_d, last_pymnt_amnt, last_credit_pull_d, last_fico_range_high, policy_code, 
                                 acc_now_delinq, open_acc_6m, open_act_il, open_il_12m, acc_open_past_24mths, total_bal_il, 
                                 il_util,open_rv_12m, open_rv_24m, max_bal_bc, all_util, total_rev_hi_lim, inq_fi, 
                                 total_cu_tl, inq_last_12m, bc_open_to_buy, delinq_2yrs, mo_sin_old_il_acct, mo_sin_old_rev_tl_op, 
                                 mo_sin_rcnt_rev_tl_op, mo_sin_rcnt_tl, mths_since_rcnt_il, mths_since_recent_inq, 
                                 num_accts_ever_120_pd, num_actv_bc_tl, num_actv_rev_tl, num_bc_sats, num_bc_tl, num_il_tl, 
                                 total_acc, mths_since_recent_bc, num_op_rev_tl, num_rev_tl_bal_gt_0, num_sats, num_tl_120dpd_2m, 
                                 num_tl_30dpd, num_tl_op_past_12m, total_bal_ex_mort, total_il_high_credit_limit, 
                                 out_prncp_inv, emp_title))
view(data.frame(Variable = names(colSums(is.na(dati))), Missing_Values = colSums(is.na(dati))))

#removing redundant variables containing the same information:
#correlation matrix
dati_cor<-dati[,sapply(dati,is.numeric)]
M <- cor(dati_cor,use="complete.obs")
corrplot(M, type="upper", order="hclust",method="number",number.cex=0.5,addCoef.col = "black",tl.cex = 0.5)
#removing
dati<-subset(dati, select = -c(funded_amnt_inv,total_pymnt_inv, total_rec_prncp, loan_amnt))

#delete observation with at least one NA
sum(!complete.cases(dati)) #NA: 1 083 316
dati<-dati[complete.cases(dati),]

#TRANSFORMING VARIABLE 
#transform "revol_util" and "int_rate" in a numeric variable
dati$revol_util<-as.numeric(gsub("%","",dati$revol_util))
dati$int_rate<-as.numeric(gsub("%","",dati$int_rate))
#transform all character variable in factor
dati<-dati %>% mutate_if(is.character, as.factor)
#transform the target categorical variable "loan_status" in a binary variable: 
#-->1 if the loan is "Charged Off", "Default", "Does not meet the credit policy. Status:Charged Off",
#Late (31-120 days)".
#-->0 otherwise
unique(dati$loan_status)
dati$default<-ifelse(dati$loan_status %in% c("Charged Off", 
                "Default", "Does not meet the credit policy. Status:Charged Off", 
                "Late (31-120 days)"),1,0)
#check
sum(dati$loan_status %in% c("Charged Off", "Default", "Does not meet the credit policy. Status:Charged Off", "Late (31-120 days)"))
sum(dati$default=="1")
}

###### EXPLORATORY DATA ANALYSIS
{
dati%>%group_by(default)%>%summarize(count = n(), rel_count = count/nrow(dati)) %>% knitr::kable()
#->class imbalance: the class labeled as '0' constitutes 88% of the dataset

#Creating a new variable based on the ratio between the funded amount and the total payment
#dati$funded_amnt_ratio<-dati$funded_amnt/dati$total_pymnt
#summary(dati$funded_amnt_ratio)
#view(dati[,c("total_pymnt","funded_amnt","funded_amnt_ratio","default")])
#plot(dati$funded_amnt_ratio,dati$default,xlim=c(0,10))
#->this variabile can be usefull
}

#INCOME
plot(dati$annual_inc)
tail(sort(dati$annual_inc))
dati <- subset(dati, dati$annual_inc <= 20000000) #delete outiliers
plot(log(dati$annual_inc))
plot(log(dati$annual_inc[dati$hardship_flag=="Y"]),dati$default[dati$hardship_flag=="Y"])
#Break income into classes and assess the probability of default:
dati$annual_inc_class<-cut(dati$annual_inc, breaks = c(0,50000,100000,150000,200000,250000,300000,350000,400000,450000,500000,550000,600000,650000,700000,750000,Inf), labels = c("0-50k","50-100k","100-150k","150-200k","200-250k","250-300k","300-350k","350-400k","400-450k","450-500k","500-550k","550-600k","600-650k","650-700k","700-750k","750k+"))
table(dati$annual_inc_class, dati$default)
prop.table(table(dati$annual_inc_class, dati$default), 1)
#-> Similar default rates among income classes

#FICO
plot(dati$fico_range_low)
plot(dati$fico_range_low,dati$default)
table(dati$fico_range_low, dati$default)
prop.table(table(dati$fico_range_low, dati$default), 1)
#-> more default with lower fico

#INSTALLMENT
plot(dati$installment)
plot(dati$installment,dati$default)

#GRADE
table(dati$grade, dati$default)
prop.table(table(dati$grade, dati$default), 1)
#-> more default with lower grade

#HOME OWNERSHIP

table(dati$home_ownership, dati$default)
dati <- subset(dati, home_ownership != "NONE")
#just 3 obs
table(dati$home_ownership)# Remove unused levels from the home_ownership factor
dati$home_ownership <- droplevels(dati$home_ownership) 
prop.table(table(dati$home_ownership, dati$default), 1)
table(dati$home_ownership)


#Analyzing default distribution based on income among individuals with home_ownership=="MORTGAGE"
plot(log(dati$annual_inc[dati$home_ownership=="MORTGAGE"]),dati$default[dati$home_ownership=="MORTGAGE"])
#->Nothing of relevance

#EMPLOYMENT_LENGTH
table(dati$emp_length, dati$default)
prop.table(table(dati$emp_length, dati$default), 1)
#->Nothing of relevance: Similar default rates among emp_length

#VERIFICATION STATUS
table(dati$verification_status, dati$default)
prop.table(table(dati$verification_status, dati$default), 1)
#->Nothing of relevance

#PURPOSE
table(dati$purpose, dati$default)
prop.table(table(dati$purpose, dati$default), 1)
#->"small business" has the highest probability to be in default

#STATE
table(dati$addr_state, dati$default)
prop.table(table(dati$addr_state, dati$default), 1)
#->Nothing of relevance

#OPEN_ACC = number of open credit lines in the borrower's credit file
table(dati$open_acc, dati$default)
prop.table(table(dati$open_acc, dati$default), 1)
#why a lower number of open credit lines has a higher probability to be in default?################################

#REVOL_UTIL = amount of credit the borrower is using relative to all available revolving credit.
summary(dati$revol_util)
plot(dati$revol_util)
plot(dati$revol_util,dati$default)

#APPLICATION_TYPE
table(dati$application_type, dati$default)
prop.table(table(dati$application_type, dati$default), 1)

#TOT_CUR_BAL = total current balance of all accounts
summary(dati$tot_cur_bal)
plot(dati$tot_cur_bal)
tail(sort(dati$tot_cur_bal), n=20)
dati <- subset(dati, dati$tot_cur_bal <= 6000000) #delete outiliers 
plot(dati$tot_cur_bal,dati$default)
#higher the total current balance lower the probability to be in default, to be check the correlation with annual income

#OPEN_IL_24M = number of installment accounts opened in past 24 months
table(dati$open_il_24m, dati$default)
prop.table(table(dati$open_il_24m, dati$default), 1)
#->Nothing of relevance

#AVG_CUR_BAL = average current balance of all accounts
plot(dati$avg_cur_bal)
plot(dati$avg_cur_bal,dati$default)
# same of tot_cur_bal

#BC_UTIL = ratio of total current balance to high credit/credit limit for all bankcard accounts.
plot(dati$bc_util)
plot(dati$bc_util,dati$default)

#CHARGEOFF_WITHIN_12_MTHS = number of charge-offs within 12 months
table(dati$chargeoff_within_12_mths, dati$default)
prop.table(table(dati$chargeoff_within_12_mths, dati$default), 1)
#->Nothing of relevance

#DELINQ_AMNT = the past-due amount owed for the accounts on which the borrower is now delinquent.
plot(dati$delinq_amnt,dati$default)
plot(dati$delinq_amnt)
abline(h=70000, col="red")
dati <- subset(dati, delinq_amnt <= 70000) #delete outliers
summary(dati$delinq_amnt)

# MORT_ACC = number of mortgage accounts
table(dati$mort_acc, dati$default)
prop.table(table(dati$mort_acc, dati$default), 1)
#->Nothing of relevance

#NUM_REV_ACCTS = number of revolving accounts
table(dati$num_rev_accts, dati$default)
prop.table(table(dati$num_rev_accts, dati$default), 1)

#NUM_TL_90G_DPD_24M = number of accounts 90 or more days past due in last 24 months
table(dati$num_tl_90g_dpd_24m, dati$default)
prop.table(table(dati$num_tl_90g_dpd_24m, dati$default), 1)
#-> this could be interesting, high number of account has more default

#PCY_TL_NVR_DLQ = percent of trades never delinquent
plot(dati$pct_tl_nvr_dlq,dati$default)
#breaks the variable in classes c(0,10,20,30,40,50,60,70,80,90,100)
table(cut(dati$pct_tl_nvr_dlq, breaks = c(0,10,20,30,40,50,60,70,80,90,100)), dati$default)
prop.table(table(cut(dati$pct_tl_nvr_dlq, breaks = c(0,10,20,30,40,50,60,70,80,90,100)), dati$default), 1)
#-> Similar default rates among classes

#PERCENT_BC_GT_75 = percentage of all bankcard accounts > 75% of limit.
summary(dati$percent_bc_gt_75)
plot(dati$percent_bc_gt_75)
plot(dati$percent_bc_gt_75,dati$default)
#->Nothing of relevance

#PUB_REC_BANKRUPTCIES = number of public record bankruptcies
table(dati$pub_rec_bankruptcies, dati$default)
prop.table(table(dati$pub_rec_bankruptcies, dati$default), 1)

#TAX_LIENS = number of tax liens (ipoteche fiscali)
unique(dati$tax_liens)
table(dati$tax_liens, dati$default)
prop.table(table(dati$tax_liens, dati$default), 1)
#-> relevant

#TOT_HI_CRED_LIM = total high credit/credit limit
plot(dati$tot_hi_cred_lim)
dati <- subset(dati, tot_hi_cred_lim <= 7000000) #delete outliers
plot(dati$tot_hi_cred_lim,dati$default)

#TOTAL_BC_LIMIT = total bankcard high credit/credit limit
plot(dati$total_bc_limit)
dati <- subset(dati, total_bc_limit <= 800000) #delete outliers
plot(dati$total_bc_limit,dati$default)
# same behavior of tot_hi_cred_lim

#out_prncp = outstanding principal amount for the loan
plot(dati$out_prncp)
plot(dati_fit$out_prncp,dati_fit$default)
#really important

#DEBT_SETTLEMENT_FLAG = whether or not the borrower, who has charged-off, 
#is working with a debt-settlement company.
unique(dati$debt_settlement_flag)
table(dati$loan_status, dati$debt_settlement_flag)
table(dati$default, dati$debt_settlement_flag)
prop.table(table(dati$debt_settlement_flag, dati$loan_status), 1)
# basically all the observation with default=1 have flag -> no really important

save(dati, file = "dati.RData")





