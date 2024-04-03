{
dati<-read.csv("loan_data_2007_2014.csv", na.strings = "")

#delete all the variable not correlated with the target
dati <- subset(dati, select = -c(member_id, emp_title,issue_d,pymnt_plan,url,desc,zip_code,
earliest_cr_line,mths_since_last_delinq,mths_since_last_record,annual_inc_joint,application_type,
collection_recovery_fee,recoveries,dti_joint,last_credit_pull_d,last_pymnt_d,mths_since_last_major_derog,policy_code,
revol_bal,revol_util,total_acc,open_acc_6m,
open_il_6m,open_il_12m,open_il_24m,mths_since_rcnt_il,total_bal_il,open_rv_12m,open_rv_24m,max_bal_bc,
all_util,total_rev_hi_lim,inq_fi,total_cu_tl,inq_last_12m,acc_now_delinq,tot_coll_amt,tot_cur_bal))

#delete all the variable with more than 50% of NA
dati<-dati[,colMeans(is.na(dati))<0.5]
View(head(dati))
}

# i want to save dati in an RData file
save(dati, file = "dati.RData")
