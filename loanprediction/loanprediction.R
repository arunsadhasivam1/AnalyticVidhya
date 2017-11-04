library("data.table")
library("ggplot2")
library("randomForest")
library(caret)
library(randomForest)


train= read.csv("input/loan/train.csv",header=TRUE)
test = read.csv("input/loan/test.csv",header=TRUE)

fmt_test<-data.frame(Loan_Status=rep("N" ,nrow(test)),test[,]); 

combined<-rbind(fmt_test,train)


result<-combined[1:367,]
 

result[which( is.na(result$LoanAmount)  ),'LoanAmount']<- 0
#to check specific record whether value is updated
#print(result[[9]][[80]])



result[which(result$ApplicantIncome>=result$CoapplicantIncome),"Loan_Status"]<-'Y'

result$LoanAmount <- as.integer(result$LoanAmount)



#to check loan amount which is zero
loanZero<-result[which(result$LoanAmount ==0 ),c("Loan_ID","Loan_Status","LoanAmount")]

print (loanZero)
#result[which(result$LoanAmount==0),"Loan_Status"]<-'Y'
result[which(result$LoanAmount ==0),"Loan_Status"]<-'Y'
 

output<-list(c(result["Loan_ID"],result["Loan_Status"]))
#print (output)


write.csv(file="output.csv",output, row.names=F)
 
