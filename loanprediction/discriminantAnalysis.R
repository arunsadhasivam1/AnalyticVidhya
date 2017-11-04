library("ggplot2")
library(data.table)
library(plyr)
library(dplyr)
library(VIM)
library(randomForest)
library(stats)

imputeMissingValue<-function(df){
  
  df$Dependents<-as.integer(df$Dependents)
  df[df$Dependents=='3+','Dependents'] <- '3'
  df$Dependents<-as.factor(df$Dependents)
  
  df$Gender=as.integer(df$Gender)
  df$Gender=as.factor(df$Gender)
  
  df$Self_Employed=as.integer(df$Self_Employed)
  df$Self_Employed=as.factor(df$Self_Employed)
  
  df$Married=as.integer(df$Married)
  
  df<- kNN(df,variable=c('Gender','Self_Employed','Married',
                         'Credit_History','Loan_Amount_Term',
                         'LoanAmount','CoapplicantIncome'))
  
  return(df)
}

#############################3. Analyze data using train generated.
train= read.csv("train.csv",header=TRUE)
test = read.csv("test.csv",header=TRUE)
fmt_test<-data.frame(Loan_Status=rep("N" ,nrow(test)),test[,]); 
combined<-rbind(fmt_test,train)

###train

train<-combined[368:981,]
test<-combined[1:367,]



###########train data formatting
str(train)
train<-imputeMissingValue(train)
train<-dplyr::select(train,Loan_Status:Property_Area)
train$Loan_Status<- ifelse(train$Loan_Status=='Y',1,0)


#colSums(is.na(train))
#write.csv(file="fmttrain.csv",train, row.names=F)

#################test

test<-imputeMissingValue(test)
test<-dplyr::select(test,Loan_Status:Property_Area)

#colSums(is.na(train))
#colSums(is.na(test))

#############################4. predict the loan_status

str(train)
str(test)
set.seed(615)
train$Loan_ID= as.character(train$Loan_ID)
test$Loan_ID= as.character(test$Loan_ID)
test$Loan_Status<- ifelse(test$Loan_Status=='Y',1,0)
train$Loan_Status<- ifelse(train$Loan_Status=='Y',1,0)
#logresult <- glm(Loan_Status~.-(Loan_ID), data=train, family=binomial)#not working
library(MASS)
logresult <- lda(Loan_Status~., data=train[,!colnames(train) %in% c("Loan_ID")])

pred=predict(logresult,newdata = test )
ldahist(data = pred$x[,1], g=test$Loan_Status)
attributes(pred)
test$Loan_Status=pred$class

#############################6. Generate Output
write.csv(file="output.csv",c(test['Loan_ID'],test['Loan_Status']), row.names=F)

#Random Forest      :Your score for this submission is : 0.805555555556. Your leaderboard score is : 0.805555555556
#Logistic regression:Your score for this submission is : 0.791666666667. Your leaderboard score is : 0.805555555556
#Discriminant Analysis:Your score for this submission is : 0.777777777778. Your leaderboard score is : 0.805555555556

