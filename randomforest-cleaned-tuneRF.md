library("ggplot2")
library(data.table)
library(plyr)
library(dplyr)
library(VIM)
library(randomForest)


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
train<-select(train,Loan_Status:Property_Area)


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
formula<-Loan_Status ~ Gender+Married+Dependents+Education+ 
  Self_Employed+ApplicantIncome +CoapplicantIncome+LoanAmount+
  Loan_Amount_Term+Credit_History+Property_Area
#fit <-randomForest(Loan_Status~ .-Loan_ID-Self_Employed, data=train,importance=TRUE, ntree=500)
fit <- randomForest(Loan_Status~ . -Loan_ID, data=train,importance=TRUE, mtry=2,ntree=64,keep.inbag	=T,type='classification'
)

print(fit)
fit=update(fit,~.-Education-Depdendents)
# varImpPlot(fit)
attach(train)
par(mfrow=c(1,3))
plot(Loan_Status~Gender+Married+Dependents+Education+ 
       Self_Employed+ApplicantIncome +CoapplicantIncome+LoanAmount+
       Loan_Amount_Term+Credit_History+Property_Area)
#for classificatin it wont work
#points(Gender,fitted(fit),col="red",pch=20)
#plot(margin(fit,test$Loan_Status))

importance(fit)
summary(fit)
#Tune Random forest
#x= matrix of predictor
#y=response vector
predictor<- data.matrix(c(Credit_History,ApplicantIncome,LoanAmount))
#remove Loan_status,Loan_id(since loan_id is character vector not factor wont allowed in random #forest also it is unique records can't be changed to factor  variable)
tuneRf <- tuneRF(x=train[,-1:-2],y=train[,1], stepFactor=1.5)
#print(tuneRf)
#tune says mtry =2 yields good  OOB - i.e error rate is low

pred=predict(fit,test)
test$Loan_Status=pred

#############################6. Generate Output
write.csv(file="output.csv",c(test['Loan_ID'],test['Loan_Status']), row.names=F)
#Your score for this submission is : 0.791666666667. Your leaderboard score is : 0.805555555556
