
library("ggplot2")



printGraph<- function(Approved,Gender,Loan_Status){
  ggplot(train, aes(Gender, Loan_Status)) +
    geom_bar( stat = "identity") +
    theme(axis.text.x = element_text(angle = 70, vjust =0.10 , color = "navy")) +
    xlab("Gender") + ylab("Loan_Status")+ggtitle("Gender vs Loan_Status")
  
  ggplot(train, aes(LoanAmount, Loan_Status)) +
    geom_bar( stat = "identity") +
    theme(axis.text.x = element_text(angle = 70, vjust =0.10 , color = "navy")) +
    xlab("LoanAmount") + ylab("Loan_Status")+ggtitle("LoanAmount vs Loan_Status")
  
  ggplot(train, aes(Loan_Amount_Term, Loan_Status)) +
    geom_bar( stat = "identity") +
    theme(axis.text.x = element_text(angle = 70, vjust =0.10 , color = "navy")) +
    xlab("Loan_Amount_Term") + ylab("Loan_Status")+ggtitle("Loan_Amount_Term vs Loan_Status")
  
  
   
  
}
format<-function(df){
  
  #LoanAmount  Loan_Amount_Term    Credit_History
  #3.Dependents
  df$Dependents<-as.character(df$Dependents)
  df[df$Dependents=='','Dependents'] <- -1
  df[df$Dependents=='3+','Dependents'] <- '3'
  df$Dependents<-as.factor(df$Dependents)
  
  library(plyr)
  #df[ is.na(df)]<- -1
  
  #5.LoanAmount
  
  #write.csv(file="combined.csv",combined, row.names=F)
  
    return (df)
  
}

calculateMetrics<-function(formula,df){
  library(Metrics)
  library(dplyr)
  #since lm expect numeric dependent variable(response variable)
  train$Loan_Status<-as.numeric(train$Loan_Status)
  #removing Married since married is not factor
  lm <- lm( log(Loan_Status ) ~., data =train)
  
 # cor(df$Loan_Status,df$Married)
  #confidence level
  #confint(lm)
 # abline(lm)
 return( summary(lm))
  
  #Multiple R-squared:  0.3451,	Adjusted R-squared:  0.3265 
  
  
  result<-rmse(train$Loan_Status,(exp(lm$fitted.values)))
  #result
   #plot(lm)
  #dont return unless return it wont change the original value
  
   
  library(rpart)
  library(e1071)
   library(rpart.plot)
   library(caret)
  
  #setting the tree control parameters
   fitControl <- trainControl(method = "cv", number = 5)
   cartGrid <- expand.grid(.cp=(1:50)*0.01)
   formula<-Loan_Status ~ Gender+Married+Dependents+Education+ 
     Self_Employed+ApplicantIncome +CoapplicantIncome+LoanAmount+
     Loan_Amount_Term+Credit_History+Property_Area
   

   
  #decision tree
    #tree_model <- train(formula, data = train, method = "rpart",
    #                    trControl = fitControl, tuneGrid = cartGrid)
      #summary(tree_model)
  #plot(tree_model)
}

formatcategorialVar<-function(df){
  library(plyr)
  
  #Gender
  df$Gender<-as.character(df$Gender)
  df[which(df$Gender==''),'Gender']<-'NS'
  df$Gender<-as.factor(df$Gender)
  
  #Self Employeed
  df$Self_Employed<-as.character(df$Self_Employed)
  df[which(df$Self_Employed==''),'Self_Employed']<-'NS'
  df$Self_Employed<-as.factor(df$Self_Employed)

  
  return(df)
}

changeNumericToCatgeorialVar<-function(df){
  #df<-train
   
  #df$ApplicantIncome<-cut(df$ApplicantIncome,breaks=c(0,1000,5000,10000,50000,90000),
  #labels=c(1,2,3,4,5 ),right=FALSE)
 
 #df$CoapplicantIncome<-cut(df$CoapplicantIncome,breaks=c(0,1,1000,5000,50000,90000),
  #labels=c(1,2,3,4,5),right=FALSE)
  
  #train$LoanAmount<-as.integer(train$LoanAmount)
  #df$LoanAmount<-cut(df$LoanAmount,breaks=c(0,100,500,1000),labels=c(1,2,3),right=FALSE)
 # train$ApplicantIncome<-as.integer(train$ApplicantIncome)
  #df$ApplicantIncome<-cut(train$ApplicantIncome,breaks=c(0,100,200,500),labels=c(1,2,3),right=FALSE)
  #df[which(df$CoapplicantIncome==0),'CoapplicantIncome']<- min(df$CoapplicantIncome)
   
 
  return (df)
}

imputeMissingValue<-function(df){
  
 #KNN computes only for the numeric value not for the Char
 #use KNN as such since 2 function similar name is there
 #kNN imputation is correct function name
 #change non-numeric to numeric first and than impute
 library(VIM)
  
  
  
  
  
 df$Dependents<-as.integer(df$Dependents)
 df[df$Dependents=='3+','Dependents'] <- '3'
 df$Dependents<-as.factor(df$Dependents)
  
 df$Gender=as.integer(df$Gender)
 df$Gender=as.factor(df$Gender)
 
 
 df$Self_Employed=as.integer(df$Self_Employed)
 df$Self_Employed=as.factor(df$Self_Employed)
 
 df$Education=as.integer(df$Education)
 df$Education=as.factor(df$Education)
 
 
 df$Married=as.integer(df$Married)
 # df$Credit_History<-as.factor(df$Credit_History)
 #since test does not have '' showing error when
 #predicting ,hence commented 
 #Error when uncommented:"Type of predictors in new data do not match that of the training data"
 #df$Married=as.factor(df$Married)
 
 df$CoapplicantIncome <- as.integer( df$CoapplicantIncome)

 
 df<- kNN(df,variable=c('Gender','Self_Employed','Married',
                    'Credit_History','Loan_Amount_Term',
                    'LoanAmount','Dependents','CoapplicantIncome'))
  
  return(df)
}

library(data.table)

#combinedTable<-data.table(combined)


#############################3. Analyze data using train generated.
train= read.csv("train.csv",header=TRUE)
test = read.csv("test.csv",header=TRUE)
fmt_test<-data.frame(Loan_Status=rep("N" ,nrow(test)),test[,]); 
combined<-rbind(fmt_test,train)

###train

train<-combined[368:981,]
test<-combined[1:367,]
###########train data formatting
train<-imputeMissingValue(train)
train<-select(train,Loan_Status:Property_Area)
train<-changeNumericToCatgeorialVar(train)

#write.csv(file="fmttrain.csv",train, row.names=F)

#################test
test<-imputeMissingValue(test)
test<-select(test,Loan_Status:Property_Area)
test<-changeNumericToCatgeorialVar(test)

# colSums(is.na(test))

#write.csv(file="fmttest.csv",test, row.names=F)


 

 #############################4.predict the loan_status

library(randomForest)
#str(test)
set.seed(615)
formula<-Loan_Status ~ Gender+Married+Dependents+Education+ 
  Self_Employed+ApplicantIncome +CoapplicantIncome+LoanAmount+
  Loan_Amount_Term+Credit_History+Property_Area

#calculateMetrics(formula,train)
fit <- randomForest(formula, data=train,importance=TRUE, ntree=39, stepFactor=2)
#varImpPlot(fit)
#colSums(is.na(test))
pred=predict(fit,test)
test$Loan_Status=pred
summary(fit$err.rate)
########xgboost############
require(xgboost)
require(Matrix)
require(data.table)
library(plyr)


sparse_matrix_train <- sparse.model.matrix(formula, data = train)

resultMatrix<-data.frame(Loan_Status=c(train$Loan_Status))
#resultMatrix$data<-test
resultMatrix$Loan_Status<-as.factor(resultMatrix$Loan_Status)
resultMatrix$Loan_Status<-revalue(resultMatrix$Loan_Status,c('1'='1','2'='0'))
resultMatrix$Loan_Status<- as.factor(resultMatrix$Loan_Status)


bst <- xgboost(data = sparse_matrix_train, label =resultMatrix$Loan_Status, max.depth = 4,
               eta = 1, nthread = 2, nround = 10)

sparse_matrix_test <- sparse.model.matrix(formula, data = test)
pred <- predict(bst, sparse_matrix_test)
pred
test$Loan_Status<-ifelse(as.integer(pred)<=0,'N','Y')
test$Loan_Status



#############################6. Generate Output

write.csv(file="output.csv",c(test['Loan_ID'],test['Loan_Status']), row.names=F)



#0.805555555556(20)
