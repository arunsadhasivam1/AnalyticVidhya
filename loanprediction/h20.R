library("ggplot2")



imputeMissingValue<-function(df){
  
  #KNN computes only for the numeric value not for the Char
  #use KNN as such since 2 function similar name is there
  #kNN imputation is correct function name
  #change non-numeric to numeric first and than impute
  library(VIM)
  
  

  
  df$Gender=as.integer(df$Gender)
  df$Gender=as.factor(df$Gender)
  
  
  df$Self_Employed=as.integer(df$Self_Employed)
  df$Self_Employed=as.factor(df$Self_Employed)
  df$Married=as.integer(df$Married)
  # df$CoapplicantIncome <- as.integer( df$CoapplicantIncome)
  
  df<- kNN(df,variable=c('Gender','Self_Employed','Married',
                         'Credit_History','Loan_Amount_Term',
                         'LoanAmount','CoapplicantIncome','Dependents'))
  
  #IMPORTANT: h20 treats '' as null but data.frame does not.
  #hence only for this added before knn imputation.
  df$Dependents<-as.integer(df$Dependents)
  #df[df$Dependents=='','Dependents'] <- 'NA'
  df[df$Dependents=='3+','Dependents'] <- '3'
  df$Dependents<-as.factor(df$Dependents)
  
  return(df)
}






library(data.table)


#############################3. Analyze data using train generated.

## H2O is an R package
library(h2o)
## Create an H2O cloud 
h2o.init(
  nthreads=-1            ## -1: use all available threads
  )    ## specify the memory size for the H2O cloud
h2o.removeAll()




#train= read.csv("train.csv",header=TRUE)
train<- h2o.importFile(path = "train.csv" )
test = h2o.importFile(path ="test.csv")
head(test)

test <-as.data.frame(test)
train <- as.data.frame(train)
#test$Loan_Status<-'N'
#test$Loan_Status<- as.factor(test$loan_Status)
test<- data.frame( Loan_Status=rep("N" ,nrow(test)),test[,])

class(train)
class(test)

str(train)
str(test)
combined<-rbind(test,train)
#combined<-as.h2o(combined)
###train
train<-combined[368:981,]
test<-combined[1:367,]

###########train data formatting
train<-imputeMissingValue(train)
library(dplyr)
train<-dplyr::select(train,Loan_Status:Property_Area)
str(train)
#################test
test<-imputeMissingValue(test)
test<-dplyr::select(test,Loan_Status:Property_Area)

#############################4. predict the loan_status


colSums(is.na(train))
colSums(is.na(test))


test$Loan_Status<- ifelse(test$Loan_Status=='Y',1,0)
train$Loan_Status<- ifelse(train$Loan_Status=='Y',1,0)



library(MASS)
library(stats)
library(randomForest)
str(train)
str(test)
 
#Rejected 1 attributes: Self_Employed
formula <- Loan_Status ~ ApplicantIncome +CoapplicantIncome+Credit_History+Loan_Amount_Term+LoanAmount+Self_Employed+
  Property_Area+Married




train<- as.h2o(train)
test<- as.h2o(test)
str(train)
train <- h2o.assign(train, "train.hex")   
 
rf1 <- h2o.randomForest(         ## h2o.randomForest function
  training_frame = train,        ## the H2O frame for training
  #validation_frame = valid,      ## the H2O frame for validation (not required)
  x=2:13,                        ## the predictor columns, by column index
  y=1,                          ## the target index (what we are predicting)
  model_id = "rfLoanPrediction",    ## name the model in H2O
  ##   not required, but helps use Flow
  ntrees = 39,                  ## use a maximum of 200 trees to create the
  ##  random forest model. The default is 50.
  ##  I have increased it because I will let 
  ##  the early stopping criteria decide when
  ##  the random forest is sufficiently accurate
  stopping_rounds = 2,           ## Stop fitting new trees when the 2-tree
  ##  average is within 0.001 (default) of 
  ##  the prior two 2-tree averages.
  ##  Can be thought of as a convergence setting
  score_each_iteration = T,      ## Predict against training and validation for
  ##  each tree. Default will skip several.
  seed = 1000000)                ## Set the random seed so that this can be
##  reproduced.
test <- h2o.assign(test, "test.hex") 

pred=predict(rf1,test)
test$Loan_Status=pred
test$Loan_Status<-ifelse(test$Loan_Status<0.5,'N','Y')

test<- as.data.frame(test)
#############################6. Generate Output

write.csv(file="output.csv",c(test['Loan_ID'],test['Loan_Status']), row.names=F)
#Your score for this submission is : 0.743055555556. Your leaderboard score is : 0.805555555556

