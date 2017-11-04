library(devtools)
library(rmongodb)
library(plyr)
library(dplyr)


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
  lm <- lm(formula , data =train)
  
  
  summary(lm)
  
  #Multiple R-squared:  0.3451,	Adjusted R-squared:  0.3265 
  
  
  #result<-rmse(train$Loan_Status,(exp(linear_model$fitted.values)))
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

imputeMissingValue<-function(df){
  
  #KNN computes only for the numeric value not for the Char
  #use KNN as such since 2 function similar name is there
  #kNN imputation is correct function name
  #change non-numeric to numeric first and than impute
  library(VIM)
  
  
  df$Dependents<-as.integer(df$Dependents)
  #df[df$Dependents=='','Dependents'] <- 'NA'
  df[df$Dependents=='3+','Dependents'] <- '3'
  df$Dependents<-as.factor(df$Dependents)
  
  df$Gender=as.integer(df$Gender)
  df$Gender=as.factor(df$Gender)
  
  
  df$Self_Employed=as.integer(df$Self_Employed)
  df$Self_Employed=as.factor(df$Self_Employed)
  
  df$Married=as.integer(df$Married)
  #since test does not have '' showing error when
  #predicting ,hence commented 
  #Error when uncommented:"Type of predictors in new data do not match that of the training data"
  #df$Married=as.factor(df$Married)
  
  df$CoapplicantIncome <- as.integer( df$CoapplicantIncome)
  
  #df$Loan_Status   <- as.integer( df$Loan_Status  )
  
  df<- kNN(df,variable=c('Gender','Self_Employed','Married',
                         'Credit_History','Loan_Amount_Term',
                         'LoanAmount','CoapplicantIncome'))
  
  
  ##NOTE:
  ## when reading from db  make sure that data types are correclty mapped.
  # sicne it by default takes char
  # need to manually map to appropriate data type
  
  df$ApplicantIncome   <- as.integer( df$ApplicantIncome  )
  df$CoapplicantIncome   <- as.integer( df$CoapplicantIncome  )
  df$LoanAmount   <- as.integer( df$LoanAmount  )
  df$Loan_Amount_Term   <- as.integer( df$Loan_Amount_Term  )
  df$Credit_History   <- as.integer( df$Credit_History  )
  df$Loan_Status   <- as.factor(( df$Loan_Status  ))
  
  
  return(df)
}

library(data.table)

#combinedTable<-data.table(combined)






dbSelect <- function(export,collection){
  
  print(collection)
  ####1.count of records
  totRec<-mongo.count(mongo, collection)
  totRec
  
  
  ####2.list  of all records in table
  query <- mongo.bson.empty()
  #rec<-mongo.find(mongo,coll,query = query,limit=100L)
  cursor <- mongo.find(mongo, collection, query)
  # Step though the matching records and display them
  while (mongo.cursor.next(cursor)){
    tmp = mongo.bson.to.list(mongo.cursor.value(cursor))
    tmp.df = as.data.frame(t(unlist(tmp)), stringAsFactors = FALSE)
    export = rbind.fill(export, tmp.df)
    
  }
  mongo.cursor.destroy(cursor)
  
  return (export)
}






mongo <- mongo.create()
if(mongo.is.connected(mongo) == TRUE) {
  #export = data.frame(stringsAsFactors =false )
  #
  export = data.frame()
  train<-dbSelect(export,'analytics.loan_prediction_train')
  names(train)
  
  test<-dbSelect(export,'analytics.loan_prediction_test')
  names(test)
  
  #fmt_test<-data.frame(Loan_Status=rep("N" ,nrow(test)),test[,]); 
  test$Loan_Status<-'N'
  combined<-rbind(test,train)
  
  ###train
  
  train<-combined[368:981,]
  test<-combined[1:367,]
  
  
  ###########train data formatting
  
 dfNames <- c("Loan_ID","Gender", "Married" ,"Dependents","Education",
                  "Self_Employed","ApplicantIncome","CoapplicantIncome",
                  "LoanAmount","Loan_Amount_Term","Credit_History",
                  "Property_Area","Loan_Status")
  library(dplyr)
  train<-imputeMissingValue(train)
  train<-dplyr::select(train, 2:14)
  
  #colSums(is.na(train))
  # write.csv(file="fmttrain.csv",train, row.names=F)
   
  #################test
  test<-imputeMissingValue(test)
   test<-dplyr::select(test,2:14)
  #############################4. predict the loan_status
  
   library(randomForest)
   
  str(train)
  str(test)
  set.seed(615)
  
  #ApplicantIncome, CoapplicantIncome, Credit_History, LoanAmount.
  #Rejected 1 attributes: Self_Employed
  formula <- Loan_Status ~ ApplicantIncome +CoapplicantIncome+Credit_History+Loan_Amount_Term+LoanAmount+Self_Employed+
  Property_Area+Married
  
  
  
  colSums(is.na(train))
  library(Boruta)
  set.seed(123)
  boruta.train <- Boruta(Loan_Status~.-Loan_ID, data = train, doTrace = 2 ,holdHistory=TRUE)
  plotImpHistory(boruta.train);
  print(boruta.train)
   
  calculateMetrics(formula,train)
  # 
  # library(klaR)
  # 
  # require(boot)
  # split=0.80
  # train_control <- trainControl(method="boot", number=100)
  # train the model
  # model <- train(formula, data=train, trControl=train_control, method="nb")
  # print(model)
  # summarize results
  #confusionMatrix(predictions$class, y_test)
  train
  
  
  fit <- randomForest(formula, data=train,importance=TRUE, ntree=38)
  #colSums(is.na(test))
  pred=predict(fit,test)
  test$Loan_Status=pred
  
  #check no of Y and N
  table(pred, test$Loan_Status)
  
  #############################6. Generate Output
  
  write.csv(file="output.csv",c(test['Loan_ID'],test['Loan_Status']), row.names=F)
}
#Your score for this submission is : 0.777777777778. Your leaderboard score is : 0.777777777778
