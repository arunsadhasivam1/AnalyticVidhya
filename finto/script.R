train= read.csv("data/Train.csv",header=TRUE)
test = read.csv("data/Test.csv",header=TRUE)
test<-data.frame(Business_Sourced=rep(0 ,nrow(test)),test[,])


train<-applyMissingValues(train)
test<-applyMissingValues(test)


#colSums(is.na(train))

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

outlier<-function(df){
  
  
  #df[df$Manager_Num_Products>=100,'Manager_Num_Products']<- mean(df$Manager_Num_Products)
  #df[df$Manager_Num_Coded>=9,'Manager_Num_Coded']<- mean(df$Manager_Num_Coded)
  #df[df$Manager_Num_Application>=22,'Manager_Num_Application']<- mean(df$Manager_Num_Application)
  
  
  return (df)
  
}

applyMissingValues<-function(df){
library(VIM)

  
  
df<- kNN(df,variable=c(
                     'Manager_Grade','Manager_Num_Application','Applicant_City_PIN',
                       'Manager_Business',   
                       'Manager_Num_Coded','Manager_Num_Products2',
                    'Manager_Business2','Manager_Num_Products'))
  return (df)
}


removeFactor <-function(df){
  df$ID <- as.character(df$ID)
  df$Application_Receipt_Date <- as.character(df$Application_Receipt_Date)
  df$Applicant_BirthDate <- as.character(df$Applicant_BirthDate)
  df$Manager_DOJ <- as.character(df$Manager_DOJ)
  df$Manager_DoB <- as.character(df$Manager_DoB)
  
  return (df)
}


# formula<-Business_Sourced ~ ID+Office_PIN+Application_Receipt_Date+Applicant_City_PIN+Applicant_Gender+Applicant_BirthDate+Applicant_Marital_Status+Applicant_Occupation+Applicant_Qualification+Manager_DOJ+Manager_Joining_Designation+Manager_Current_Designation+Manager_Grade+Manager_Status+Manager_Gender+Manager_DoB+Manager_Num_Application+Manager_Num_Coded+Manager_Business+Manager_Num_Products+Manager_Business2+Manager_Num_Products2
# 


library(randomForest)
library(dplyr)
df<-select(train,ID:Business_Sourced)
test<-select(test,Business_Sourced:Manager_Num_Products2)

train<-removeFactor(train)
test<-removeFactor(test)


train$Business_Sourced <-as.integer(train$Business_Sourced)
test$Business_Sourced <-as.integer(test$Business_Sourced)

train$Business_Sourced <-as.factor(train$Business_Sourced)
test$Business_Sourced <-as.factor(test$Business_Sourced)



str(train)
str(test)


#formula<-Business_Sourced ~ .
# formula<-Business_Sourced ~ Office_PIN+Applicant_City_PIN+Applicant_Gender+Applicant_Marital_Status+Applicant_Occupation+Applicant_Qualification+Manager_Joining_Designation+Manager_Current_Designation+Manager_Grade+Manager_Status+Manager_Gender+Manager_Num_Application+Manager_Num_Coded+Manager_Business+Manager_Num_Products+Manager_Business2+Manager_Num_Products2



levels(test$Applicant_Gender) <- levels(train$Applicant_Gender)
levels(test$Applicant_Marital_Status) <- levels(train$Applicant_Marital_Status)
levels(test$Applicant_Occupation) <- levels(train$Applicant_Occupation)
levels(test$Manager_Joining_Designation) <- levels(train$Manager_Joining_Designation)
levels(test$Manager_Current_Designation) <- levels(train$Manager_Current_Designation)
levels(test$Applicant_Qualification) <- levels(train$Applicant_Qualification)



formula<-Business_Sourced ~ Applicant_City_PIN+
  Applicant_Occupation+Applicant_Qualification+Manager_Joining_Designation+
  Manager_Current_Designation+Manager_Grade+Manager_Status+
  Manager_Gender+Manager_Num_Application+Manager_Num_Coded+
  Manager_Business+Manager_Num_Products+Manager_Num_Products2
str(train)
set.seed(615)


train<-outlier(train)
test<-outlier(test)
nrow(train)
train1<-subset(train,train$Manager_Num_Products<100)
nrow(train1)
rnf <- randomForest(formula=formula, data=train,ntree=135,mtry=2,na.action = na.omit)


#train[,!names(train) %in% c('ID','Application_Receipt_Date','Applicant_BirthDate','Manager_DOJ','Manager_DoB','')]

print(rnf)
pred=predict(rnf,test)
test$Business_Sourced=pred

#fit$importanceSD
#varImpPlot(fit)

nrow(train)

#############################6. Generate Output
write.csv(file="output.csv",c(test['ID'],test['Business_Sourced']), row.names=F)

#0.58826	
