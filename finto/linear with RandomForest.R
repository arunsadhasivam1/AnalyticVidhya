train= read.csv("data/Train.csv",header=TRUE)
test = read.csv("data/Test.csv",header=TRUE)
test<-data.frame(Business_Sourced=rep(0 ,nrow(test)),test[,])




#colSums(is.na(train))

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

outlier<-function(df){
  
  
#   summary(train$Manager_Num_Products)
#   summary(train$Manager_Num_Coded)
#   summary(train$Manager_Num_Application)
#   
#   sum(train[train$Manager_Num_Application,'Manager_Num_Application']>=22)
  
  df[df$Manager_Num_Products>=100,'Manager_Num_Products']<- mean(df$Manager_Num_Products)
  df[df$Manager_Num_Coded>=9,'Manager_Num_Coded']<- mean(df$Manager_Num_Coded)
  df[df$Manager_Num_Application>=22,'Manager_Num_Application']<- mean(df$Manager_Num_Application)
  
  
  return (df)
  
}

applyMissingValues<-function(df){
  library(VIM)
  df<- kNN(df,variable=c(
    'Manager_Grade','Applicant_City_PIN',
    'Manager_Business',   
    'Manager_Num_Coded',
    'Manager_Num_Products',
    'Manager_Num_Application',
    'Manager_Num_Products2',
    'Manager_Business2'))
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


 

train<-applyMissingValues(train)
test<-applyMissingValues(test)


library(randomForest)
library(dplyr)
train<-select(train,ID:Business_Sourced)
test<-select(test,Business_Sourced:Manager_Num_Products2)

train<-removeFactor(train)
test<-removeFactor(test)


train$Business_Sourced <-as.integer(train$Business_Sourced)
test$Business_Sourced <-as.integer(test$Business_Sourced)

train$Business_Sourced <-as.factor(train$Business_Sourced)
test$Business_Sourced <-as.factor(test$Business_Sourced)




levels(test$Applicant_Gender) <- levels(train$Applicant_Gender)
levels(test$Applicant_Marital_Status) <- levels(train$Applicant_Marital_Status)
levels(test$Applicant_Occupation) <- levels(train$Applicant_Occupation)
levels(test$Manager_Joining_Designation) <- levels(train$Manager_Joining_Designation)
levels(test$Manager_Current_Designation) <- levels(train$Manager_Current_Designation)
levels(test$Applicant_Qualification) <- levels(train$Applicant_Qualification)





train<-outlier(train)
test<-outlier(test)



train$Applicant_Gender <- as.numeric(train$Applicant_Gender)
train$Applicant_Gender <- as.factor(train$Applicant_Gender)


train$Manager_DOJ <- as.character(train$Manager_DOJ)
train$Manager_DoB <- as.character(train$Manager_DoB)
train$Application_Receipt_Date<- as.character(train$Application_Receipt_Date)


train$Applicant_Marital_Status <- as.numeric(train$Applicant_Marital_Status)
train$Applicant_Marital_Status <- as.factor(train$Applicant_Marital_Status)

train$Applicant_Occupation <- as.numeric(train$Applicant_Occupation)
train$Applicant_Occupation <- as.factor(train$Applicant_Occupation)

train$Applicant_Qualification <- as.numeric(train$Applicant_Qualification)
train$Applicant_Qualification <- as.factor(train$Applicant_Qualification)


train$Manager_Joining_Designation <- as.numeric(train$Manager_Joining_Designation)
train$Manager_Joining_Designation <- as.factor(train$Manager_Joining_Designation)

train$Manager_Current_Designation <- as.numeric(train$Manager_Current_Designation)
train$Manager_Current_Designation <- as.factor(train$Manager_Current_Designation)

train$Manager_Status <- as.numeric(train$Manager_Status)
train$Manager_Status <- as.factor(train$Manager_Status)

train$Manager_Gender <- as.numeric(train$Manager_Gender)
train$Manager_Gender <- as.factor(train$Manager_Gender)


#TEST###################
test$Applicant_Gender <- as.numeric(test$Applicant_Gender)
test$Applicant_Gender <- as.factor(test$Applicant_Gender)


test$Manager_DOJ <-as.character(test$Manager_DOJ)
test$Manager_DoB<-as.character(test$Manager_DoB)
test$Application_Receipt_Date<-as.character(test$Application_Receipt_Date)




test$Applicant_Marital_Status <- as.numeric(test$Applicant_Marital_Status)
test$Applicant_Marital_Status <- as.factor(test$Applicant_Marital_Status)

test$Applicant_Occupation <- as.numeric(test$Applicant_Occupation)
test$Applicant_Occupation <- as.factor(test$Applicant_Occupation)

test$Applicant_Qualification <- as.numeric(test$Applicant_Qualification)
test$Applicant_Qualification <- as.factor(test$Applicant_Qualification)

test$Applicant_Qualification <- as.numeric(test$Applicant_Qualification)
test$Applicant_Qualification <- as.factor(test$Applicant_Qualification)


test$Manager_Joining_Designation <- as.numeric(test$Manager_Joining_Designation)
test$Manager_Joining_Designation <- as.factor(test$Manager_Joining_Designation)



test$Manager_Current_Designation <- as.numeric(test$Manager_Current_Designation)
test$Manager_Current_Designation <- as.factor(test$Manager_Current_Designation)




test$Manager_Status <- as.numeric(test$Manager_Status)
test$Manager_Status <- as.factor(test$Manager_Status)

test$Manager_Gender <- as.numeric(test$Manager_Gender)
test$Manager_Gender <- as.factor(test$Manager_Gender)


formula<-Business_Sourced ~ Applicant_City_PIN+Applicant_Gender+
  #Applicant_BirthDate+
  Applicant_Marital_Status+
  Applicant_Occupation+
  #Applicant_Qualification+
  #Manager_DOJ+
  #Manager_Joining_Designation+ 
  Manager_Current_Designation+
  Manager_Grade+Manager_Status+  Manager_Gender+
  #Manager_DoB+
  Manager_Num_Application+Manager_Num_Coded+
  Manager_Business+Manager_Num_Products+Manager_Business2+Manager_Num_Products2


#str(train)
#str(test)
#colSums(is.na(test))
set.seed(615)

##########Performance linear regression to identify important variables######################
#add a Target as numeric variable rather than categorial to do linear regression.
#Even you give type='regression' in randomforest() it still does
#classification unless change the target variable as numeric
train$Business_Sourced1 <- as.numeric(train$Business_Sourced)
#lm <- lm(formula=Business_Sourced1 ~ Applicant_City_PIN, data=train)

linearRegformula<-Business_Sourced ~ Applicant_City_PIN+Applicant_Gender+
  #Applicant_BirthDate+
  Applicant_Marital_Status+
  Applicant_Occupation+
  #Applicant_Qualification+
  #Manager_DOJ+
  #Manager_Joining_Designation+ 
  Manager_Current_Designation+
  Manager_Grade+
#Manager_Status+  Manager_Gender+
  #Manager_DoB+
  Manager_Num_Application+Manager_Num_Coded+
  #Manager_Business+
  Manager_Num_Products+
  #Manager_Business2+Manager_Num_Products2


lnRegRf <- lm(formula=Business_Sourced1 ~ Manager_Gender, data=train)
summary(lnRegRf)

library(Boruta)

Boruta::Boruta(formula=formula,data=train)
##########Performance linear regression to identify important variables######################



###################### Random forest classified based on lm idenity important variables##########
#After linear prediction idenitifes important variables add those do random forest classification
#linear validated Randomforest- variable selected based on linear importance R2-value
LnvalidatedRf<-Business_Sourced ~ Applicant_City_PIN+Applicant_Gender+
  Applicant_Marital_Status+
  Applicant_Occupation+
  Manager_Current_Designation+
  Manager_Grade+
  Manager_Num_Application+Manager_Num_Coded+
  Manager_Num_Products

set.seed(512)
rnf <- randomForest(formula=LnvalidatedRf, data=train,ntree=576,mtry=2)
print(rnf)
pred=predict(rnf,test)
test$Business_Sourced=pred
###############################



#fit$importanceSD
#varImpPlot(fit)

# attributes(pred)
# labels<-attr(pred,which="names")
# prediction<-data.frame(pred)
# 
# PredTrain = ROCR::prediction(prediction =prediction,labels)


#############################6. Generate Output
write.csv(file="output.csv",c(test['ID'],test['Business_Sourced']), row.names=F)

#0.58826	
