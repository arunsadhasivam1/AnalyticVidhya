


library("ggplot2")
library(rpart)
library(e1071)
library(rpart.plot)
library(caret)
library(dplyr)


train= read.csv("BigMart/input/Train.csv",header=TRUE)
test = read.csv("BigMart/input/Test.csv",header=TRUE)


#colSums(is.na(train))
#Item_Weight         na

plotGraph<-function(){
  ggplot(train, aes(Item_Type, Item_MRP)) +
    geom_boxplot() +ggtitle("Box Plot") + 
    theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "red")) +
    xlab("Item Type") + ylab("Item MRP") + ggtitle("Item Type vs Item MRP")
  
  ggplot(train, aes(x= Item_Visibility, y = Item_Outlet_Sales)) + 
    geom_point(size = 2.5, color="navy") +
    xlab("Item Visibility") + ylab("Item Outlet Sales") +
    ggtitle("Item Visibility vs Item Outlet Sales")
  
  
  
  ggplot(train, aes(Item_Type, Item_Outlet_Sales)) +
    geom_bar( stat = "identity") +
    theme(axis.text.x = element_text(angle = 70, vjust =0.10 , color = "navy")) +
    xlab("Item Type") + ylab("Item Outlet Sales")+ggtitle("Item Type vs Sales")
  
  ggplot(train, aes(Item_Type, Item_MRP)) +
    geom_boxplot() +ggtitle("Box Plot") + 
    theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "red")) +
    xlab("Item Type") + ylab("Item MRP") + ggtitle("Item Type vs Item MRP")
  
  
  ggplot(combined, aes(x= Item_Visibility, y = Item_Outlet_Sales)) + 
    geom_point(size = 2.5, color="navy") +
    xlab("Item Visibility") + ylab("Item Outlet Sales") +
    ggtitle("Item Visibility vs Item Outlet Sales")
  
}

test$ Item_Outlet_Sales<-1
combined<-rbind(train,test)
combined$Item_Weight[is.na(combined$Item_Weight)]<-median(combined$Item_Weight,na.rm=T)
combined$Item_Visibility <- ifelse(combined$Item_Visibility == 0,
                                   median(combined$Item_Visibility), combined$Item_Visibility)            
#plotGraph()

##########
library(plyr)

combined$Item_Fat_Content <-revalue(combined$Item_Fat_Content, c("low fat" = "Low Fat"))


#################  model test

train=combined[1:8523,]
test=combined[8524:nrow(combined),]

result<-''
#query<- train%>%
#arrange(Item_Outlet_Sales)%>%
#mutate(result=1-train$Item_Weight)
library(Metrics)
linear_model <- lm(log(Item_Outlet_Sales) ~ ., data = train)
print(linear_model)
par(mfrow=c(2,2))


result<-rmse(train$Item_Outlet_Sales, exp(linear_model$fitted.values))
test$Item_Outlet_Sales=result


fitControl<-trainControl(method="cv",number=5)
cartGrid<-expand.grid(.cp=(1:10)*0.01)

tree_model<-train(Item_Outlet_Sales ~ ., data = train, method = "rpart",
                  trControl = fitControl, tuneGrid = cartGrid)

#print(tree_model)
write.csv(file="result.csv",c(test['Item_Identifier'],test['Outlet_Identifier'],	test['Item_Outlet_Sales']), row.names=F)
#query<-select (train,c(Item_Identifier))

