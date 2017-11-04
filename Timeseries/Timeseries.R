train= read.csv("Train.csv",header=TRUE)
test = read.csv("Test.csv",header=TRUE)
memory.size(max = TRUE)
memory.limit(size = 3072)


ts<-ts(c(train$Datetime,train$Count),frequency=12,start=c(2012,8)) 
formula <- Count ~ Datetime
lm <- lm(formula , data =train)
plot.ts(ts  ,ann = FALSE, frame.plot = TRUE)
summary(lm)