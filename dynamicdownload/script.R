library(downloader)

url<-"https://github.com/arunsadhasivam/AnalyticsVidhya/blob/master/BigMart/input/Test.csv"
download(url,"Test.csv")
test1 <- read.csv("Test.csv")

head(test,10)
