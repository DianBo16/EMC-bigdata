#check for missing packages and install them
list.of.packages <- c("reshape","stats","graphics","zoo","timeDate","forecast")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#setwd("C:/Users/simivine/Desktop/Ğì¶«ÌÎ/EMC/EMC/card")
#read the raw data(.txt)
#the raw data should be placed under the current working directory
trade<-read.table("/home/nijiaqi/trade.txt",header=TRUE,sep="\t",stringsAsFactors=FALSE)

#save the data
save(trade,file="trade.RData")

#load the .RData
load("trade.RData")

#split the trade$timestamp 
temp<-strsplit(trade$timestamp," ")
temp<-matrix(unlist(temp),ncol=2,byrow=TRUE)
temp<-as.data.frame(temp,stringsAsFactors=FALSE)
colnames(temp)<-c("date","time")

# re-encode variables "time" as categorical variable "timecat"
temp<-within(temp,{
  timecat<-NA
  timecat[time<"10:00:00"]                     <-"Breakfast"
  timecat[time>="10:00:00"&time<"15:00:00"]    <-"Lunch"
  timecat[time>="15:00:00"&time<"24:00:00"]    <-"Dinner"
  
})

#combine the columns
#the column "count" is used to record the number of person consuming in the canteen in each record
#we assume that only one person in each record 
count<-1
trade.modify<-cbind(trade["syscode"],temp[c("date","timecat")],count)

#convert trade.modify$date to a time fromat
trade.modify$date<-as.Date(trade.modify$date)

#as the size of the data is too large to run,we select a subset:toaccount=100001
myset<-subset(trade.modify,syscode==34&date>="2014-09-01"&date<="2014-09-28")

#training with the data of the first three weeks of September 2014
#testing with the data of the fourth week of September 2014
#the training data set
trainset<-subset(myset,date<="2014-09-21")
#the testing data set
testset<-subset(myset,date>="2014-09-01")

#aggregate the data
dailycount.train<-aggregate(count ~ date+timecat, data =trainset, sum)
dailycount.test<-aggregate(count ~ date+timecat, data =testset, sum)

#select the subset
dailycount.Breakfast.train<-subset(dailycount.train,timecat=="Breakfast")
dailycount.Lunch.train<-subset(dailycount.train,timecat=="Lunch")
dailycount.Dinner.train<-subset(dailycount.train,timecat=="Dinner")

dailycount.Breakfast.test<-subset(dailycount.test,timecat=="Breakfast")
dailycount.Lunch.test<-subset(dailycount.test,timecat=="Lunch")
dailycount.Dinner.test<-subset(dailycount.test,timecat=="Dinner")

#using the ARIMA model
library(forecast)
library(xts)
dailycount.series.Breakfast<-ts(dailycount.Breakfast.train["count"],frequency=7)
dailycount.series.Lunch<-ts(dailycount.Lunch.train["count"],frequency=7)
dailycount.series.Dinner<-ts(dailycount.Dinner.train["count"],frequency=7)


plot.ts(dailycount.series.Breakfast, xlab="Breakfast", ylab="number")
plot.ts(dailycount.series.Lunch, xlab="Lunch", ylab="number")
plot.ts(dailycount.series.Dinner,xlab="Dinner", ylab="number")



dailycount.series.Breakfast1<-ts(dailycount.Breakfast.test["count"],frequency=7)
dailycount.series.Lunch1<-ts(dailycount.Lunch.test["count"],frequency=7)
dailycount.series.Dinner1<-ts(dailycount.Dinner.test["count"],frequency=7)


plot.ts(dailycount.series.Breakfast1, xlab="Breakfast", ylab="number")
plot.ts(dailycount.series.Lunch1, xlab="Lunch", ylab="number")
plot.ts(dailycount.series.Dinner1,xlab="Dinner", ylab="number")



auto.arima(dailycount.series.Breakfast,trace=T)
auto.arima(dailycount.series.Lunch,trace=T)
auto.arima(dailycount.series.Dinner,trace=T)

dailycount.arima.Breakfast<-arima(dailycount.series.Breakfast,order=c(0,0,1),seasonal=list(order=c(1,0,1),period=7))
dailycount.forecasts.Breakfast<-forecast.Arima(dailycount.arima.Breakfast,h=7,level=c(95))

dailycount.arima.Lunch<-arima(dailycount.series.Lunch,order=c(3,4,2),seasonal=list(order=c(1,0,0),period=7))
dailycount.forecasts.Lunch<-forecast.Arima(dailycount.arima.Lunch,h=7,level=c(95))

dailycount.arima.Dinner<-arima(dailycount.series.Dinner,order=c(2,0,2),seasonal=list(order=c(1,0,1),period=7))
dailycount.forecasts.Dinner<-forecast.Arima(dailycount.arima.Dinner,h=7,level=c(95))
#comparing

par(mfrow=c(3,1))
plot(dailycount.series.Breakfast1,col="green",ylim=c(1000,9000),main="Breakfast")
par(new=TRUE)
plot.forecast(dailycount.forecasts.Breakfast,plot.conf=FALSE,col="green",fcol="orange",shaded=FALSE,ylim=c(1000,9000),axes = FALSE,,main="")

plot(dailycount.series.Lunch1,col="red",ylim=c(1000,9000),main="Lunch")
par(new=TRUE)
plot.forecast(dailycount.forecasts.Lunch,plot.conf=FALSE,col="red",fcol="orange",shaded=FALSE,ylim=c(1000,9000),axes = FALSE,,main="")

plot(dailycount.series.Dinner1,col="blue",ylim=c(1000,9000),main="Dinner")
par(new=TRUE)
plot.forecast(dailycount.forecasts.Dinner,plot.conf=FALSE,col="blue",fcol="orange",shaded=FALSE,ylim=c(1000,9000),axes = FALSE,,main="")

