# ML- Project of 2020 (customer satisfaction) :
# install.packages("lubridate") 
# install.packages("dplyr") 
# install.packages("tm") 
# install.packages("ROSE") 
#running all the libraries :
library(ggplot2)
library(caTools)
library(lubridate)
library(dplyr)
library(tm)
library(ROSE)
library(corrplot)
library(rpart) 
library(rpart.plot)
library(randomForest)
library(pROC)
library(chron)

#reading the data :
setwd("C:/Users/mymod/OneDrive/Desktop/Courses/שנה ג/שנה ג- סמסטר ב/כריית ידע ולמידת מכונה/R projects")
satisfy.raw <- read.csv('satisfaction2.csv' , stringsAsFactors = TRUE)


# :::: note : we will sometimes use "satisfy.raw" data to draw graphs. but changes and work will be on "satisfy" data.


#checking somethings :
str(satisfy.raw)
summary(satisfy.raw)

satisfy <- satisfy.raw 
str(satisfy)
summary(satisfy)


#the target value :
table(satisfy.raw$satisfaction_v2)
str(satisfy.raw$satisfaction_v2)


# ::::: First Level :::: preparing the data ::::::






#1 : deleting the id field :
satisfy$id <- NULL


#2 : checking the gender field :
ggplot(satisfy.raw, aes(Gender, fill = satisfaction_v2)) + geom_bar(position = 'fill')
ggplot(satisfy.raw, aes(Gender, fill = satisfaction_v2)) + geom_bar()

#we can see that there is no affection/a little affection so we will delete
satisfy$Gender <- NULL


#3 : checking the Customer.Type field(loyal/disloyal) :
ggplot(satisfy.raw, aes(Customer.Type, fill = satisfaction_v2)) + geom_bar()
ggplot(satisfy.raw, aes(Customer.Type, fill = satisfaction_v2)) + geom_bar(position = 'fill')

#we can see that disloyal customers tends to be more dissatisfied


#4 : checking the Age field :
table(satisfy.raw$Age)
ggplot(satisfy.raw, aes(Age, fill = satisfaction_v2)) + geom_histogram(position = 'fill')

#we can see that middle age customer(25-55) tend to be more satesfied


#5 : checking the Type.of.Travel field(Business travel/Personal Travel) :
table(satisfy.raw$Type.of.Travel)
ggplot(satisfy.raw, aes(Type.of.Travel, fill = satisfaction_v2)) + geom_bar()
ggplot(satisfy.raw, aes(Type.of.Travel, fill = satisfaction_v2)) + geom_bar(position = 'fill')

#we can obviously see that Business travel customers tend to be more satisfied


#6 : checking the Class field(Business/Eco/Eco plus) :
table(satisfy.raw$Class)
ggplot(satisfy.raw, aes(Class, fill = satisfaction_v2)) + geom_bar()
ggplot(satisfy.raw, aes(Class, fill = satisfaction_v2)) + geom_bar(position = 'fill')

#we can see that eco plus class : has a little bit of customers and also have nearly the same effect as eco class, so we will combine them into eco:
make_eco <- function(x) {
  if(x=="Eco Plus") return(as.factor("Eco"))
  return(as.factor(x))
}

satisfy$Class <- sapply(satisfy$Class, make_eco)

#delete the eco plus level :
satisfy$Class <- droplevels(satisfy$Class)


#7 : checking the Flight.Distance field :
ggplot(satisfy.raw, aes(Flight.Distance, fill = satisfaction_v2)) + geom_histogram(position = 'fill')

#we can see that there is affection, its really weird but when the distance is more far, customer tend to be more satesfied


#8 : checking the Inflight.wifi.service field(satisfaction from 0-5 about wifi) :
table(satisfy.raw$Inflight.wifi.service)
ggplot(satisfy.raw, aes(Inflight.wifi.service, fill = satisfaction_v2)) + geom_bar(position = 'fill')


#i dont think that make sense because 0,5(highest and lowest level are the same), and 1 satifaction is more than 2,3.
#so we will delte this field.
satisfy$Inflight.wifi.service <- NULL


#9 : checking the Departure.Arrival.time.convenient(satisfaction from 0-5 about arrival time) :
table(satisfy.raw$Departure.Arrival.time.convenient)
ggplot(satisfy.raw, aes(Departure.Arrival.time.convenient, fill = satisfaction_v2)) + geom_bar(position = 'fill')

#we can hardly see any difference. beside,it shows that people said that departure time was good are less satesfied wich is not sense, so we will delete
satisfy$Departure.Arrival.time.convenient <- NULL


#10 : checking the Ease.of.Online.booking(satisfaction from 0-5 about online booking) :
table(satisfy.raw$Ease.of.Online.booking)
summary(satisfy.raw$Ease.of.Online.booking)
ggplot(satisfy.raw, aes(Ease.of.Online.booking, fill = satisfaction_v2)) + geom_bar(position = 'fill')

#the number of customers who rate 0 is samll , also we see that they have a big percentage of saisfaction which can be misleading so we will make it 1:
make_one <- function(x){
  if(x==0) return(as.integer(1))
  return(as.integer(x))
}

satisfy$Ease.of.Online.booking <- sapply(satisfy$Ease.of.Online.booking, make_one)
ggplot(satisfy, aes(Ease.of.Online.booking, fill = satisfaction_v2)) + geom_bar(position = 'fill')


#11 : checking the Gate.location(satisfaction from 0-5 about gate location) :
table(satisfy.raw$Gate.location)

satisfy$Gate.location <- sapply(satisfy$Gate.location, make_one)
ggplot(satisfy, aes(Gate.location, fill = satisfaction_v2)) + geom_bar(position = 'fill')
#we can see that gate location affects


#12 : checking the Food.and.drink(satisfaction from 0-5 about Food.and.drink) :
table(satisfy.raw$Food.and.drink)

satisfy$Food.and.drink <- sapply(satisfy$Food.and.drink, make_one)
ggplot(satisfy, aes(Food.and.drink, fill = satisfaction_v2)) + geom_bar(position = 'fill')
#we can see clearly that there is affection


#13 : checking the Online.boarding(satisfaction from 0-5 about Online.boarding) :
table(satisfy.raw$Online.boarding)

ggplot(satisfy.raw, aes(Online.boarding, fill = satisfaction_v2)) + geom_bar(position = 'fill')
#the number of customers who rate 0 is samll , also we see that they have a big percentage of saisfaction which can be misleading so we will make it 1:

satisfy$Online.boarding <- sapply(satisfy$Online.boarding, make_one)
ggplot(satisfy, aes(Online.boarding, fill = satisfaction_v2)) + geom_bar(position = 'fill')


#14 : checking the Seat.comfort(satisfaction from 0-5 about Seat.comfort) :
table(satisfy.raw$Seat.comfort)

satisfy$Seat.comfort <- sapply(satisfy$Seat.comfort, make_one)
ggplot(satisfy, aes(Seat.comfort, fill = satisfaction_v2)) + geom_bar(position = 'fill')
#we can see clearly that there is affection


#15 : checking the Inflight.entertainment(satisfaction from 0-5 about Inflight.entertainment) :
table(satisfy.raw$Inflight.entertainment)

satisfy$Inflight.entertainment <- sapply(satisfy$Inflight.entertainment, make_one)
ggplot(satisfy, aes(Inflight.entertainment, fill = satisfaction_v2)) + geom_bar(position = 'fill')
#we can see clearly that there is affection


#16 : checking the On.board.service(satisfaction from 0-5 about On.board.service) :
table(satisfy.raw$On.board.service)

satisfy$On.board.service <- sapply(satisfy$On.board.service, make_one)
ggplot(satisfy, aes(On.board.service, fill = satisfaction_v2)) + geom_bar(position = 'fill')
#we can see clearly that there is affection


#17 : checking the Leg.room.service(satisfaction from 0-5 about Leg.room.service) :
table(satisfy.raw$Leg.room.service)

satisfy$Leg.room.service <- sapply(satisfy$Leg.room.service, make_one)
ggplot(satisfy, aes(Leg.room.service, fill = satisfaction_v2)) + geom_bar(position = 'fill')
#we can see clearly that there is affection


#18 : checking the Baggage.handling(satisfaction from 0-5 about Baggage.handling) :
table(satisfy.raw$Baggage.handling)

ggplot(satisfy, aes(Baggage.handling, fill = satisfaction_v2)) + geom_bar(position = 'fill')
#we can see clearly that there is affection


#19 : checking the Checkin.service(satisfaction from 0-5 about Checkin.service) :
table(satisfy.raw$Checkin.service)

satisfy$Checkin.service <- sapply(satisfy$Checkin.service, make_one)
ggplot(satisfy, aes(Checkin.service, fill = satisfaction_v2)) + geom_bar(position = 'fill')
#we can see clearly that there is affection


#20 : checking the Inflight.service(satisfaction from 0-5 about Inflight.service) :
table(satisfy.raw$Inflight.service)

satisfy$Inflight.service <- sapply(satisfy$Inflight.service, make_one)
ggplot(satisfy, aes(Inflight.service, fill = satisfaction_v2)) + geom_bar(position = 'fill')
#we can see clearly that there is affection


#20 : checking the Cleanliness(satisfaction from 0-5 about Cleanliness) :
table(satisfy.raw$Cleanliness)

satisfy$Cleanliness <- sapply(satisfy$Cleanliness, make_one)
ggplot(satisfy, aes(Cleanliness, fill = satisfaction_v2)) + geom_bar(position = 'fill')
#we can see clearly that there is affection


#21 : checking the Departure.Delay.in.Minutes :
table(satisfy.raw$Departure.Delay.in.Minutes)
summary(satisfy.raw$Departure.Delay.in.Minutes)


ggplot(satisfy.raw, aes(Departure.Delay.in.Minutes, fill = satisfaction_v2)) + geom_histogram(position = 'fill')

make_number <- function(x){
  if(x>400) return(as.integer(400))
  return(as.integer(x))
}

satisfy$Departure.Delay.in.Minutes <- sapply(satisfy$Departure.Delay.in.Minutes, make_number)
ggplot(satisfy, aes(Departure.Delay.in.Minutes, fill = satisfaction_v2)) + geom_histogram(position = 'fill')
# there is small effect


#22 : checking the Arrival.Delay.in.Minutes :
table(satisfy.raw$Arrival.Delay.in.Minutes )
summary(satisfy.raw$Arrival.Delay.in.Minutes)

ggplot(satisfy.raw, aes(Arrival.Delay.in.Minutes , fill = satisfaction_v2)) + geom_histogram(position = 'fill')

make_zero <- function (x) {
  if (is.na(x)) {
    return(as.integer(0))
  }
  return(as.integer(x))
}

make_300 <- function(y){
  if(y>350) return(as.integer(350))
  return(as.integer(y))
}

satisfy$Arrival.Delay.in.Minutes <- sapply(satisfy$Arrival.Delay.in.Minutes, make_zero)
satisfy$Arrival.Delay.in.Minutes <- sapply(satisfy$Arrival.Delay.in.Minutes, make_300)

ggplot(satisfy, aes(Arrival.Delay.in.Minutes , fill = satisfaction_v2)) + geom_histogram(position = 'fill')
# we cant see affection so we will delete :

satisfy$Arrival.Delay.in.Minutes <- NULL



# the final df before making models :
str(satisfy)
summary(satisfy)





# ::::: Second Level :::: making models ::::::


# we split the data into trainning set and test set based on the 70/30 ratio that we're used to
filter <- sample.split(satisfy$satisfaction_v2, SplitRatio=0.7)

satisfy.test <- subset(satisfy,filter == F)
satisfy.train <- subset(satisfy,filter == T)

# first we start with dicision tree

model.dt <- rpart(satisfaction_v2~., satisfy.train)

# build the plot, the cex= changes the font size

rpart.plot(model.dt,box.palette="RdBu", shadow.col="gray", nn=TRUE,cex = 0.7)

# we evaluate the model on the test set
prediction.dt <- predict(model.dt, satisfy.test)

actual.dt <- satisfy.test$satisfaction_v2

# convert the factor to numeric

prediction.dt.corrected <- as.numeric(prediction.dt)

# there are two columns but we only use one

prediction.dt.corrected <- prediction.dt.corrected[1:(length(prediction.dt.corrected)/2) ]

# convert the factor to numeric

actual.dt.corrected <- as.numeric(actual.dt)-1

# calculate precision and recall
conf.dt <- table(actual.dt.corrected,prediction.dt.corrected>0.5)
str(conf.dt)

precision.dt <- conf.dt['1','TRUE'] / (conf.dt['1','TRUE'] + conf.dt['1', 'FALSE'])
recall.dt <-conf.dt['1','TRUE'] / (conf.dt['1','TRUE'] + conf.dt['0', 'TRUE'])

# draw roc curve 
library(pROC)

rocurve.dt <- roc(actual.dt.corrected, prediction.dt.corrected, direction = "<", levels = c("1", "0"))


plot(rocurve.dt,col ='red', main = 'ROC Chart')

# now we move to random forest

#install.packages("randomForest") 

library(randomForest)

model.rf <- randomForest(satisfaction_v2~., satisfy.train, importance = TRUE, na.action=na.exclude)

# test the test set
prediction.rf <- predict(model.rf, satisfy.test)

actual.rf <- satisfy.test$satisfaction_v2

# correct the results
prediction.rf.corrected <- as.numeric(prediction.rf)-1

actual.rf.corrected <- as.numeric(actual.rf)-1

#calculate precision and recall
conf.rf <- table(actual.rf.corrected ,prediction.rf.corrected>0.5)

precision.rf <- conf.rf['1', 'TRUE'] / (conf.rf['1','TRUE'] + conf.rf['1', 'FALSE'])

recall.rf <-conf.rf['1', 'TRUE'] / (conf.rf['1', 'TRUE'] + conf.rf['0', 'TRUE'])



# draw two roc curves for the two models
library(pROC)

rocurve.dt <- roc(actual.dt.corrected, prediction.dt.corrected, direction = "<", levels = c("1", "0"))

rocurve.rf <- roc(actual.rf.corrected, prediction.rf.corrected, direction = ">", levels = c("1", "0"))

plot(rocurve.dt,col ='red', main = 'ROC Chart')
par(new = TRUE)
plot(rocurve.rf,col ='blue', main = 'ROC Chart')


sample(conf.rf)


# we use this to see how much impact the data has on the models results
varImpPlot(model.rf)

