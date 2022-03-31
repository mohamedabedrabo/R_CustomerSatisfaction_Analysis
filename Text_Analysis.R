#Machine learning project on text file:
#Dataframe that contains reports, and we need to classify if they are from 'Union website' ot not.

#before anything i will run all the libraries that i might need:
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


#:::: Question no 1 ::::



#:: read the data :

setwd("C:/Users/mymod/OneDrive/Desktop/Courses/שנה ג/שנה ג- סמסטר ב/כריית ידע ולמידת מכונה/R projects")
onion.raw <- read.csv('OnionOrNot.csv' , stringsAsFactors = FALSE)

# not using the original df for safety:
onion <- onion.raw



#convert to yes/no from 0/1 :

onion$label <- factor(onion$label, levels= 0:1, labels = c('NO','YES'))


str(onion)
#onion$text <- as.character(onion$text)



#Making Pie Chart :

table(onion$label)
#we have 9000 yes, 15000 no :

slices <- c(9000,15000)
lbls <- c('Yes','No')

pie(c(9000,15000),c('Yes','No'),explode=0.1,
      main="Pie Chart of Yes/No ")




#Making corpus :
onion.corpus <- Corpus(VectorSource(onion$text))
onion.corpus[[1]][[1]]
onion.corpus[[1]][[2]]

#cleaning the corpus from Punctuation :
clean.corpus <- tm_map(onion.corpus, removePunctuation)
clean.corpus[[1]][[1]]

#cleaning the corpus from numbers :
clean.corpus <- tm_map(clean.corpus, removeNumbers)
clean.corpus[[1]][[1]]

#making all the text with small digits :
clean.corpus <- tm_map(clean.corpus, content_transformer(tolower))

#cleaning the corpus from stop words :
clean.corpus <- tm_map(clean.corpus, removeWords, stopwords())

#cleaning the corpus from spaces :
clean.corpus <- tm_map(clean.corpus, stripWhitespace)

#making document matrix because we want to see the dimensions-dim :
dtm <- DocumentTermMatrix(clean.corpus)
dim(dtm)



#we want to delete all the words that didnt show more than n times :

freq <- function(n,dtm,corpus) {
  dtm.freq <- DocumentTermMatrix(corpus, list(dictionary = findFreqTerms(dtm,n))) 
  
  return (dim(dtm.freq)[2])
}

freq(420,dtm,clean.corpus)


#:: C) ::



GraphFunction <- function(FUN){
  vec1 <- seq(100,1000,length.out = 180)
  vec2 <- c()
  i = 0
  for (item in vec1){
    i <- FUN(item,dtm,clean.corpus)
    vec2 <- append(vec2,i)
  }  
  df <- data.frame(vec1,vec2)
  names(df) <- c('n','frequency')
  Graph <- ggplot(df, aes(n, frequency)) + geom_point() + stat_smooth(method = lm)
  return (Graph)
}

GraphFunction(freq)



#:: D) ::

 #250 min, 48 words



#:: E) ::


#some changes and preperations :
dtm.freq <- DocumentTermMatrix(clean.corpus, list(dictionary = findFreqTerms(dtm,250)))

inspect(dtm.freq[1:10,1:20])

conv_01 <- function(x){
  x <- ifelse(x>0,1,0)
  return (as.integer(x))
}

dtm.final <- apply(dtm.freq, MARGIN = 1:2, conv_01)

dtm.df <- as.data.frame(dtm.final)

conv_01_type <- function(x){
  if (x =='NO') return(as.integer(0))
  return (as.integer(1))
}

onion$label <- sapply(onion$label, conv_01_type)

dtm.df$label <- onion$label 
str(dtm.df)



#filter/test/train :

filter <- sample.split(dtm.df$label, SplitRatio = 0.7)
onion.train = subset(dtm.df, filter==T)
onion.test = subset(dtm.df, filter==F)

dim(onion.train)
dim(onion.test)




#:::: Question no 3 ::::



#:: A) ::

#the model :
LR.model <- glm(label ~., family = binomial(link = 'logit'), data = onion.train)
summary(LR.model)

#prediction and actual :
predition <- predict(LR.model, onion.test, type = 'response')
actual <- onion.test$label

confusion_matrix <- table(actual,predition > 0.5)

precision <- confusion_matrix[2,2]/(confusion_matrix[2,2] + confusion_matrix[1,2])
recall <- confusion_matrix[2,2]/(confusion_matrix[2,2] + confusion_matrix[2,1]) 



#:: B) ::


#the model :
DT.model <- rpart(label ~., onion.train)
rpart.plot(DT.model, box.palette = "RdBu", shadow.col = "gray", nn= TRUE)

#prediction actual :
DTpredition <- predict(DT.model, onion.test)
DTactual <- onion.test$label

#Precision Recall :
DTconfusion_matrix <- table(DTactual,DTpredition > 0.50)

DTprecision <- DTconfusion_matrix[2,2]/(DTconfusion_matrix[2,2] + DTconfusion_matrix[1,2])
DTrecall <- DTconfusion_matrix[2,2]/(DTconfusion_matrix[2,2] + DTconfusion_matrix[2,1]) 


#:: C) ::

#Decision Tree


#:: D) ::


rocCurveLR <- roc(actual,predition, direction = ">", levels = c(1, 0))
rocCurveDT <- roc(DTactual,DTpredition, direction = ">", levels = c(1, 0))

#plot the chart 
plot(rocCurveLR, col = 'red',main = "ROC Chart")
par(new = TRUE)
plot(rocCurveDT, col = 'blue',main = "ROC Chart")

auc(rocCurveLR)
auc(rocCurveDT)



#:::: Question no 4 ::::



#:: A) ::


summary(DT.model)
#we can see that life, news, just are the most 3 important words.


#making df from these 3 :
dtm.new <- data.frame(dtm.df$life, dtm.df$news, dtm.df$just)
names(dtm.new) <- c("life", "news", "just")
dtm.new$label <- onion$label 

#filtering :
filterNew <- sample.split(dtm.new$label, SplitRatio = 0.7)
trainNew = subset(dtm.new, filter==T)
testNew = subset(dtm.new, filter==F)


#the model :
New.model <- rpart(label ~., trainNew)
rpart.plot(New.model, box.palette = "RdBu", shadow.col = "gray", nn= TRUE)

#prediction actual :
Newpredition <- predict(New.model, testNew)
Newactual <- testNew$label

#Precision Recall :
Newconfusion_matrix <- table(Newactual,DTpredition > 0.50)

accuracy.new <- (Newconfusion_matrix[2,1] + Newconfusion_matrix[2,2]) / length(testNew$label)



#:: B) ::

dtm.article <- data.frame("ARLINGTON, MA—Noting that from a quick glance it seemed like he’d fare all right, observers of a missing person poster confirmed Monday that the young man pictured looked like he could probably take care of himself out there. “Don’t get me wrong, it’s a drag that they can’t find him, but from what I can see he’s got a pretty strong frame and I don’t think it would kill him to sleep on a park bench for a couple of nights,” said local man Drew Klause, noting that the teenager’s spry build and youthful exuberance meant he would most likely bounce back from any trauma resulting from attempted muggings or scuffles with other homeless individuals. “I remember at that age I was in peak physical condition and basically felt like I was invincible, and there’s no way I was in as good of shape as this kid is. Plus, he’s got a really kind-looking face, so I bet it wouldn’t be hard to get someone to take pity on him and give him a few bucks for a motel room.” Klause went on to admit that if it turned out the missing teen had a chemical dependency or mental health issues, he was pretty much fucked.

")
names(dtm.article) <- c("text")

article.corpus <- Corpus(VectorSource(dtm.article$text))

#cleaning the corpus from Punctuation :
article.corpus <- tm_map(article.corpus, removePunctuation)


#cleaning the corpus from numbers :
article.corpus <- tm_map(article.corpus, removeNumbers)


#making all the text with small digits :
article.corpus <- tm_map(article.corpus, content_transformer(tolower))

#cleaning the corpus from stop words :
article.corpus <- tm_map(article.corpus, removeWords, stopwords())

#cleaning the corpus from spaces :
article.corpus <- tm_map(article.corpus, stripWhitespace)

#making document matrix because we want to see the dimensions-dim :
article.dtm <- DocumentTermMatrix(article.corpus)
dim(article.dtm)


article.final <- apply(article.dtm, MARGIN = 1:2, conv_01)

article.df <- as.data.frame(article.final)


ArticlePredition <- predict(LR.model, article.df, type = 'response')

