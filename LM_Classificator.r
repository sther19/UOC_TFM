#####################################################################################################
#                                                                                                   #
#                                SENTIMENT CLASSIFICATOR WITH LM                                    #
#                                                                                                   #
# Script Description:  Analysis of different LM Methods in order to buy a Tweet Sentiment           #
#                      Analys Classificator                                                         #
#                                                                                                   #
# File Location:       https://www.kaggle.com/crowdflower/twitter-airline-sentiment                 #
#                      github (Folder Data; File Tweets.csv)                                        #
#                                                                                                   #
# Author:              Esther Cordova                                                               #
# Last Update:         2017-02-06                                                                   #
#                                                                                                   #
#####################################################################################################

#*******************************************      Parameters   **************************************  
dir<-"C:/Users/esther.cordova/Desktop/Code_TFM/Files"       #Ptah where File is stored;To be changed
file<-"Tweets.csv"                                           #File Name

wordsToRemove = c('get', 'cant', 'can', 'now', 'just', 'will', 'dont', 'ive', 'got', 'much')
#****************************************************************************************************


#*******************************************      Libraries    **************************************
if(!require(lubridate)){
  install.packages('lubridate',repos='http://cran.es.r-project.org')
  require(lubridate)
} 
if(!require(tm)){
  install.packages('tm',repos='http://cran.es.r-project.org')
  require(tm)
}
if(!require(slam)){
  install.packages('slam',repos='http://cran.es.r-project.org')
  require(slam)
}
if(!require(e1071)){
  install.packages('e1071',repos='http://cran.es.r-project.org')
  require(e1071)
}
if(!require(MASS)){
  install.packages('MASS',repos='http://cran.es.r-project.org')
  require(MASS)
}
if(!require(lattice)){
  install.packages('lattice',repos='http://cran.es.r-project.org')
  require(lattice)
}
if(!require(caret)){
  install.packages('caret',repos='http://cran.es.r-project.org')
  require(caret)
}
if(!require(klaR)){
  install.packages('klaR',repos='http://cran.es.r-project.org')
  require(klaR)
}
if(!require(rpart)){
  install.packages('rpart',repos='http://cran.es.r-project.org')
  require(rpart)
}
if(!require(gtools)){
  install.packages('gtools',repos='http://cran.es.r-project.org')
  require(gtools)
}
set.seed(606)
#****************************************************************************************************


#*******************************************      Functions    **************************************
#Corpus Function for text mining
analyseText = function(text_to_analyse){
  # analyse text and generate matrix of words
  # Returns a dataframe containing 1 tweet per row, one word per column
  # and the number of times the word appears per tweet
  CorpusTranscript = Corpus(VectorSource(text_to_analyse))
  CorpusTranscript = tm_map(CorpusTranscript, content_transformer(tolower), lazy = T)
  CorpusTranscript = tm_map(CorpusTranscript, PlainTextDocument, lazy = T)
  CorpusTranscript = tm_map(CorpusTranscript, removePunctuation)
  CorpusTranscript = tm_map(CorpusTranscript, removeWords, wordsToRemove)
  CorpusTranscript = tm_map(CorpusTranscript, removeWords, stopwords("english"))
  CorpusTranscript = DocumentTermMatrix(CorpusTranscript)
  CorpusTranscript = removeSparseTerms(CorpusTranscript, 0.97) # keeps a matrix 97% sparse
  CorpusTranscript = as.data.frame(as.matrix(CorpusTranscript))
  colnames(CorpusTranscript) = make.names(colnames(CorpusTranscript))
  
  return(CorpusTranscript)
}

#Custom Random Forest Functions
customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes
#****************************************************************************************************


#*********************************   Read and Preprocessing Data  ***********************************
setwd(dir)
rawdata<-read.csv(file=file,sep=",",header = T)

#Obtenemos variable fecha, hora y dia de la semana
rawdata$weekday<-wday(as.Date(rawdata$tweet_created,format="%Y-%m-%d %H:%M:%S"),label=T)
rawdata$date<-as.Date(rawdata$tweet_created,format="%Y-%m-%d %H:%M:%S")
rawdata$hora<-substring(rawdata$tweet_created,12,13)                           

#Eliminamos 16 y 24 de Febrero pq no estan los dias completos
rawdata<-subset(rawdata,!(as.character(date) %in% c('2015-02-16','2015-02-24')))

# Remove the @airline bit of the text of the tweet and tweets larger than 170
rawdata$text = gsub("^@\\w+ *", "", rawdata$text)
rawdata$text_length <- sapply(as.character(rawdata$text), function(x) nchar(x))
rawdata<-rawdata[rawdata$text_length<170,]

#subset only positive and negative sentiments and necessary fields from rawdat dataset 
smalldf<-rawdata[,c('airline_sentiment','airline','negativereason','text')]
tweet_data <- smalldf[smalldf$airline_sentiment != 'neutral',c(1,4)]
tweet_data$airline_sentiment<-factor(tweet_data$airline_sentiment)
tweet_data$text<-gsub("\\bthank\\b","thanks",tweet_data$text)
#****************************************************************************************************


#*********************************   Subset Train and Test Data   ***********************************
# seleccionar 2/3 al azar
N=dim(tweet_data)[1]
all=seq(1,N)
dtrain=sort(sample(N,N*0.75))
dtest=setdiff(all,dtrain)
train=tweet_data[dtrain,]
test=tweet_data[dtest,]

#Aplico funcion corpus a train y test
xTrain = analyseText(train$text)
yTrain<-train$airline_sentiment
xTest = analyseText(test$text)
yTest<-test$airline_sentiment

#Añado a xTrain y xTest palabras que no tienen en comun por poco voluem de datos
xTrain$Type<-"Train"
xTest$Type<-"Test"
temp<-smartbind(xTrain,xTest)
temp[is.na(temp)] <- 0
xTest<-subset(temp,temp$Type=="Test")
xTrain<-subset(temp,temp$Type=="Train")
xTrain$Type<-xTest$Type<-NULL
#****************************************************************************************************


#*********************************     Define CV parameters       ***********************************
# Paso de kfolds
control <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions = TRUE)

# Metrica de calidad % instancias correctas sobre total
metric <- "Accuracy"
seed<-547
#****************************************************************************************************


#******************************  Test of different Models with Caret   ******************************
#Naive bayes
set.seed(seed)
model.nb = train(xTrain,yTrain,'nb',metric=metric,trControl=control)
model.nb$results
plot(model.nb)
predict.nb = predict(model.nb, newdata=xTest, type="raw") 
confusionMatrix(predict.nb,yTest)
CM_nb<-confusionMatrix(predict.nb,yTest)
ctreeVarImp = varImp(model.nb)
plot(ctreeVarImp)

#logistic regression
set.seed(seed)
model.lr = train(xTrain,yTrain,'glm',family="binomial",metric=metric,trControl=control)
model.lr$results
predict.lr = predict(model.lr, newdata=xTest, type="raw") 
confusionMatrix(predict.lr,yTest)
CM_lr<-confusionMatrix(predict.lr,yTest)
ctreeVarImp = varImp(model.lr)
plot(ctreeVarImp)

#Generalized Boosted Models 
set.seed(seed)
model.gb = train(xTrain,yTrain,'gbm',metric=metric,trControl=control)
model.gb$results
plot(model.gb)
ctreeVarImp = varImp(model.gb)
plot(ctreeVarImp)
model.gb$bestTune
summary(model.gb)
predict.gb = predict(model.gb, newdata=xTest, type="raw") 
confusionMatrix(predict.gb,yTest)
CM_gb<-confusionMatrix(predict.gb,yTest)

#random forest
set.seed(seed)
tunegrid <- expand.grid(.mtry=c(1:6), .ntree=c(200,500,1000,1500))
model.rf <- train(xTrain,yTrain, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)
model.rf$results
plot(model.rf)
ctreeVarImp = varImp(model.rf)
plot(ctreeVarImp)
predict.rf = predict(model.rf, newdata=xTest, type="raw") 
confusionMatrix(predict.rf,yTest)
CM_rf<-confusionMatrix(predict.rf,yTest)

#tree cart
set.seed(seed)
model.tc<-train(xTrain,yTrain, method="rpart", metric=metric, trControl=control)
model.tc$results
plot(model.tc)
ctreeVarImp = varImp(model.tc)
plot(ctreeVarImp)
predict.tc = predict(model.tc, newdata=xTest, type="raw") 
confusionMatrix(predict.tc,yTest)
CM_cart<-confusionMatrix(predict.tc,yTest)

#SVM Radial
#Tune for retrieve optimum cost and gamma
model.svm1<-train(xTrain,yTrain, method="svmRadial", tuneLenght=10,metric=metric, trControl=control)
model.svm1
grid <- expand.grid(sigma = c(.035, .036, .037),C = c(0.4, 0.5, 0.6))
model.svm1<-train(xTrain,yTrain, method="svmRadial", metric=metric, tuneGrid = grid,trControl=control)
model.svm1
model.svm1$results
plot(model)
predict.svm1 = predict(model, newdata=xTest, type="raw") 
confusionMatrix(predict.svm1,yTest)
CM_svm1<-confusionMatrix(predict.svm1,yTest)
ctreeVarImp = varImp(model.svm1)
plot(ctreeVarImp)

#SVM Lineal
#Tune for retrieve optimum cost and gamma
model.svm2<-train(xTrain,yTrain, method="svmLinear", tuneLenght=9,metric=metric, trControl=control)
model.svm2
grid <- expand.grid(C = c(0.9,0.95,1.0,1.05,1.1))
model.svm2<-train(xTrain,yTrain, method="svmLinear", metric=metric, tuneGrid = grid,trControl=control)
model.svm2
model.svm2$results
plot(model.svm2)
predict.svm2 = predict(model.svm2, newdata=xTest, type="raw") 
confusionMatrix(predict.svm2,yTest)
predict.svm2<-confusionMatrix(predict.svm2,yTest)
ctreeVarImp = varImp(model.svm2)
plot(ctreeVarImp)

# comparamos resultados
AllResults <- resamples (list ( NaiveBayes = model.nb,
                                LogisticRegression = model.lr,
                                GeneralizedBoostedModel = model.gb,
                                RandomForest = model.rf,
                                Cart = model.tc,
                                SVMRadial = model.svm1,
                                SVMLineal = model.svm2))

summary ( AllResults )
dotplot ( AllResults )
modelCor(AllResults)
