#####################################################################################################
#                                                                                                   #
#                                TWEET CONTENT ANALISIS & CLUSTERING                                #
#                                                                                                   #
# Script Description:  Analysis of Tweet content via clustering and Corpus                          #
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
if(!require(scales)){
  install.packages('scales',repos='http://cran.es.r-project.org')
  require(scales)
}
if(!require(ggplot2)){
  install.packages('ggplot2',repos='http://cran.es.r-project.org')
  require(ggplot2)
} 
if(!require(lubridate)){
  install.packages('lubridate',repos='http://cran.es.r-project.org')
  require(lubridate)
} 
if(!require(zoo)){
  install.packages('zoo',repos='http://cran.es.r-project.org')
  require(zoo)
} 
if(!require(tm)){
  install.packages('tm',repos='http://cran.es.r-project.org')
  require(tm)
}
if(!require(SnowballC)){
  install.packages('SnowballC',repos='http://cran.es.r-project.org')
  require(SnowballC)
}
if(!require(606)){
  install.packages('606',repos='http://cran.es.r-project.org')
  require(606)
}
if(!require(slam)){
  install.packages('slam',repos='http://cran.es.r-project.org')
  require(slam)
}
if(!require(wordcloud)){
  install.packages('wordcloud',repos='http://cran.es.r-project.org')
  require(wordcloud)
}
if(!require(e1071)){
  install.packages('e1071',repos='http://cran.es.r-project.org')
  require(e1071)
}
if(!require(fpc)){
  install.packages('fpc',repos='http://cran.es.r-project.org')
  require(fpc)
}
if(!require(cluster)){
  install.packages('cluster',repos='http://cran.es.r-project.org')
  require(cluster)
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
#***************************************************************************************************


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

#lenght of tweets
lengh_plot<-ggplot(rawdata, aes(x = text_length, 
                                fill = airline_sentiment)) + 
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = c('#EC7063','#99A3A4','#73C6B6')) +
  labs(x = 'Tweet Length') +  theme(text = element_text(size=12))

#word frequency and build cloud of words for each sentiment
smalldf<-rawdata[,c('airline_sentiment','airline','negativereason','text')]
NegativeTweets<-smalldf[smalldf$airline_sentiment=='negative',]
PositiveTweets<-smalldf[smalldf$airline_sentiment=='positive',]

neg_word = analyseText(NegativeTweets$text)
freqWords_neg = colSums(neg_word)
freqWords_neg = freqWords_neg[order(freqWords_neg, decreasing = T)]
pos_word = analyseText(PositiveTweets$text)
freqWords_pos = colSums(pos_word)
freqWords_pos = freqWords_pos[order(freqWords_pos, decreasing = T)]
freqWords_pos[1] = freqWords_pos[1] + freqWords_pos[2]
freqWords_pos = freqWords_pos[-2]

Positive<-as.matrix(freqWords_pos)
colnames(Positive)<-"Positive"
Negative<-as.matrix(freqWords_neg)
colnames(Negative)<-"Negative"

#Plot WordCloud
par(mfrow=c(1,2))
wordcloud(rownames(Negative), Negative, min.freq =3, scale=c(5, .2), 
          random.order = FALSE, random.color = FALSE, colors = brewer.pal(9, 'Reds')[4:9])
wordcloud(rownames(Positive), Positive, min.freq =3, scale=c(5, .2), 
          random.order = FALSE, random.color = FALSE, colors= brewer.pal(9, 'Blues')[4:9])


# kmeans to determine the proximity of words
# calculate the distance of each negative word
distance_neg <- dist(t(as.matrix(neg_word)), method = 'euclidean')
kmodel <- kmeans(distance_neg, 3)
clusters<-as.data.frame(as.matrix(kmodel$cluster))
clusters$Cluster<-row.names(clusters)
row.names(clusters)<-NULL
group1<-paste(clusters[clusters$V1==1,2],collapse="; ")
group2<-paste(clusters[clusters$V1==2,2],collapse="; ")
group3<-paste(clusters[clusters$V1==3,2],collapse="; ")
cat(sprintf("Cluster1:\n%s\n",group1))
cat(sprintf("Cluster2:\n%s\n",group2))
cat(sprintf("Cluster3:\n%s\n",group3))

# calculate the distance of each positive word
distance_pos <- dist(t(as.matrix(pos_word)), method = 'euclidean')
kmodel <- kmeans(distance_pos, 3)
clusters<-as.data.frame(as.matrix(kmodel$cluster))
clusters$Cluster<-row.names(clusters)
row.names(clusters)<-NULL
group1<-paste(clusters[clusters$V1==1,2],collapse="; ")
group2<-paste(clusters[clusters$V1==2,2],collapse="; ")
group3<-paste(clusters[clusters$V1==3,2],collapse="; ")
cat(sprintf("Cluster1:\n%s\n",group1))
cat(sprintf("Cluster2:\n%s\n",group2))
cat(sprintf("Cluster3:\n%s\n",group3))

# hierarchical clustering
fit = hclust(d = distance_neg, method = 'ward.D')
#fancy plot
plot(fit, col = "#EC7063", col.main = "#EC7063",main = 'Negative Sentiment', xlab = '',
     col.axis = "#F38630", lwd = 3, lty = 1, sub = "", hang = -1, axes = FALSE) +
  axis(side = 2, at = seq(0, 400, 100), labels = FALSE, 
       lwd = 2) +
  mtext(seq(0, 100, 10), side = 2, at = seq(0, 100, 10), line = 1, 
        col = "#A38630", las = 2)


# hierarchical clustering
fit = hclust(d = distance_pos, method = 'ward.D')
#fancy plot
plot(fit, col = "#066303", col.main = "#066303",main = 'Positive Sentiment', xlab = '',
     col.axis = "#F38630", lwd = 3, lty = 1, sub = "", hang = -1, axes = FALSE)
axis(side = 2, at = seq(0, 400, 100), labels = FALSE, 
     lwd = 2)
mtext(seq(0, 100, 10), side = 2, at = seq(0, 100, 10), line = 1, 
      col = "#A38630", las = 2)
