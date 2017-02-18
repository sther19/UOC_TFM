#####################################################################################################
#                                                                                                   #
#                            SENTIMENT ANALYSIS DATASET DESCRIPTION                                 #
#                                                                                                   #
# Script Description:  Analysis of Dataset of Tweeter Airline Sentiment + Delay Flights             #
#                      Time granularities Analysis                                                  #
#                                                                                                   #
# Tweet File Location: https://www.kaggle.com/crowdflower/twitter-airline-sentiment                 #
#                      github (Folder Data; File Tweets.csv)                                        #
#                                                                                                   #
# Delay File Location: http://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236&DB_Short_Name=On-Time
#                      github(Folder Data; File Delay_Airlines.csv)                                 #                                            #
#                                                                                                   #
# Author:              Esther Cordova                                                               #
# Last Update:         2017-02-06                                                                   #
#                                                                                                   #
#####################################################################################################

#*******************************************      Parameters   **************************************  
dir<-"C:/Users/esther.cordova/Desktop/Code_TFM/Files"       #Ptah where File is stored;To be changed
file<-"Tweets.csv"                                           #File Name
file2<-"Delay_Airlines.csv"
setwd(dir)
#****************************************************************************************************


#******************************************** Libraries *********************************************
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
if(!require(SnowballC)){
  install.packages('SnowballC',repos='http://cran.es.r-project.org')
  require(SnowballC)
}
if(!require(dplyr)){
  install.packages('dplyr',repos='http://cran.es.r-project.org')
  require(dplyr)
}
set.seed(606)
#********************************************************************************************************


#*****************************************  Tweet File Analysis   ***************************************
setwd(dir)
rawdata<-read.csv(file=file,sep=",",header = T)

#Exploratory Analysis of DataSet
cat(sprintf("\nLos campos que contine este dataset son los siguientes:\n%s",
            paste(colnames(rawdata),collapse="; ")))
cat(sprintf("Las dimensiones del dataset son: \n\t%i Filas;\n\t%i Columnas\n",
            dim(rawdata)[1],dim(rawdata)[2]))

#Frequency of each type of sentiments
x<-as.data.frame(table(rawdata$airline_sentiment))
y<-as.data.frame(round(100*prop.table(table(rawdata$airline_sentiment)),2))
FreqSentiments<-merge(x,y,by="Var1")
colnames(FreqSentiments)<-c("SentimentType","NumOccurences","Percentages")
Table1<-FreqSentiments

gbar = ggplot(FreqSentiments, aes(x = SentimentType, y = Percentages, fill = SentimentType))
plot1 = gbar + geom_bar(stat = 'identity') + ggtitle("Tweet Sentiment") + 
  theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1),
        axis.title.y = element_text(vjust = 2), axis.title.x = element_text(vjust = -1))+
  scale_fill_manual(values=c('#CC0000','#666666','#66A61E')) + 
  geom_text(aes(label=Percentages),vjust = -1, nudge_y = -0.8)


#Check the ratio of positive and negative tweets for each airline
x<-as.data.frame(table(rawdata$airline,rawdata$airline_sentiment))
y<-as.data.frame(round(100*prop.table(table(rawdata$airline,rawdata$airline_sentiment)),2))
FreqSentimentsAirline<-merge(x,y,by=c("Var1","Var2"))
colnames(FreqSentimentsAirline)<-c("Airline","SentimentType","NumOccurences","Percentages")                             
Table2<-FreqSentimentsAirline

gbar = ggplot(FreqSentimentsAirline, aes(x = Airline, y = Percentages, fill = SentimentType)) + 
  ggtitle('Proportion of Type of Tweets per Airline') +
  theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1), axis.title.x = element_text(vjust = -1))
plot2 = gbar + geom_bar(stat = 'identity',position = 'fill') +
  scale_fill_manual(values=c('#CC0000','#666666','#66A61E'))


#Obtenemos variable fecha, hora y dia de la semana
rawdata$weekday<-wday(as.Date(rawdata$tweet_created,format="%Y-%m-%d %H:%M:%S"),label=T)
rawdata$date<-as.Date(rawdata$tweet_created,format="%Y-%m-%d %H:%M:%S")
rawdata$hora<-substring(rawdata$tweet_created,12,13)


#Analizamos numero de occurrencias por dia y hora, por si algun dia no está completo
x<-as.data.frame(table(rawdata$date,rawdata$hora))
colnames(x)<-c("Date","Hour","Tweets")
x<-x[x$Tweets!=0,]
x$Date<-as.Date(x$Date,format="%Y-%m-%d")
plotDay<-ggplot(x, aes(x=Date, y=Hour)) + geom_point() + ggtitle('Tweets Stored per Day and Hour')


#Eliminamos 16 y 24 de Febrero pq no estan los dias completos
rawdata<-subset(rawdata,!(as.character(date) %in% c('2015-02-16','2015-02-24')))


#Check the days of the week with more tweets
x<-as.data.frame(table(rawdata$weekday))
y<-as.data.frame(round(100*prop.table(table(rawdata$weekday)),2))
FreqTweetWeekday<-merge(x,y,by=c("Var1"))
colnames(FreqTweetWeekday)<-c("Weekday","NumOccurences","Percentages")  
FreqTweetWeekday<-FreqTweetWeekday[order(FreqTweetWeekday$Weekday),]
Table3<-FreqTweetWeekday                      

gbar = ggplot(FreqTweetWeekday, aes(x = Weekday,y = Percentages,fill=Weekday)) + 
  ggtitle('Proportion of Tweets per WeekDay') +
  theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1), 
        axis.title.x = element_text(vjust = -1))
plot3 = gbar + geom_bar(stat = 'identity') + scale_fill_brewer()+geom_text(aes(label=Percentages),vjust=-0.5)


#Check the ratio of positive and negative tweets per day of the week
x<-as.data.frame(table(rawdata$weekday,rawdata$airline_sentiment))
y<-as.data.frame(round(100*prop.table(table(rawdata$weekday,rawdata$airline_sentiment)),2))
FreqSentimentsWeekday<-merge(x,y,by=c("Var1","Var2"))
colnames(FreqSentimentsWeekday)<-c("Weekday","SentimentType","NumOccurences","Percentages")                             
FreqSentimentsWeekday<-FreqSentimentsWeekday[order(FreqSentimentsWeekday$Weekday),]
Table4<-FreqSentimentsWeekday

gbar = ggplot(FreqSentimentsWeekday, aes(x = Weekday, y = Percentages, fill = SentimentType)) + 
  ggtitle('Proportion of Type of Tweets per WeekDay') +
  theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1), 
        axis.title.x = element_text(vjust = -1))
plot4 = gbar + geom_bar(stat = 'identity', position=position_dodge()) +
  scale_fill_manual(values=c('#CC0000','#666666','#66A61E'))


#Check the hours of the day with more tweets
x<-as.data.frame(table(rawdata$hora))
y<-as.data.frame(round(100*prop.table(table(rawdata$hora)),2))
FreqTweetHora<-merge(x,y,by=c("Var1"))
colnames(FreqTweetHora)<-c("Hour","NumOccurences","Percentages")  
FreqTweetHora<-FreqTweetHora[order(FreqTweetHora$Hour),]
Table5<-FreqTweetHora

gbar = ggplot(FreqTweetHora, aes(x = Hour,y = Percentages)) + 
  ggtitle('Proportion of Tweets per Hour') +
  theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1,hjust =0), 
        axis.title.x = element_text(vjust = -1))
plot5 = gbar + geom_bar(stat = 'identity',color="black", fill="steelblue") 


#Check the hours of the day with more tweets per type
x<-as.data.frame(table(rawdata$hora,rawdata$airline_sentiment))
y<-as.data.frame(round(100*prop.table(table(rawdata$hora,rawdata$airline_sentiment)),2))
FreqTweetHora<-merge(x,y,by=c("Var1","Var2"))
colnames(FreqTweetHora)<-c("Hour","SentimentType","NumOccurences","Percentages")  
FreqTweetHora<-FreqTweetHora[order(FreqTweetHora$Hour),]
Table6<-FreqTweetHora

gbar = ggplot(FreqTweetHora, aes(x = Hour, y = Percentages, group = SentimentType, colour=SentimentType)) + 
  ggtitle('Proportion of Type of Tweets per Hour') +
  theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1), 
        axis.title.x = element_text(vjust = -1))

plot6 = gbar + geom_point(size=3,fill="white") +
  scale_colour_manual(values=c('#CC0000','#666666','#66A61E'))+geom_smooth(method = 'loess')

#********************************************************************************************************


#*************************************  AirlineDelay File Analysis   ************************************
data<-read.csv(file2,header = T, sep = ",")
cat(sprintf("\nLos campos que contine este dataset son los siguientes:\n%s",
            paste(colnames(data),collapse="; ")))
cat(sprintf("Las dimensiones del dataset son: \n\t%i Filas;\n\t%i Columnas\n",
            dim(data)[1],dim(data)[2]))

#Preprocesado de Data
data2<-subset(data,CARRIER %in% c("AA","DL","VX","WN","UA","OH"))
data<-data2[,-c(1,2,3,7:9,11,14,18:25)]
data2<-subset(data,FL_DATE %in% factor(unique(rawdata$date)))
data<-data2
rm(data2)
data$weekday<-wday(as.Date(data$FL_DATE,format="%Y-%m-%d"),label=T)
data$DEP_TIME<-round((60*trunc(data$DEP_TIME/100)+100*((data$DEP_TIME/100)-trunc(data$DEP_TIME/100)))/60)
data$DEP_TIME<-ifelse(data$DEP_TIME==0,24,data$DEP_TIME)
data$OnTime<-ifelse((data$ARR_DEL15+data$DEP_DEL15)==2,"NO","YES")
data$OnTime2<-ifelse(data$CANCELLED==1,"CANCELLED",data$OnTime)
data$OnTime<-ifelse(data$DIVERTED==1,"DIVERTED",data$OnTime2)


#Flights delay
x<-as.data.frame(table(data$OnTime))
y<-as.data.frame(round(100*prop.table(table(data$OnTime)),2))
FreqDelay<-merge(x,y,by=c("Var1"))
colnames(FreqDelay)<-c("OnTime","NumOccurences","Percentages")  
Table7<-FreqDelay

gbar = ggplot(FreqDelay, aes(x = OnTime,y = Percentages)) + 
  ggtitle('On Time Flights Ratio') +
  theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1), 
        axis.title.x = element_text(vjust = -1))
plot7 = gbar + geom_bar(stat = 'identity',color="black", fill="steelblue") +
  geom_text(aes(label = Percentages),vjust=-0.5)


#Subset Flights NoOntime
NoOntime<-subset(data,OnTime!="YES")

#Number per DayDelay
x<-as.data.frame(table(NoOntime$weekday))
y<-as.data.frame(round(100*prop.table(table(NoOntime$weekday)),2))
DayDelay<-merge(x,y,by=c("Var1"))
colnames(DayDelay)<-c("Weekday","FlightsOffTime","Percentages")                             
DayDelay<-DayDelay[order(DayDelay$Weekday),]
Table8<-DayDelay

gbar = ggplot(DayDelay, aes(x = Weekday,y = Percentages,fill=Weekday)) + 
  ggtitle('Proportion of No OnTime Flights per WeekDay') +
  theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1), 
        axis.title.x = element_text(vjust = -1))
plot8 = gbar + geom_bar(stat = 'identity') + scale_fill_brewer() +
  geom_text(aes(label = FlightsOffTime),vjust=-0.7)


#Number per hour Delay
x<-as.data.frame(table(NoOntime$DEP_TIME))
y<-as.data.frame(round(100*prop.table(table(NoOntime$DEP_TIME)),2))
HourDelay<-merge(x,y,by=c("Var1"))
colnames(HourDelay)<-c("Hour","FlightsOffTime","Percentages")                             
HourDelay<-HourDelay[order(HourDelay$Hour),]
Table9<-HourDelay

gbar = ggplot(HourDelay, aes(x = as.numeric(Hour), y = Percentages)) + 
  ggtitle('Proportion of Flights No OnTime per Hour') + labs(x="Hour")+
  theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1), 
        axis.title.x = element_text(vjust = -1))
plot9 = gbar + geom_point(size=3,fill='#CC0000') +geom_smooth()

#********************************************************************************************************


#************************************  Tweet and Airline correlation ************************************
#Per day Analisis
#Merge negative tweets per day + no ontime flights and scale data to see correlation
y<-FreqSentimentsWeekday[FreqSentimentsWeekday$SentimentType == 'negative',c(1,3)]
merge<-merge(DayDelay,y, by=c("Weekday"))
merge$FlightsOffTime<-scale(as.numeric(merge$FlightsOffTime))
merge$NegativeTweets<-scale(as.numeric(merge$NumOccurences))

plot_correlation1<-ggplot(merge, aes(x=NegativeTweets, y=FlightsOffTime)) +
  ggtitle('Num No OnTime Flights vs Num Negative Tweets per WeekDay') +
  theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1))+
  geom_point(shape=1,size=3)+geom_text(aes(label = Weekday),vjust = 0, nudge_y = 0.05)+
  geom_smooth(data=subset(merge, Weekday !="Tues"), method='lm',formula=y~x,se=T)

#get fit data
fit<-lm(NegativeTweets~FlightsOffTime, data=merge[merge$Weekday!="Tues",])
summary(fit)


#Per hour Analisis
#Merge negative tweets per hour + no ontime flights and scale data to see correlation
x<-HourDelay[,c(1,2)]
colnames(x)<-c("Hora","Freq")
x$Hora<-as.numeric(x$Hora)
x$Group<-"FlightsOffTime"
y<-FreqTweetHora[FreqTweetHora$SentimentType == 'negative',c(1,3)]
colnames(y)<-c("Hora","Freq")
y$Hora<-as.numeric(y$Hora)
y$Group<-"NegativeTweets"

merge<-rbind(x,y)
gbar = ggplot(merge, aes(x = as.numeric(Hora), y = Freq, group = Group, colour=Group)) + 
  ggtitle('Num NegativeTwets and NoOntime Flights per Hour') + labs(x="Hour")+
  theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1), 
        axis.title.x = element_text(vjust = -1))
plot10 = gbar + geom_point(size=3,fill="white") +
  scale_colour_manual(values=c('#CC0000','#66A61E'))+geom_smooth(method = 'loess')

newdf1<-merge[merge$Group=="NegativeTweets",c(1,2)]
colnames(newdf1)[2]<-"NegativeTweets"
newdf2<-merge[merge$Group=="FlightsOffTime",c(1,2)]
colnames(newdf2)[2]<-"FlightsOffTime"
merge<-merge(newdf1,newdf2,by=c("Hora"))

plot_correlation2<-ggplot(merge, aes(x=NegativeTweets, y=FlightsOffTime)) +
  ggtitle('Num No OnTime Flights vs Num Negative Tweets per Hour') +
  theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1))+
  geom_point(shape=1,size=3)+geom_text(aes(label = Hora),vjust = -1, nudge_y = 0.8)+
  geom_smooth(data=merge, method='lm',formula=y~x,se=T)+
  geom_vline(xintercept = 420, lwd=1, lty = 'dashed')
#********************************************************************************************************


#************************************ See all the tables and graphs  ************************************
Table1
plot1
Table2
plot2
plotDay
Table3
plot3
Table4
plot4
Table5
plot5
Table6
plot6
Table7
plot7
Table8
plot8
Table9
plot9
plot_correlation1
plot10
plot_correlation2
#********************************************************************************************************

