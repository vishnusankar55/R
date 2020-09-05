  ###########Word Cloud #######
install.packages("tm")
install.packages("wordcloud")
install.packages("RColorBrewer")
library(tm)
library(wordcloud)
#data= file.choose()
setwd("C:/Users/Asus/Desktop/Priyanka/Simpli learn/June_July_2020/Day5_4July")
getwd()
data=read.csv("Word_cloud_Day4.csv",header = T,stringsAsFactors = F)
str(data)
##convert into data frame
data=data.frame(data)
View(data)
#top 6 entries in ur data
head(data)
#bottom 6 entries
tail(data)

colnames(data) <- c("doc_id", "text") 
View(data)
#Cleaning the data
Word1=Corpus(DataframeSource(data))
head(Word1)  ###wrong command
#corpus represents a collection of (data) texts, typically labeled with text annotations
?Corpus
Word1[[1]][1]

Corpus = tm_map(Word1, content_transformer(tolower))
Corpus_1 = tm_map(Corpus, removeNumbers)

Corpus_2 = tm_map(Corpus_1, removeWords,stopwords("english"))

Corpus_3 = tm_map(Corpus_2, removePunctuation)

Corpus = tm_map(Corpus_3,stripWhitespace)

Corpus[[1]][1]
#Creating a TDM
# A document-term matrix or term-document matrix is a mathematical 
# matrix that describes the frequency of terms that occur in a collection of documents.
tdm =TermDocumentMatrix(Corpus)
m=as.matrix(tdm)
m
v=sort(rowSums(m),decreasing = TRUE)
d = data.frame(word=names(v), freq=v)
View(d)

#par(mar=c(1,1,1,1)) 
dev.off()
wordcloud(words=d$word,freq=d$freq,min.freq=1,max.words=100,random.order=FALSE,
          rot.per=0.5,colors=brewer.pal(8,"Dark2"))
warnings()
d$word
d$freq
?wordcloud

#########################ggplot2######example

install.packages("ggplot2")
library(ggplot2)

df <- data.frame(dose=c("D0.5", "D1", "D2"),
                 len=c(4.2, 10, 29.5))
View(df)

?ggplot2::geom_bar 
p<-ggplot(df, aes(x=dose, y=len)) +
  geom_bar(stat="identity", color="blue",fill="white")+ theme_minimal()   
p
p+coord_flip()      

#### Density Plot #######
df1 <- data.frame(
  sex=factor(rep(c("F", "M"), each=200)),
  weight=round(c(rnorm(200, mean=55, sd=5),
                 rnorm(200, mean=65, sd=5))))
View(df1)

p1 <- ggplot(df1, aes(x=weight)) +  geom_density()
p1

# Add mean line
p1+ geom_vline(aes(xintercept=mean(weight)),
               color="blue", linetype="dashed", size=1)
###If we have 2 categorys in the data

p3= ggplot(df1, aes(x=weight, color=sex)) +
  geom_density()
p3
####Histogram####
data("airquality")
View(airquality)

ggplot(airquality,aes(x = Ozone))  + geom_histogram (aes (y = ..count..),
                           binwidth= 5, colour = "black", fill = "blue")
+ scale_x_continuous(name = "Mean in ozone in \nparts per billion",
                     breaks = seq(0, 175, 25),
                     limits=c(175))+ scale_y_continuous(name = "count")


############## Boxplot#######
data("airquality")
str(airquality)
airquality$Month<- factor(airquality$Month , labels = c("May", "Jun", "Jul", "Aug","Sep"))
airquality$Month

ggplot (airquality, aes (x = Month, y= Ozone))+ geom_boxplot (fill = "blue", colour = "black")
+ scale_y_continuous (name = "Mean ozone in \nparts per billion",
                      breaks = seq (0, 175, 25), limits=c(0, 175))
+ scale_x_discrete (name = "Month")
+ ggtitle("Boxplot of mean ozone by month")


####pRACTISE : Example 2 : Wordcloud
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

# Read the text file from internet
filePath <- "http://www.sthda.com/sthda/RDoc/example-files/martin-luther-king-i-have-a-dream-speech.txt"
text <- readLines(filePath)
# Load the data as a corpus
docs <- Corpus(VectorSource(text))
inspect(docs)
##ansformation is performed using tm_map() function to replace, 
#for example, special characters from the text.
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

#Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

#Document matrix is a table containing the frequency of the words. Column names are words and row names are documents.
#The function TermDocumentMatrix() from text mining package can be used as follow
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)


#set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colorPalette = "black")

#kindly download the ggplot2 cheatsheet
#https://github.com/rstudio/cheatsheets/blob/master/data-visualization-2.1.pdf
