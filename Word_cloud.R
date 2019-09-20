#load data
library(pdftools)
data <- pdf_text("nc.pdf")


library(tm)
options(header=F,stringsAsFactors = F,FileEncoding="latin1")

#make corpus
corpus <- Corpus(VectorSource(data))

#data cleaning
corpus <- tm_map(corpus,tolower)
corpus <- tm_map(corpus,removePunctuation)
corpus <- tm_map(corpus,removeNumbers)
corpus <- tm_map(corpus,removeWords,stopwords("english"))
remove_specials <- function(x) gsub("\r?\n|\r"," ",x)
corpus <- tm_map(corpus,content_transformer(remove_specials))
corpus <- tm_map(corpus,stripWhitespace)
#finally clean text
cleantxt <- tm_map(corpus,PlainTextDocument)
cleantxt <- Corpus(VectorSource(cleantxt))
#term document matrix
tdm <- TermDocumentMatrix(cleantxt,control=list(minWordLength=c(1,Inf)))
word_mat <- as.matrix(tdm)
#inspecting frequency words
findFreqTerms(tdm,lowfreq = 10)

#Bar plot
term_freq <- rowSums(word_mat)
term_freq <- subset(term_freq,term_freq>=7)
library(ggplot2)
barplot(term_freq,las=2,col=rainbow(20))
#if any unwanted word comes use tm_map(corpus,removeWords,c("","",""))

#word cloud
library(wordcloud)
wordFreq <- sort(rowSums(word_mat),decreasing = T)

grayLevel <- gray((wordFreq+10)/(max(wordFreq)+10))

#1st word cloud with gray level
wordcloud(words=names(wordFreq),freq=wordFreq,min.freq=5,random.order = F,color=grayLevel)
#2nd with colors
wordcloud(words=names(wordFreq),freq=wordFreq,min.freq=5,random.order = F,color=brewer.pal(11,"Paired"))

library(wordcloud2)
df <- data.frame(names(wordFreq),wordFreq)
colnames(df) <- c("Word","Frequency")
head(df)
#remove all non utf-8 characters
df$Word <- iconv(df$Word,from="UTF-8",to="UTF-8",sub="")
wordcloud2(df,size=0.8,shape="star")

#sentiments analysis
library(syuzhet)
library(reshape2)
library(dplyr)
library(scales)
library(ggplot2)
library(lubridate)

sentiment_score <- get_nrc_sentiment(as.character(cleantxt))
#my pdf is alone so it takes all the text data in the 1st row
sentiment_score <- sentiment_score[1,]
sentiment_score

#bar plot
barplot(colSums(sentiment_score),las=2,ylab="Score",col=rainbow(10))
