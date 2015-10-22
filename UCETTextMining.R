# Require "tm" package
require(tm)

# Set working directory
setwd("~/Documents/MachineLearningProjects/UCETCodingResearch")

# Import data
separated.combined.data <- read.csv("Combined1and5.csv", stringsAsFactors=FALSE)

q1.results <- read.csv("Question1Results.csv", header=TRUE)
q1.results <- q1.results[,1:6]
q1.results <- q1.results[1:624,]

q1.responses <- separated.combined.data[1:624,]
q1.responses <- q1.responses[,-2]

require(plyr)
q1.results <- rename(q1.results, c("STUDENT."="Index"))


##############################  "Asynchronous Communication" Text Analysis  #################################
###
### Calculate word frequencies for responses labeled as "Asynchronous Communication" by UCET Coders.
### This will give an idea of what type of asynchronous communcation was a dissatisifier (email,
### discussion forums, etc.)

# Combine coding results with responses based by student number
q1.results.responses <- merge(q1.results, q1.responses, by="Index")

# Get only the rows that have "Asynchronous Communication"
q1.async.responses <- q1.results.responses[q1.results.responses$Variable1 == "Asynchronous Communication"
                                           | q1.results.responses$Variable2 == "Asynchronous Communication"
                                           | q1.results.responses$Variable3 == "Asynchronous Communication"
                                           | q1.results.responses$Variable4 == "Asynchronous Communication"
                                           | q1.results.responses$Variable5 == "Asynchronous Communication",]

# Combine all the responses together
q1.async.responses.only <- paste(q1.async.responses$Response, collapse=" ")

# Set up source and corpus
q1.async.source <- VectorSource(q1.async.responses.only)
q1.async.corpus <- Corpus(q1.async.source)

# Clean text data
q1.async.corpus <- tm_map(q1.async.corpus, content_transformer(tolower))
q1.async.corpus <- tm_map(q1.async.corpus, removePunctuation)
q1.async.corpus <- tm_map(q1.async.corpus, stripWhitespace)
q1.async.corpus <- tm_map(q1.async.corpus, removeWords, stopwords("english"))

# Make a document-term matrix
q1.async.dtm <- DocumentTermMatrix(q1.async.corpus)
q1.async.dtm2 <- as.matrix(q1.async.dtm)

# Find the most frequent terms
q1.async.freq <- colSums(q1.async.dtm2)
q1.async.freq <- sort(q1.async.freq, decreasing=TRUE)
q1.async.freq


##############################    The Satisfiers & Dissatisfiers Wordcloud    ################################
###
### Create a wordcloud of the most frequent words in the survey responses for Questions 1 and 5 combined
### 

# Combine all the responses together.
combined.data <- paste(separated.combined.data$Response, collapse=" ")

# Set up source and corpus (Google: "Computational Linguistics")
combined.data.source <- VectorSource(combined.data)
corpus <- Corpus(combined.data.source)

# Clean text data (the reason we're using "tm")
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# Make a document-term matrix
dtm <- DocumentTermMatrix(corpus)
dtm2 <- as.matrix(dtm)

# Find the most frequent terms
frequency <- colSums(dtm2)
frequency <- sort(frequency, decreasing=TRUE)
head(frequency)

#install.packages("wordcloud")
library(wordcloud)

words <- names(frequency)
wordcloud(words[1:100], frequency[1:100])


####################################     The Satisfiers Wordcloud     ######################################
###
### Create a wordcloud of the most frequent words in the survey responses for Questions 1 only.
### The Satisfiers
###

# Create a new dataframe made up of only rows corresponding to "Question 1"
question1.only.data <- separated.combined.data[which(separated.combined.data$Question == 1),]

# Combine all the responses together
question1.responses <- paste(question1.only.data$Response, collapse=" ")

# Set up source and corpus (Google: "Computational Linguistics")
combined.data.source2 <- VectorSource(question1.responses)
corpus2 <- Corpus(combined.data.source2)

# Clean text data (the reason we're using "tm")
corpus2 <- tm_map(corpus2, content_transformer(tolower))
corpus2 <- tm_map(corpus2, removePunctuation)
corpus2 <- tm_map(corpus2, stripWhitespace)
corpus2 <- tm_map(corpus2, removeWords, stopwords("english"))

# Make a document-term matrix
dtmTwo <- DocumentTermMatrix(corpus2)
dtm2.2 <- as.matrix(dtmTwo)

# Find the most frequent terms
frequency2 <- colSums(dtm2.2)
frequency2 <- sort(frequency2, decreasing=TRUE)
head(frequency2)

#install.packages("wordcloud")
require(wordcloud)

words2 <- names(frequency2)
wordcloud(words2[1:100], frequency2[1:100])


####################################    The Dissatisfiers Wordcloud    ######################################
###
### Create a wordcloud of the most frequent words in the survey responses for Questions 5 only.
### The Dissatisfiers
###

# Create a new dataframe made up of only rows corresponding to "Question 1"
question5.only.data <- separated.combined.data[which(separated.combined.data$Question == 5),]

# Combine all the responses together
question5.responses <- paste(question5.only.data$Response, collapse=" ")

# Set up source and corpus (Google: "Computational Linguistics")
combined.data.source3 <- VectorSource(question5.responses)
corpus3 <- Corpus(combined.data.source3)

# Clean text data (the reason we're using "tm")
corpus3 <- tm_map(corpus3, content_transformer(tolower))
corpus3 <- tm_map(corpus3, removePunctuation)
corpus3 <- tm_map(corpus3, stripWhitespace)
corpus3 <- tm_map(corpus3, removeWords, stopwords("english"))

# Make a document-term matrix
dtmThree <- DocumentTermMatrix(corpus3)
dtm3 <- as.matrix(dtmThree)

# Find the most frequent terms
frequency3 <- colSums(dtm3)
frequency3 <- sort(frequency3, decreasing=TRUE)
head(frequency3)

#install.packages("wordcloud")
require(wordcloud)

words3 <- names(frequency3)
wordcloud(words3[1:100], frequency3[1:100])


####################################    Compare Wordclouds    ################################################
###
### Show both wordclouds for satisfiers and dissatisfiers side by side for comparison. 
### Satisfiers Vs. Dissatisfiers
###

old.par <- par(mfrow=c(1, 2), mar=rep(7, 4))
wordcloud(words2[1:100], frequency2[1:100])
wordcloud(words3[1:100], frequency3[1:100])
par(old.par)


####################################  Find Differences in Frequencies  #######################################
###
### Find the words that have greatest difference in frequencies between Questions 1 and 5 .
### Satisfiers Vs. Dissatisfiers
###

### ----- Put the Satisfiers words and frequencies in a dataframe sorted alphabetically

compare.freq.df <- data.frame(frequency2)
compare.freq.df <- compare.freq.df[order(compare.freq.df$row.names),]




#############################################################################################################
### To remove everything created in this R Script use: 
rm(combined.data, combined.data.source, corpus, dtm, frequency, 
   i, q1.async.corpus, q1.async.dtm, q1.async.freq, q1.async.responses.only, 
   q1.async.source, wordsq1.async.dtm2, q1.async.responses, q1.responses, 
   q1.results, q1.results.responses, separated.combined.data, dtm2, dtm2.2,
   dtm3, q1.async.dtm2, question1.only.data, question5.only.data, corpus2, 
   corpus3, dtmThree, dtmTwo, frequency2, frequency3, old.layout, old.par,
   question1.responses, question5.responses, words, words2, words3,
   combined.data.source2, combined.data.source3, compare.freq.df)
