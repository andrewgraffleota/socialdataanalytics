#ASSIGNMENT  

#loading required libraries
library(SnowballC) #helps with text stemming getting words to word stem 
library(tm) #text mining package
library(syuzhet) #generates a score for each of the tweets for 10 diff emotions (sentiment processing)
library(wordcloud) #creates a world cloud of the commonly occuring words
dir_path<-"C:\\Users\\Student\\OneDrive - Whitireia and WelTec\\BIT 2023\\Documents\\Fuel Companies"
setwd(dir_path) #setting the above as our working directory - where all csv files are located
getwd() #get working directory, confirming the base of which we operate

#BP TWEETS=======================================================================

bptweets.df <- read.csv("bptweets.csv") #reading the bptweets file saved in Fuel Companies folder and converting into a new data frame/variable
head(bptweets.df) #head function listing the first few rows of our new data frame
head(bptweets.df$text) #we display the text field only

bptweets.df2 <- bptweets.df$text #converting the text only field to a new data frame 

head(bptweets.df2) #we see the new first few rows of text only data frame

#Converting text to readable string text to perform gsub operations
rmv_ivalid_char <- function(x){ iconv(x, "latin1", "ASCII", sub="")} #new functions to convert latin1 to acsii 
rmv_ivalid_char2 <- function(x){ iconv(x, "utf-8", "ASCII", sub="")} #new functions to convert utf-8 to acsii 

bptweets.df2 <- rmv_ivalid_char(bptweets.df2) #new remove invalid char function performed on tweets 
bptweets.df2 <- rmv_ivalid_char2(bptweets.df2) #repeated cycle of remove invalid char2 function on tweets 

head(bptweets.df2) #displays new converted text in order to now perform gsub operation 

#We remove URLs, hashtags, control codes, punctuation
#gsub meaning global substitution to find and replace so we can clean up tweets for text mining and sentiment analysis
bptweets.df2 <- gsub("\\s*<U\\+\\w+>","",bptweets.df2) #removing unwanted patterns <U+...> replaces with "" a way of clearing
bptweets.df2 <- gsub("http.*","",bptweets.df2) #removing http, URLs the star asterisk to remove the entire word to the right (depending on positioning)
bptweets.df2 <- gsub("https.*","",bptweets.df2) #removing https
bptweets.df2 <- gsub("[\\]+","",bptweets.df2) #removing \\ char
bptweets.df2 <- gsub("#.*","",bptweets.df2) #removing hashtags
bptweets.df2 <- gsub("*","",bptweets.df2) #removing asterisks
bptweets.df2 <- gsub("&amp","",bptweets.df2) #removing &amp 
bptweets.df2 <- gsub(";","",bptweets.df2) #removing semi colon
bptweets.df2 <- gsub("@\\w+","",bptweets.df2) #removing twitter handles and w+ meaning with words

head(bptweets.df2) #shows newly cleaned up bptweets.df2 for sentimental analysis

#BP SENTIMENTAL ANALYSIS========================================================

bpword.df <- as.vector(bptweets.df2) #to get sentiment we must convert dataframe to vector 
bpemotion.df <- get_nrc_sentiment(bpword.df) #now we're able to get scoring using nrc function
bpemotion.df2 <- cbind(bptweets.df2, bpemotion.df) #joining tweets (bptweets.df and bpemotion.df) to scoring using cbind function 
head(bpemotion.df2) #listing the first few scoring of tweets of the new bpemotion.df2 

sent.value <- get_sentiment(bpword.df) #extract sentiment scoring for each tweet 

most.emotionalcontent <- bpword.df[sent.value == max(sent.value)] #most positive sentiment (max = highest scoring)
most.emotionalcontent #most emotional content tweet

least.emotionalcontent <- bpword.df[sent.value <= min(sent.value)] #most negative sentiment (min = lowest scoring)
least.emotionalcontent #least emotional content tweet

positive.tweets <- bpword.df[sent.value > 0] #positive tweets given a value of more than 0
negative.tweets <- bpword.df[sent.value < 0] #negative tweets given a value of less than 0
neutral.tweets <- bpword.df[sent.value == 0] #neutral tweets given a value of equal than 0

head(positive.tweets) #lists first few rows of positive tweets
head(negative.tweets) #lists first few rows of negative tweets
head(neutral.tweets) #list of neutral tweets

#BP SENTIMENT BAR CHART================================================================================================================================

#sentiment analysis so we can see scoring with the highest emotional content in different categories
bpsentiment<-get_nrc_sentiment(bpword.df)

#collect sentiment score for each emotion
bpsentiment.anger =sum(bpsentiment$anger) #plotting values for each dataframe to use for bar chart
bpsentiment.anticipation =sum(bpsentiment$anticipation) #sum meaning total and $ variable for what emotional category
bpsentiment.disgust =sum(bpsentiment$disgust)
bpsentiment.fear =sum(bpsentiment$fear)
bpsentiment.joy =sum(bpsentiment$joy)
bpsentiment.sadness =sum(bpsentiment$sadness)
bpsentiment.surprise =sum(bpsentiment$surprise)
bpsentiment.trust =sum(bpsentiment$trust)
#creating the yaxis variables to plot bar graph
yAxis <- c(bpsentiment.anger,
           + bpsentiment.anticipation,
           + bpsentiment.disgust,
           + bpsentiment.fear,
           + bpsentiment.joy,
           + bpsentiment.sadness,
           + bpsentiment.surprise,
           + bpsentiment.trust)
#creating xaxis to plot labels for bar graph
xAxis <- c("Anger","Anticipation","Disgust","Fear","Joy", "Sadness","Surprise","Trust") #these are the labels
colors <- c("red","blue","orange","red","green","orange", "blue", "green")
yRange <- range(0,yAxis) #in accordance with
barplot(yAxis, names.arg = xAxis, #barplot function creating our bar graph
        xlab = "Sentiment Analysis", ylab = "Score", main = "Sentiment for BP Tweets", col = colors, border = "black", ylim = yRange, xpd = F, axisnames = T, cex.axis = 0.8, cex.sub = 0.8, col.sub = "blue")

#BP PIE CHART===================================================================

Positive <- length(positive.tweets) #counts number of positive tweets in new data frame 
Neutral <- length(neutral.tweets) #counts number of neutral tweets in new data frame
Negative <- length(negative.tweets) #counts number of negative tweets in new data frame

count <-c(Positive, Neutral, Negative) #adding above values into new data frame count to plot pie chart
labels <-c("Positive Tweets", "Neutral Tweets", "Negative Tweets") #naming the labels 
head(count) #can see the total numbers of each scored tweet category

pie(count, labels = count, main = "BP Twitter", col = rainbow(length(count))) #plotting pie chart. including count, labels, title and colour
legend("topright", legend = c ("Postive Tweets", "Neutral Tweets", "Negative Tweets"), fill = c("red", "blue", "green")) #adding a legend to indicate numbers on pie chart for detail

#BP GENERATE TDM===================================================================

bptweet_corpus <- Corpus(VectorSource(bpword.df)) #using the newly clean tweets in bpword.df we create a corpus

bptdm <- TermDocumentMatrix(bptweet_corpus, #applying some transformations for our term document matrix using corpus
                            control = list(removePunctuation = TRUE, wordLengths=c(5, 15), #removes punctuation and list words charcters 5-15 in length
                                           stopwords = c("thank", "bp",stopwords("english")), #stopwords that add no value to analysis, we find what to put here after finding freq commonly terms
                                           removeNumbers = TRUE, tolower = TRUE)) #remove numbers, and set to lower case
bptdm.matrix <- as.matrix(bptdm) #defining bptdm into a matrix to calculate word frequencies 
word_freqs <- sort(rowSums(bptdm.matrix), decreasing=FALSE) #creating word_freqs variable that counts the words in bptdm.matrix in decreasing order
ordr <- order(word_freqs, decreasing=TRUE) #creating a sort order for the above variable, notice decreasing true so most common terms first
word_freqs[head(ordr)] #we can see first six most commonly occuring terms

findAssocs(bptdm,"sorry",0.25) #from above results we use most freq words and find word associations with tweet
findAssocs(bptdm,"thanks",0.3) #correlation figures vary to find good range of associations 
findAssocs(bptdm,"energy",0.3)

bpdm <- data.frame(word=names(word_freqs), freq=word_freqs) #create data frame with words and their frequencies

#generate wordcloud of bpdm word and bpdm freq of the words, with adjustments of colour and sort 
wordcloud(bpdm$word, bpdm$freq, max.freq = 50, #minimum frequency 50 (only words showing at least 50 times)
          random.order=FALSE, colors=brewer.pal(8, "Dark2")) #mixes the order of words and colours


#MOBIL TWEETS====================================================================

#reading csv from our current working directory
mobiltweets.df <- read.csv("mobiltweets.csv") 
head(mobiltweets.df)
head(mobiltweets.df$text)

#new mobiletweets dataframe of text only
mobiltweets.df2 <- mobiltweets.df$text
head(mobiltweets.df2)

#Converting text to readable string text to perform operations
rmv_ivalid_char <- function(x){ iconv(x, "latin1", "ASCII", sub="")}
rmv_ivalid_char2 <- function(x){ iconv(x, "utf-8", "ASCII", sub="")}

#removing invalid characters using the above function
mobiltweets.df2 <- rmv_ivalid_char(mobiltweets.df2)
mobiltweets.df2 <- rmv_ivalid_char2(mobiltweets.df2)

#preprocessing stage cleaning up text to perform text mining analysis 

mobiltweets.df2 <- gsub("\\s*<U\\+\\w+>","",mobiltweets.df2)
mobiltweets.df2 <- gsub("http.*","",mobiltweets.df2)
mobiltweets.df2 <- gsub("[\\]+","",mobiltweets.df2)
mobiltweets.df2 <- gsub("#","",mobiltweets.df2)
mobiltweets.df2 <- gsub("*","",mobiltweets.df2)
mobiltweets.df2 <- gsub("https.*","",mobiltweets.df2)
mobiltweets.df2 <- gsub("&amp","and",mobiltweets.df2)
mobiltweets.df2 <- gsub(";","",mobiltweets.df2)
mobiltweets.df2 <- gsub("w/!","",mobiltweets.df2)
mobiltweets.df2 <- gsub("[\n\n]", "", mobiltweets.df2)
mobiltweets.df2 <- gsub("[[:punct:]]", "", mobiltweets.df2) #removing punctuation
mobiltweets.df2 <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", mobiltweets.df2) #removing RT|via other control codes

#we can see the first few rows of the newly cleaned text for sentimental analysis 
head(mobiltweets.df2)

#MOBIL SENTIMENTAL ANALYSIS=====================================================

mobilword.df <- as.vector(mobiltweets.df2)
mobilemotion.df <- get_nrc_sentiment(mobilword.df)
mobilemotion.df2 <- cbind(mobiltweets.df2, mobilemotion.df) 
head(mobilemotion.df2)

sent.value <- get_sentiment(mobilword.df)
#highest scoring terms 
most.emotionalcontent <- mobilword.df[sent.value == max(sent.value)]
most.emotionalcontent

least.emotionalcontent <- mobilword.df[sent.value <= min(sent.value)] 
least.emotionalcontent 

#getting scoring values and inputting into new data frames
positive.tweets <- mobilword.df[sent.value > 0]
negative.tweets <- mobilword.df[sent.value < 0]
neutral.tweets <- mobilword.df[sent.value == 0]

#see all the most commonly scored tweets in each category 
head(positive.tweets)
head(negative.tweets)
head(neutral.tweets)

#MOBIL SENTIMENT BAR CHART======================================================

# Perform sentiment analysis so we can see scoring with the highest emotional content in different categories
mobilsentiment<-get_nrc_sentiment(mobilword.df)

# Get the sentiment score for each emotion
mobilsentiment.anger =sum(mobilsentiment$anger)
mobilsentiment.anticipation =sum(mobilsentiment$anticipation)
mobilsentiment.disgust =sum(mobilsentiment$disgust)
mobilsentiment.fear =sum(mobilsentiment$fear)
mobilsentiment.joy =sum(mobilsentiment$joy)
mobilsentiment.sadness =sum(mobilsentiment$sadness)
mobilsentiment.surprise =sum(mobilsentiment$surprise)
mobilsentiment.trust =sum(mobilsentiment$trust)

# Create the bar chart
yAxis <- c(mobilsentiment.anger,
           + mobilsentiment.anticipation,
           + mobilsentiment.disgust,
           + mobilsentiment.fear,
           + mobilsentiment.joy,
           + mobilsentiment.sadness,
           + mobilsentiment.surprise,
           + mobilsentiment.trust)

xAxis <- c("Anger","Anticipation","Disgust","Fear","Joy", "Sadness","Surprise","Trust")

colors <- c("red","blue","orange","red","green","orange", "blue", "green")

yRange <- range(0,yAxis) 

barplot(yAxis, names.arg = xAxis, 
        xlab = "Sentiment Analysis", ylab = "Score", main = "Sentiment for Mobil Tweets", col = colors, border = "black", ylim = yRange, xpd = F, axisnames = T, cex.axis = 0.8, cex.sub = 0.8, col.sub = "blue")


#MOBIL PIE CHART================================================================

#creating new dataframes to plot data for pie chart 
Positive <- length(positive.tweets)
Neutral <- length(neutral.tweets)
Negative <- length(negative.tweets)

#couting how many so we can how many tweets per category
count <- c(Positive, Neutral, Negative)
labels <- c("Positive Tweets", "Neutral Tweets", "Negative Tweets")
head(count) #can inspect the numbers of tweets 

#creating pie chart with given dataframes 
pie(count, labels = count, main = "Mobil Twitter", col = rainbow(length(count)))
legend("topright", legend = c ("Positive Tweets", "Neutral Tweets", "Negative Tweets"), fill = c("red", "blue", "green"))

#MOBILE TDM AND WORD CLOUD======================================================

#creating a corpus to create tdm thus a word cloud
mobiltweet_corpus <- Corpus(VectorSource(mobilword.df))
mobiltdm <- TermDocumentMatrix(mobiltweet_corpus,
                               control = list(removePunctuation = TRUE, wordLengths=c(5, 15),
                                              stopwords = c("mobil",stopwords("english")),
                                              removeNumbers = TRUE, tolower = TRUE))
mobiltdm.matrix <- as.matrix(mobiltdm)
word_freqs <- sort(rowSums(mobiltdm.matrix), decreasing=FALSE) 
ordr <- order(word_freqs, decreasing=TRUE) 
word_freqs[head(ordr)]

findAssocs(mobiltdm,"energy",0.2)
findAssocs(mobiltdm,"emissions",0.3)
findAssocs(mobiltdm,"technology",0.3)

mobildm <- data.frame(word=names(word_freqs), freq=word_freqs)
wordcloud(mobildm$word, mobildm$freq, max.freq = 50, #create word cloud of words appearing at least 50 times
          random.order=FALSE, colors=brewer.pal(8, "Dark2"))

#Z ENERGY TWEETS=================================================================

zenergytweets.df <- read.csv("zenergytweets.csv")
head(zenergytweets.df)
head(zenergytweets.df$text)
zenergytweets.df2 <- zenergytweets.df$text

head(zenergytweets.df2)

rmv_ivalid_char <- function(x){ iconv(x, "latin1", "ASCII", sub="")}
rmv_ivalid_char2 <- function(x){ iconv(x, "utf-8", "ASCII", sub="")}
zenergytweets.df2 <- rmv_ivalid_char(zenergytweets.df2)
zenergytweets.df2 <- rmv_ivalid_char2(zenergytweets.df2)

zenergytweets.df2 <- gsub("\\s*<U\\+\\w+>","",zenergytweets.df2)
zenergytweets.df2 <- gsub("http.*","",zenergytweets.df2)
zenergytweets.df2 <- gsub("[\\]+","",zenergytweets.df2)
zenergytweets.df2 <- gsub("#.*","",zenergytweets.df2)
zenergytweets.df2 <- gsub("*","",zenergytweets.df2)
zenergytweets.df2 <- gsub("https.*","",zenergytweets.df2)
zenergytweets.df2 <- gsub("&amp","and",zenergytweets.df2)
zenergytweets.df2 <- gsub(";","",zenergytweets.df2)
zenergytweets.df2 <- gsub("@\\w+","",zenergytweets.df2)
zenergytweets.df2 <- gsub("&gt","",zenergytweets.df2)
zenergytweets.df2 <- gsub("[[:punct:]]", "", zenergytweets.df2)
zenergytweets.df2 <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", zenergytweets.df2)
zenergytweets.df2 <- gsub("http\\w+", "", zenergytweets.df2)

#cleaned preprocessed tweets

head(zenergytweets.df2)

#Z ENERGY SENTIMENTAL ANALYSIS=====================================================

zenergyword.df <- as.vector(zenergytweets.df2)
zenergyemotion.df <- get_nrc_sentiment(zenergyword.df)
zenergyemotion.df2 <- cbind(zenergytweets.df2, zenergyemotion.df) 
head(zenergyemotion.df2)

sent.value <- get_sentiment(zenergyword.df)

most.sentimental <- zenergyword.df[sent.value == max(sent.value)]
most.sentimental
least.sentimental <- zenergyword.df[sent.value <= min(sent.value)] 
least.sentimental 

positive.tweets <- zenergyword.df[sent.value > 0]
negative.tweets <- zenergyword.df[sent.value < 0]
neutral.tweets <- zenergyword.df[sent.value == 0]

head(positive.tweets)
head(negative.tweets)
head(neutral.tweets)

# Perform sentiment analysis so we can see scoring with the highest emotional content in different categories
zenergysentiment<-get_nrc_sentiment(zenergyword.df)

# Get the sentiment score for each emotion
zenergysentiment.anger =sum(zenergysentiment$anger)
zenergysentiment.anticipation =sum(zenergysentiment$anticipation)
zenergysentiment.disgust =sum(zenergysentiment$disgust)
zenergysentiment.fear =sum(zenergysentiment$fear)
zenergysentiment.joy =sum(zenergysentiment$joy)
zenergysentiment.sadness =sum(zenergysentiment$sadness)
zenergysentiment.surprise =sum(zenergysentiment$surprise)
zenergysentiment.trust =sum(zenergysentiment$trust)

# Create the bar chart
yAxis <- c(zenergysentiment.anger,
           + zenergysentiment.anticipation,
           + zenergysentiment.disgust,
           + zenergysentiment.fear,
           + zenergysentiment.joy,
           + zenergysentiment.sadness,
           + zenergysentiment.surprise,
           + zenergysentiment.trust)

xAxis <- c("Anger","Anticipation","Disgust","Fear","Joy", "Sadness","Surprise","Trust")

colors <- c("red","blue","orange","red","green","orange", "blue", "green")

yRange <- range(0,yAxis) 

barplot(yAxis, names.arg = xAxis, 
        xlab = "Sentiment Analysis", ylab = "Score", main = "Sentiment for ZEnergy Tweets", col = colors, border = "black", ylim = yRange, xpd = F, axisnames = T, cex.axis = 0.8, cex.sub = 0.8, col.sub = "blue")

#Z ENERGY PIECHART==============================================================

Positive <- length(positive.tweets)
Neutral <- length(neutral.tweets)
Negative <- length(negative.tweets)
count <- c(Positive, Neutral, Negative)
labels <- c("Positive Tweets", "Neutral Tweets", "Negative Tweets")
head(count)
pie(count, labels = count, main = "Zenergy Twitter", col = rainbow(length(count)))
legend("topright", legend = c ("Postive Tweets", "Neutral Tweets", "Negative Tweets"), fill = c("red", "blue", "green"))

#Z ENERGY TDM AND WORD CLOUD===================================================================

zenergytweet_corpus <- Corpus(VectorSource(zenergyword.df))

zenergytdm <- TermDocumentMatrix(zenergytweet_corpus,
                                 control = list(removePunctuation = TRUE, wordLengths=c(5, 15),
                                                stopwords = c("Zenergy","youre",stopwords("english")),
                                                removeNumbers = TRUE, tolower = TRUE))
zenergytdm.matrix <- as.matrix(zenergytdm)
word_freqs <- sort(rowSums(zenergytdm.matrix), decreasing=FALSE) 
ordr <- order(word_freqs, decreasing=TRUE) 
word_freqs[head(ordr)]

findAssocs(zenergytdm,"thanks",0.25)
findAssocs(zenergytdm,"sorry",0.25)
findAssocs(zenergytdm,"message",0.25)

zenergydm <- data.frame(word=names(word_freqs), freq=word_freqs)
wordcloud(zenergydm$word, zenergydm$freq, max.freq = 50, 
          random.order=FALSE, colors=brewer.pal(8, "Dark2"))

#=================================================================================================


