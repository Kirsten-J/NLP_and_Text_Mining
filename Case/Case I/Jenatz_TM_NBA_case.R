setwd("C:/Users/kirst/OneDrive - Hult Students/HULT/NLP and Text Mining/Text-Mining-NLP/Case/Case I/Data")

library(readr)
library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(stringi)
library(tm)
library(gt)
library(qdap)
library(wordcloud)
library(RColorBrewer)
library(textclean)
library(ggdendro)

library(syuzhet)
library(scales)
library(reshape2)

# To limit errors please run this code
Sys.setlocale('LC_ALL','C')
Sys.setenv(LANG = "en_US.UTF-8")
Sys.setenv('R_MAX_VSIZE'=32000000000)

#FUNCTIONS 

#basic cleaning of text
basicSubs <- function(x){
  x <- gsub('http\\S+\\s*', '', x)
  x <- gsub('(RT|via)((?:\\b\\W*@\\w+)+):', '', x)
  x <- iconv(x, "UTF-8", "UTF-8", "byte")
  #x <- gsub('@.*', "", x)
  x <- tolower(x)
  return(x)
}

cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url)) 
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}


# Bigram token maker
bigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
}


#reading all files into one
temp <- list.files(pattern="*0|1.csv")
nike <- do.call(rbind, lapply(temp, read_csv))


#separating date data into more columns for easier manipulation
nike <- nike %>% mutate(year = year(created), month = month(created, label = T), day = day(created))


nike$txt <- basicSubs(nike$text)


#write.csv(nike,'Nike.csv')

nike <- read_csv("Nike.csv")

length(unique(nike$text)) # some tweets repeat <- they must mention various teams
length(unique(nike$txt)) # some tweets seem to say the same thing but may have different retweets or links, etc. 


### TEAM BY TEAM MENTIONS 

#count mentions by team
teams <- nike %>% group_by(team) %>% summarise(n = n()) %>% arrange(desc(n)) %>% head(10)

# Chg to factor for ggplot
teams$team <- factor(teams$team, 
                     levels=unique(as.character(teams$team))) 

ggplot(teams, aes(x=team, y=n)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=n), colour="white",hjust=1.25, size=3.0)

teams_month <- nike %>% group_by(team, month) %>% summarise(n = n()) %>% filter(month == "Mar") %>% arrange(n) %>% head(10)

# Chg to factor for ggplot
teams_month$team <- factor(teams_month$team, 
                           levels=unique(as.character(teams_month$team))) 
teams_month$month <- factor(teams_month$month, 
                            levels=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))

ggplot(teams_month, aes(x=month, y=n, fill = team)) + 
  geom_bar(position = position_dodge2(padding = 0.2), stat = 'identity') +
  coord_flip() 

## have to make teams a specific color to make it make sense

####### CONFERENCES ANALYSIS

#choosing best teams by conference: https://www.espn.com/nba/standings/_/sort/winpercent/dir/desc
# top 5 based on 2020 standings
eastern <- c("Milwaukee Bucks", "Toronto Raptors", "Boston Celtics", "Indiana Pacers", "Miami Heat")
western <- c("Los Angeles Lakers", "Los Angeles Clippers", "Denver Nuggets", "Houston Rockets", "Oklahoma City Thunder")
eastern_data <- nike %>% filter(team %in% eastern)
western_data <- nike %>% filter(team %in% western)

eastern_data$trial <- replace_emoji(eastern_data$txt)
western_data$trial <- replace_emoji(western_data$txt)

nike %>% filter(team == "Houston Rockets") %>% select(txt)
#write.csv(eastern_data,'Eastern_data.csv')
#write.csv(western_data,'Western_data.csv')

eastern_data <- read_csv('Eastern_data.csv')
western_data <- read_csv('Western_data.csv')

#Masai Ujuri is president of Toronto Raptors 

### EASTERN CONFERENCE
#words to remove from eastern data
stops_eastern <- c(stopwords('SMART'), stopwords('English'), "milwaukee", 'ago', 'today', 'nba','game', 'team', "bucks", "toronto", "raptors", "boston", "celtics", "indiana", "pacers", "miami", "heat")

#random sampling because it is way too big to work with TDM and matrix
eastern_idx <- sample(1:nrow(eastern_data), size = round(0.02*nrow(eastern_data),0)) #0.02 for the single tokens, # 0.005 for bigram
eastern_data_subset <- eastern_data[eastern_idx,]

# Apply the VCorpus Function to a VectorSource of the original text object
eastern_cleanTxt <- VCorpus(VectorSource(eastern_data_subset$trial))
# Clean the Corpus with the cleanCorpus function, this will take a few seconds
eastern_cleanTxt <- cleanCorpus(eastern_cleanTxt, stops_eastern)

# Construct a TDM in an object 
easternTDM <- TermDocumentMatrix(eastern_cleanTxt,control=list(tokenize=bigramTokens)) # FOR THE Word Frequency table

################################## stop running it
# Construct a TDM in an object 
easternTDM <- TermDocumentMatrix(eastern_cleanTxt,control=list(weighting=weightTf)) # for the word cloud 

# Switch this to a simple matrix 
easternTDMm <- as.data.frame(as.matrix(easternTDM))

easternTDMv <- sort(rowSums(easternTDMm), decreasing = TRUE)
easternDF   <- data.frame(word = names(easternTDMv), freq = easternTDMv)
rownames(easternDF) <- NULL

#write.csv(easternDF, 'easternDF_bigram.csv')
#write.csv(easternDF, 'easternDF_single.csv')
easternDF_bigram <- read_csv('easternDF_bigram.csv')
easternDF_single <- read_csv('easternDF_single.csv')

#WORD CLOUDS
# Choose a color & drop light ones
pal <- brewer.pal(8, "Blues")
pal <- pal[-(1:2)]


# Make simple word cloud
# Reminder to expand device pane
set.seed(1234)
wordcloud(easternDF_single$word,
          easternDF_single$freq,
          max.words    = 50,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))


#top words
# Simple barplot; values greater than 15
topWords_eastern      <- subset(easternDF_bigram, easternDF_bigram$freq >= 45) #600 for single tokens
topWords_eastern      <- topWords_eastern[order(topWords_eastern$freq, decreasing=T),]

# Chg to factor for ggplot
topWords_eastern$word <- factor(topWords_eastern$word, 
                                levels=unique(as.character(topWords_eastern$word))) 

ggplot(topWords_eastern, aes(x=word, y=freq)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=freq), colour="white",hjust=1.25, size=3.0)


#Dendrogram
# Reduce TDM
easternTDM_reduced <- removeSparseTerms(easternTDM, sparse=0.995) #shoot for ~50 terms; 1.5% of cells in row have a value  
easternTDM_reduced

# Organize the smaller TDM
easternTDMm_reduced <- as.data.frame(as.matrix(easternTDM_reduced))

# Basic Hierarchical Clustering
hc <- hclust(dist(easternTDMm_reduced))
plot(hc,yaxt='n')

ggdendrogram(hc, rotate=FALSE) 


#Sentiment Analysis
#eastern_sentiment <- get_nrc_sentiment(eastern_data_subset$txt) #commenting because it takes for ever
#write.csv(eastern_sentiment, 'eastern_sentiment.csv')

eastern_sentiment <- read_csv('eastern_sentiment.csv')
eastern_sentiment$...1 <- NULL
barplot(colSums(eastern_sentiment),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores Eastern Conference Tweets')




### WESTERN CONFERENCE
#words to remove from western data
stops_western <- c(stopwords('SMART'), stopwords('English'), 'nba', 'game', 'team', 'ago', 'today', "lakers", "clippers", 'la', 'los angeles', "denver", "nuggets", "houston", "rockets", "oklahoma city", "thunder")


#random sampling because it is way too big to work with TDM and matrix
western_idx <- sample(1:nrow(western_data), size = round(0.03*nrow(western_data),0)) #0.04 for the single tokens
western_data_subset <- western_data[western_idx,]

# Apply the VCorpus Function to a VectorSource of the original text object
western_cleanTxt <- VCorpus(VectorSource(western_data_subset$trial))
# Clean the Corpus with the cleanCorpus function, this will take a few seconds
western_cleanTxt <- cleanCorpus(western_cleanTxt, stops_western)

# Construct a TDM in an object 
westernTDM <- TermDocumentMatrix(western_cleanTxt,control=list(tokenize=bigramTokens)) # FOR THE Word Frequency table

################################## stop running it
# Construct a TDM in an object 
westernTDM <- TermDocumentMatrix(western_cleanTxt,control=list(weighting=weightTf)) # for the word cloud 

# Switch this to a simple matrix 
westernTDMm <- as.data.frame(as.matrix(westernTDM))

westernTDMv <- sort(rowSums(westernTDMm), decreasing = TRUE)
westernDF   <- data.frame(word = names(westernTDMv), freq = westernTDMv)
rownames(westernDF) <- NULL

#write.csv(westernDF, 'westernDF_bigram.csv')
#write.csv(westernDF, 'westernDF_single.csv')
westernDF_bigram <- read_csv('westernDF_bigram.csv')
westernDF_single <- read_csv('westernDF_single.csv')


#WORD CLOUDS
# Make simple word cloud
wordcloud(westernDF_single$word,
          westernDF_single$freq,
          max.words    = 50,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))


# Simple barplot; values greater than 15
topWords_western      <- subset(westernDF_bigram, westernDF_bigram$freq >= 130) #600 for single tokens
topWords_western      <- topWords_western[order(topWords_western$freq, decreasing=T),]

# Chg to factor for ggplot
topWords_western$word <- factor(topWords_western$word, 
                                levels=unique(as.character(topWords_western$word))) 

ggplot(topWords_western, aes(x=word, y=freq)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=freq), colour="white",hjust=1.25, size=3.0)


#Dendrogram
# Reduce TDM
westernTDM_reduced <- removeSparseTerms(westernTDM, sparse=0.995) #shoot for ~50 terms; 1.5% of cells in row have a value  
westernTDM_reduced

# Organize the smaller TDM
westernTDMm_reduced <- as.data.frame(as.matrix(westernTDM_reduced))

# Basic Hierarchical Clustering
hc <- hclust(dist(westernTDMm_reduced))
plot(hc,yaxt='n')

ggdendrogram(hc, rotate=FALSE) 

#Sentiment Analysis
#western_sentiment <- get_nrc_sentiment(western_data_subset$txt) #commenting because it kills my rstudio
western_sentiment <- read_csv('western_sentiment.csv')
western_sentiment$...1 <- NULL
barplot(colSums(western_sentiment),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores Western Conference Tweets')


#write.csv(western_sentiment, 'western_sentiment.csv')



## COMONALITY EAST VS WEST
library(pbapply)
# Another way to extract the cleaned text 
eastern_clean <- unlist(pblapply(eastern_cleanTxt, content))
eastern_clean <- paste(eastern_clean, collapse = ' ')
western_clean <- unlist(pblapply(western_cleanTxt, content))
western_clean <- paste(western_clean, collapse = ' ')


# Combine the subject documents into a corpus of *2* documents
allConferences <- c(eastern_clean, western_clean)
allConferences <- VCorpus((VectorSource(allConferences)))

# Make TDM
ConferencesTDM  <- TermDocumentMatrix(allConferences)
ConferencesTDMm <- as.matrix(ConferencesTDM)

# Make sure order is correct!
colnames(ConferencesTDMm) <- c('eastern', 'western')


commonality.cloud(ConferencesTDMm, 
                  max.words=150, 
                  random.order=FALSE,
                  colors='blue',
                  scale=c(3.5,0.25))

# Make comparison cloud
comparison.cloud(ConferencesTDMm, 
                 max.words=75, 
                 random.order=FALSE,
                 title.size=0.5,
                 colors=c('red','blue'),
                 scale=c(3,0.1))



####### NIKE vs ADIDAS vs UNDERARMOR
nike_mentions <- nike %>% filter(grepl("nike",txt))
adidas_mentions <- nike %>% filter(grepl("adidas",txt))
underarmour_mentions <- nike %>% filter(grepl("under armour",txt))
#write_csv(nike_mentions, 'nike_mentions.csv')
#write_csv(adidas_mentions, 'adidas_mentions.csv')
#write_csv(underarmour_mentions, 'underarmour_mentions.csv')
nike_mentions <- read_csv('nike_mentions.csv')
adidas_mentions <- read_csv('adidas_mentions.csv')
underarmour_mentions <- read_csv('underarmour_mentions.csv')

nike_mentions[50:60,'txt']
adidas_mentions[30:40,'txt']

#Sentiment Analysis Nike

nike_mentions_sentiment <- get_nrc_sentiment(nike_mentions$txt)
#adidas_mentions$txt <- str_replace_all(adidas_mentions$txt,"[^[:graph:]]", " ") 
adidas_mentions_sentiment <- get_nrc_sentiment(adidas_mentions$txt)
underarmour_mentions_sentiment <- get_nrc_sentiment(underarmour_mentions$txt)#commenting because it kills my rstudio


#write_csv(nike_mentions_sentiment, 'nike_mentions_sentiment.csv')
#write_csv(adidas_mentions_sentiment, 'adidas_mentions_sentiment.csv')
#write_csv(underarmour_mentions_sentiment, 'underarmour_mentions_sentiment.csv')

nike_mentions_sentiment <- read_csv('nike_mentions_sentiment.csv')
adidas_mentions_sentiment <- read_csv('adidas_mentions_sentiment.csv')
underarmour_mentions_sentiment <- read_csv('underarmour_mentions_sentiment.csv')

barplot(colSums(nike_mentions_sentiment),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores Nike Tweets')
barplot(colSums(adidas_mentions_sentiment),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores Adidas Tweets')
barplot(colSums(underarmour_mentions_sentiment),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores Underarmour Tweets')


######### NIKE VS ADIDAS SPONSORSHIPS

nike_data <- nike %>% filter(grepl("lebron|giannis|antetokounmpo|kawhi|kingjames",txt))
adidas_data <- nike %>% filter(grepl("damian|lillard|harden|steph|curry",txt))

nike_data$trial <- replace_emoji(nike_data$txt)
adidas_data$trial <- replace_emoji(adidas_data$txt)

#write.csv(nike_data,'Nike_sponsors.csv')
#write.csv(adidas_data,'Adidas_sponsors.csv')

nike_data <- read_csv('Nike_sponsors.csv')
adidas_data <- read_csv('Adidas_sponsors.csv')

nike_data[70:80,'txt']
adidas_data[70:80,'txt']
underarmour_mentions[1:10,]

########## NIKE SPONSORSHIPS ANALYSIS

#words to remove from nike data
stops_nike_sponsors <- c(stopwords('SMART'), stopwords('English'), 'lebron', 'fire', 'giannis', 'kawhi', 'james', 'lakers', 'bucks', 'milwaukee', 'toronto', 'antetokounmpo', 'clippers', 'leonard', 'miami', 'raptors', 'angeles', 'cleveland', 'golden', 'cavaliers', 'nba', 'game', 'heat')

#random sampling because it is way too big to work with TDM and matrix
nike_idx <- sample(1:nrow(nike_data), size = round(0.06*nrow(nike_data),0)) # 0.06 for single # 0.03 for bigram
nike_data_subset <- nike_data[nike_idx,]
# Apply the VCorpus Function to a VectorSource of the original text object
nike_cleanTxt <- VCorpus(VectorSource(nike_data_subset$trial))
# Clean the Corpus with the cleanCorpus function, this will take a few seconds
nike_cleanTxt <- cleanCorpus(nike_cleanTxt, stops_nike_sponsors)

# Construct a TDM in an object 
nikeTDM <- TermDocumentMatrix(nike_cleanTxt,control=list(tokenize=bigramTokens)) # FOR THE Word Frequency table


###############
# Construct a TDM in an object 
nikeTDM <- TermDocumentMatrix(nike_cleanTxt,control=list(weighting=weightTf))

# Switch this to a simple matrix still
nikeTDMm <- as.data.frame(as.matrix(nikeTDM))

nikeTDMv <- sort(rowSums(nikeTDMm), decreasing = TRUE)
nikeDF   <- data.frame(word = names(nikeTDMv), freq = nikeTDMv)
rownames(nikeDF) <- NULL

#write.csv(nikeDF, 'nikeDF_bigram.csv')
#write.csv(nikeDF, 'nikeDF_single.csv')
nikeDF_bigram <- read_csv('nikeDF_bigram.csv')
nikeDF_single <- read_csv('nikeDF_single.csv')

#WORD CLOUDS
# Make simple word cloud
wordcloud(nikeDF_single$word,
          nikeDF_single$freq,
          max.words    = 50,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))

# Simple barplot; values greater than 15
topWords_nike <- subset(nikeDF_bigram, nikeDF_bigram$freq >= 80) #600 for single tokens
topWords_nike <- topWords_nike[order(topWords_nike$freq, decreasing=T),]

# Chg to factor for ggplot
topWords_nike$word <- factor(topWords_nike$word, 
                             levels=unique(as.character(topWords_nike$word))) 

ggplot(topWords_nike, aes(x=word, y=freq)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=freq), colour="white",hjust=1.25, size=3.0)


#Sentiment Analysis
#nike_data_subset$txt <- str_replace_all(nike_data_subset$txt,"[^[:graph:]]", " ") 
#nike_sentiment <- get_nrc_sentiment(nike_data_subset$txt) #commenting because it kills my rstudio
#write.csv(nike_sentiment, 'nike_sentiment.csv')
nike_sentiment <- read_csv('nike_sentiment.csv')
nike_sentiment$...1 <- NULL
barplot(colSums(nike_sentiment),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores Nike Sponsorships Tweets')









########### ADIDAS SPONSORSHIP ANALYSIS


#words to remove from adidas data
stops_adidas_sponsors <- c(stopwords('SMART'), stopwords('English'), 'james', 'nba', 'game', 'damian', 'harden', 'state','blazers','trail','portland','damelillard', 'houston', 'curry', 'pts', 'rockets', 'warriors', 'golden', 'stephen', 'steph', 'lillard')

#random sampling because it is way too big to work with TDM and matrix
adidas_idx <- sample(1:nrow(adidas_data), size = round(0.09*nrow(adidas_data),0)) #0.05 for bigram # 0.09 for single 
adidas_data_subset <- adidas_data[adidas_idx,]
# Apply the VCorpus Function to a VectorSource of the original text object
adidas_cleanTxt <- VCorpus(VectorSource(adidas_data_subset$trial))
# Clean the Corpus with the cleanCorpus function, this will take a few seconds
adidas_cleanTxt <- cleanCorpus(adidas_cleanTxt, stops_adidas_sponsors)

# Construct a TDM in an object 
adidasTDM <- TermDocumentMatrix(adidas_cleanTxt,control=list(tokenize=bigramTokens)) # FOR THE Word Frequency table


################

# Construct a TDM in an object 
adidasTDM <- TermDocumentMatrix(adidas_cleanTxt,control=list(weighting=weightTf))

# Switch this to a simple matrix still 
adidasTDMm <- as.data.frame(as.matrix(adidasTDM))

adidasTDMv <- sort(rowSums(adidasTDMm), decreasing = TRUE)
adidasDF   <- data.frame(word = names(adidasTDMv), freq = adidasTDMv)
rownames(adidasDF) <- NULL

#write.csv(adidasDF, 'adidasDF_bigram.csv')
#write.csv(adidasDF, 'adidasDF_single.csv')
adidasDF_bigram <- read_csv('adidasDF_bigram.csv')
adidasDF_single <- read_csv('adidasDF_single.csv')

#WORD CLOUDS
# Make simple word cloud
# Reminder to expand device pane
wordcloud(adidasDF_single$word,
          adidasDF_single$freq,
          max.words    = 50,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))

# Simple barplot; values greater than 15
topWords_adidas <- subset(adidasDF_bigram, adidasDF_bigram$freq >= 100) #600 for single tokens
topWords_adidas <- topWords_adidas[order(topWords_adidas$freq, decreasing=T),]

# Chg to factor for ggplot
topWords_adidas$word <- factor(topWords_adidas$word, 
                               levels=unique(as.character(topWords_adidas$word))) 

ggplot(topWords_adidas, aes(x=word, y=freq)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=freq), colour="white",hjust=1.25, size=3.0)



#Sentiment Analysis
#adidas_data_subset$txt <- str_replace_all(adidas_data_subset$txt,"[^[:graph:]]", " ") 
#adidas_sentiment <- get_nrc_sentiment(adidas_data_subset$txt) #commenting because it kills my rstudio
#write.csv(adidas_sentiment, 'adidas_sentiment.csv')
adidas_sentiment <- read_csv('adidas_sentiment.csv')
adidas_sentiment$...1 <- NULL
barplot(colSums(adidas_sentiment),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores Adidas Sponsorships Tweets')


###### Interesting links

#https://www.businesswire.com/news/home/20211027005392/en/UNO%C2%AE-Teams-Up-With-Nike-and-Giannis-for-a-Wild-Product-Collection <- nike x uno x giannis card game 
#https://sports.yahoo.com/luka-doncic-reportedly-getting-jordan-163203344.html luka doncic nike shoe after 2019 nov tweet <- interesting fact
## info about both conferences https://basketballword.com/differences-east-west-nba/


