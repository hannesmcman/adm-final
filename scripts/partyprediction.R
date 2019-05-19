###############################################################################
# Program: Congress Twitter Exploration
# Research Questions:
# Can we pedict what party someone is based on the structure/style of their tweets?
# Is there a relationship between a congressperson's tweet activity
# (frequency, favorites, followers) and their legislative productivity
# (cosponsorships, votes for passed legislation)?
# Author: Ben Westermeyer
###############################################################################

library(rtweet)
library(httpuv)
library(readr)
library(purrr)
library(dplyr)
library(randomForest)
library(textfeatures)
library(cluster)
library(factoextra)
library(ggrepel)
library(class)
library(ranger)
library(rjson)
library(FNN)
library(gbm)
library(doBy)
library(tidyr)
library(adabag)

###############################################################################
# SET UP
###############################################################################

congress.df <- read_csv("Desktop/ADM R/Final Project/legislators-current.csv")
congress.df <- congress.df[,c("govtrack_id","full_name","party","twitter")]
# Remove everyone without a twitter
congress.df <- filter(congress.df,!is.na(twitter))
# Brownley's twitter handle has changed
congress.df$twitter <- ifelse(congress.df$twitter=="JuliaBrownley26","RepBrownley",congress.df$twitter)
# Rob Bishop has never tweeted, so we should remove him
congress.df <- congress.df[congress.df$twitter!="RepRobBishop",]
# same with Roger Marshall
congress.df <- congress.df[congress.df$twitter!="RepMarshall",]



###############################################################################
# RESEARCH QUESTION 1: PREDICTING PARTY BASED ON TWEET STRUCTURE
###############################################################################

# Get the 100 most recent tweets from
# each of the 529 congressmembers with active twitters
#timeline <- get_timelines(congress.df$twitter, n = 100,check=FALSE)

# Make sure we didn't lose any congressmembers in the query process
#screen_names <- c(unique(timeline$screen_name))
#compare <- cbind.data.frame(congress.df$twitter,screen_names)

# Save the dataset created by the query so we don't have to run it every time
#saveRDS(timeline,file="Desktop/ADM R/Final Project/congress_timeline.rds")
# Now read it in
timeline <- readRDS(file="Desktop/ADM R/Final Project/congress_timeline.rds")

# Get text features
# note: word_dims creates vectors
# which are numerical representations of word features
# such as the context of individual words
#text_features <- textfeatures(timeline$text,normalize = TRUE,word_dims=50)
#saveRDS(text_features,file="Desktop/ADM R/Final Project/congress_text_features.rds")
text_features <- readRDS(file="Desktop/ADM R/Final Project/congress_text_features.rds")

# Combine info we got from get_timeline with info we got from textfeatures
#names(timeline)
#timeline.numeric <- dplyr::select_if(timeline,is.numeric)
#timeline.numeric <- data.frame(scale(timeline.numeric))
#timeline.numeric <- cbind(handle=timeline$screen_name,timeline.numeric)
#features.df <- cbind(timeline.numeric, textfeatures.numeric)
features.df <- cbind(twitter=timeline$screen_name,text_features)
#names(features.df)

# Now we need to bring back in the party variable from the congress dataset

# Let's see if the screen_names from timeline query match the names from congress dataset
first <- as.character(congress.df$twitter)
second <- as.character(unique(features.df$twitter))
compare <- cbind.data.frame(first,second)
mismatch <- compare[first!=second,]
# looks like 55 twitter handles don't match because of case sensitivity

# We need to replace the twitter column in congress.df with the screen_names
# from the timeline dataset
congress.df$twitter <- second
# now it should match the timeline screen names
sum(congress.df$twitter!=as.character(unique(timeline$screen_name)))

# now we can merge
congressfeatures.df <- merge(congress.df,features.df,by="twitter")

# Change party variable to binary
# Democrat and will be 0 
# Ind will also be 0 (Sen. Sanders and Sen. King both caucus with the democrats)
# Republicans will be 1
congressfeatures.df$partyind <- ifelse(congressfeatures.df$party=="Republican",1,0)
congressfeatures.df$party <- congressfeatures.df$partyind
congressfeatures.df <- select(congressfeatures.df,-partyind)

# we don't want twitter handles and names as predictors
congressfeatures.df <- select(congressfeatures.df,-c(govtrack_id,twitter,full_name))

# create train and test sets
# Note: I do not do cross validatoin for any of the following methods because
# the data is too large to do CV in a reasonable amount of time

party.N <- nrow(congressfeatures.df)
party.train <- sample(1:party.N,party.N/2,rep=F)
party.train.df <- congressfeatures.df[party.train,]
party.test.df <- congressfeatures.df[-party.train,]

# LOGISTIC REGRESSION
party.logmod <- glm(party~.,family="binomial",data=party.train.df)
party.logprobs <- predict(party.logmod,newdata=party.test.df,type="response")
party.logpreds <- ifelse(party.logprobs>0.5,1,0)
    
# confusion matrix
with(party.test.df,table(party, party.logpreds))
(party.logerr <- with(party.test.df,mean(party!=party.logpreds)))

# RANDOM FOREST
party.rfmod <- ranger(party~.,
                num.trees=300,
                data=party.train.df)

party.rfpredinfo <- predict(party.rfmod,data=party.test.df)
party.rfprobs <- party.rfpredinfo$predictions
party.rfpreds <- ifelse(party.rfprobs>0.5,1,0)

# confusion matrix
with(party.test.df,table(party, party.rfpreds))
(party.rferr <- mean(party.test.df$party!=party.rfpreds))

# BOOSTING
party.gbmmod <- gbm(party ~ ., data=party.train.df,
                n.trees=300,
                distribution="bernoulli",
                #distribution="adaboost",
                interaction.depth = 2,
                shrinkage=0.1)
             

party.gbmprobs <- predict(party.gbmmod,newdata=party.test.df,n.trees=300,type="response")
party.gbmpreds <- ifelse(party.gbmprobs > 0.5,1,0)

with(party.test.df,table(party, party.gbmpreds))
(party.gbmerr <- mean(party.test.df$party!=party.gbmpreds))

#Recreate a variable importance plot using ggplot.
gbmImp <- varImp(party.gbmmod, scale = TRUE, numTrees=300)
impvals <- as.vector(gbmImp$Overall)
varnames <- as.vector(row.names(gbmImp))
gbminfo <- cbind(Variable=varnames,Importance=as.numeric(impvals))
gbminfo <- as.data.frame(gbminfo)
gbminfo$Variable <- as.character(gbminfo$Variable)
gbminfo$Importance <- as.character(gbminfo$Importance)
gbminfo$Importance <- as.numeric(gbminfo$Importance)
gbminfo <- gbminfo[order(gbminfo$Importance,decreasing=TRUE),]
gbminfo <- gbminfo[1:30,]

ggplot(data=gbminfo, aes(x=Variable,y=Importance)) +
  geom_bar(stat="identity") + 
  coord_flip() +
  scale_x_discrete(
    limits=gbminfo$Variable,
    labels=gbminfo$Variable 
  )+
  ggtitle("Variable Importance for Predicting Congressperson's Party Based on Tweets")+
  xlab("Variables")+
  ylab("Relative Importance")

# The number of lower case letters is the predictor most useful 
# in trying to determine which party a congressperson belongs to based on their tweets

c(Logistic=party.logerr,RandomForest=party.rferr,Boosting=party.gbmerr)

# all agorithms have very similar mileage, but RF performs best.
# It is very interesting that we are able to predict with some success
# the party of a congressperson just based on the style and structure of their tweets
# This indicates that democrats and republicans really do have some
# fundamental differences in the way that they communicate with their bases
