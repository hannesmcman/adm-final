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

###############################################################################
# RESEARCH QUESTION 2: PREDICTING LEGISLATIVE PRODUCTIVITY
# BASED ON TWEET FREQUENCY AND POPULARITY
###############################################################################

# Legislative records from https://www.govtrack.us/data/analysis/by-congress/116/
sponsorshipanalysis_s <- read_csv("Desktop/ADM R/Final Project/sponsorshipanalysis_s.txt")
sponsorshipanalysis_h <- read_csv("Desktop/ADM R/Final Project/sponsorshipanalysis_h.txt")
sponsorship.df <- rbind(sponsorshipanalysis_s,sponsorshipanalysis_h)

#Leadership score is computed for each Member of Congress 
#by looking at how often other Members of Congress cosponsor their bills

#profiles <- get_timelines(congress.df$twitter, n = 1,check=FALSE)
#saveRDS(profiles,file="Desktop/ADM R/Final Project/congress_profiles.rds")
# Now read it in
profiles <- readRDS(file="Desktop/ADM R/Final Project/congress_profiles.rds")

profiles$tweetfreq <- 
  as.numeric(profiles$statuses_count)/as.numeric(profiles$created_at-profiles$account_created_at)

profiles <- profiles[,c("screen_name","followers_count","tweetfreq")]

# Now let's get average RT and favorite variables from recent tweets
timeline <- as.data.frame(timeline)
rtsummary <- summaryBy(cbind(favorite_count,retweet_count)~screen_name,data=timeline,FUN=mean)

tweetinfo <- merge(x=rtsummary,y=profiles,by="screen_name")

# Get govtrack ID from congress dataset
moreinfo <- merge(x=congress.df,y=tweetinfo,by.x="twitter",by.y="screen_name")

# Finally, merge in the productivity info
productivity.df <- merge(x=moreinfo,y=sponsorship.df,by.x="govtrack_id",by.y="ID")

#productivity.df <- productivity.df[,-c(1:4,10:12)]
mod1 <- lm(introduced_bills_116~tweetfreq+favorite_count.mean+retweet_count.mean+followers_count,data=productivity.df)
summary(mod1)
preds <- predict(mod1,newdata=productivity.df)
mean((productivity.df$introduced_bills_116-preds)^2)


N <- nrow(productivity.df)
train <- sample(1:N,N/2,rep=F)
train.df <- productivity.df[train,]
test.df <- productivity.df[-train,]

train.dat <- train.df[c(5:8)] 
ytrain <- train.df$introduced_bills_116
test.dat <- test.df[c(5:8)] 
ytest <- test.df$introduced_bills_116

knnregmod <- knn.reg(train.dat,test.dat,ytrain,k=3)
mean((ytest-knnregmod$pred)^2)


# Random Forest
rfmod <- ranger(introduced_bills_116~
                  tweetfreq+favorite_count.mean+retweet_count.mean+followers_count,
                data=train.df)
predinfo <- predict(rfmod,data=test.df)
preds <- predinfo$predictions

sqrt(mean((test.df$introduced_bills_116-preds)^2))
sd(test.df$introduced_bills_116)

# Random forest on average will predict a value
# within one standard deviation of the correct value



productivity.df %>%
ggplot()+
  geom_point(aes(x=tweetfreq,y=introduced_bills_116))



gbm.legmod <- gbm(introduced_bills_116 ~ tweetfreq+favorite_count.mean+retweet_count.mean+followers_count, 
              data=train.df,
              n.trees=300,
              distribution="gaussian",
              interaction.depth = 2,
              shrinkage=0.1)

gbmprobs <- predict(gbm.legmod,newdata=test.df,n.trees=300,type="response")
gbmpreds <-  ifelse(gbmprobs > 0.5,1,0)

(gbmerr <- mean((test.df$introduced_bills_116-gbmpreds)^2))


