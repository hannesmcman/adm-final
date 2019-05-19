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
# RESEARCH QUESTION 2: PREDICTING LEGISLATIVE PRODUCTIVITY
# BASED ON TWEET FREQUENCY AND POPULARITY
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

timeline <- readRDS(file="cache/congress_timeline.rds")
text_features <- readRDS(file="cache/congress_text_features.rds")
features.df <- cbind(twitter=timeline$screen_name,text_features)

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

# Legislative records from https://www.govtrack.us/data/analysis/by-congress/116/
sponsorshipanalysis_s <- read_csv("data/sponsorshipanalysis_s.txt")
sponsorshipanalysis_h <- read_csv("data/sponsorshipanalysis_h.txt")
sponsorship.df <- rbind(sponsorshipanalysis_s,sponsorshipanalysis_h)

#Leadership score is computed for each Member of Congress 
#by looking at how often other Members of Congress cosponsor their bills

#profiles <- get_timelines(congress.df$twitter, n = 1,check=FALSE)
#saveRDS(profiles,file="Desktop/ADM R/Final Project/congress_profiles.rds")
# Now read it in
profiles <- readRDS(file="cache/congress_profiles.rds")

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
productivity.df <- productivity.df[,c(5:8,14)]
#productivity.df <- data.frame(scale(productivity.df))

colnames(productivity.df) <- 
  c("Average_Favorite_Count","Average_Retweet_Count","Followers","Tweet_Frequency","Bills_Introduced")

# KNN

knnMSE <- function(k,numFolds){
  
  N<-nrow(productivity.df)
  folds<-sample(1:numFolds,N,rep=T)
  
  mseKFold<-numeric(numFolds)
  
  for(fold in 1:numFolds){
    
    train.dat  <- productivity.df[folds != fold,1:4]
    train.y <- productivity.df$Bills_Introduced[folds != fold]
    test.dat   <- productivity.df[folds == fold,1:4]
    test.y <- productivity.df$Bills_Introduced[folds == fold]
    
    mod.cv  <- knn.reg(train.dat,test.dat,train.y,k=k)
    
    #test.df$pred <- predict(mod.cv,newdata=test.df)  
    mseKFold[fold] <- mean((test.y-mod.cv$pred)^2)
    
  }
  mse.kfold <- mean(mseKFold)
}

# Try k values from 1 to 50
knnmses <- numeric(50)
for(i in 1:50){
  knnmses[i] <- knnMSE(i,10)
}

# report the best MSE and the k value used to get it
product.knnerr <- min(knnmses)
c(k=which.min(knnmses),MSE=product.knnerr)


# RANDOM FOREST
rfMSE <- function(mtry,numTrees,numFolds){
  
  N<-nrow(productivity.df)
  folds<-sample(1:numFolds,N,rep=T)
  
  mseKFold<-numeric(numFolds)
  
  for(fold in 1:numFolds){
    
    train.df  <- productivity.df[folds != fold,]
    test.df   <- productivity.df[folds == fold,]
    
    mod.cv  <- 
      ranger(Bills_Introduced~Average_Favorite_Count+Average_Retweet_Count+Followers+Tweet_Frequency,
                 num.trees = numTrees,
                 mtry=mtry,
                 importance="impurity",
                 data=train.df)
    
    predinfo <- predict(mod.cv,data=test.df)
    preds <- predinfo$predictions
    err <- with(test.df,mean((Bills_Introduced-preds)^2))
    
    mseKFold[fold] <- err
    
  }
  mse.kfold <- mean(mseKFold)
}

mvals <- seq(1,4)
treevals <- seq(100,300,50)
grid <- expand.grid(mvals,treevals)

rfmses <- numeric(nrow(grid))
for(i in 1:nrow(grid)){
  m <- grid[i,1]
  trees <- grid[i,2]
  mse <- rfMSE(m,trees,10)
  rfmses[i] <- mse
}

# report the best MSE and the parameters used to get it
product.rferr <- min(rfmses)
bestmtry <- grid[which.min(rfmses),1]
bestnumtrees <- grid[which.min(rfmses),2]
c(mtry=bestmtry,numtrees=bestnumtrees,MSE=product.rferr)

# Let's create a model using the best parameters and look at a variable importance plot

train <- sample(1:nrow(productivity.df),nrow(productivity.df)/2,replace = F)
product.train.df <- productivity.df[train,]
product.test.df <- productivity.df[-train,]

product.rfmod <- ranger(Bills_Introduced~
                Average_Favorite_Count+Average_Retweet_Count+Followers+Tweet_Frequency,
                mtry <- bestmtry,
                num.trees = bestnumtrees,
                importance="impurity",
                data=product.train.df)

varimp <- as.data.frame(product.rfmod$variable.importance)
colnames(varimp) <- "Importance"
varimp$Variable <- rownames(varimp)

ggplot(varimp, aes(x=reorder(Variable,Importance), y=Importance,fill=Importance))+ 
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Variable Importance")+
  xlab("")+
  ggtitle("Importance of Twitter Profile Features in Predicting Congressperson's Legislative Productivity")+
  guides(fill=F)

# The number of followers is by far the strongest predictor of how many bills
# A congressperson wlil introduce to congress
# the frequency at which a person tweets and the numer of favorites and RTS
# they tend to receive are not as important.

c("RF Error"=product.rferr,"KNN Error"=product.knnerr)

# Random forest and KNN will both on average predict a value of bills
# within one standard deviation of the correct value
sqrt(c(product.knnerr,product.rferr))
sd(productivity.df$Bills_Introduced)
