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

# rt <- search_tweets(
#   "#rstats", n = 18000, include_rts = FALSE
# )
# names(rt)
# 
# djt <- get_timeline("realDonaldTrump", n = 3200)
# 
# names(djt)
# 
# summary(djt)
# 
# djt.text_features <- textfeatures(djt$text)
# 
# djt.numeric <- cbind(djt, djt.text_features)
# djt.numeric <- dplyr::select_if(djt.numeric, is.numeric)
# djt.numeric <- data.frame(scale(djt.numeric))
# names(djt.numeric)
# num_nas <- map_dbl(1:ncol(djt.numeric), function (colNum) {
#   sum(is.na(djt.numeric[,colNum]))
# })
# 
# names(num_nas) <- names(djt.numeric)
# num_nas
# cols_with_too_many_nas <- which(num_nas > 10)
# djt.numeric <- djt.numeric[,-cols_with_too_many_nas]
# djt.numeric <- na.omit(djt.numeric)
# names(djt.numeric)
# 
# mod.pc <- prcomp(djt.numeric)
# 
# fviz_pca_var(mod.pc)
# 
# k.values <- 2:8
# 
# avg_si <- function(k) {
#   print(k)
#   mod.km<- kmeans(djt.numeric, centers = k, nstart = 25)
#   ss <- silhouette(mod.km$cluster, get_dist(djt.numeric))
#   mean(ss[, 3])
# }
# avg_si_vals <- map_dbl(k.values, avg_si)
# 
# data.frame(k=k.values,si=avg_si_vals)%>%
#   ggplot()+
#   geom_point(aes(k,si))+
#   geom_line(aes(k,si))+
#   scale_x_continuous(breaks=k.values)+
#   ggtitle("Average silhoutte values")

showCluster <- function (K, data.df, labels, n) {
  mod.pc <- prcomp(data.df)
  rot.mat <- mod.pc$rotation
  data.mat <- as.matrix(data.df)
  data.rot <-  data.mat%*% rot.mat
  dataRot.df <- data.frame(data.rot)
  mod.km <- kmeans(dataRot.df,K,nstart=25)
  dataRot.df$cluster <- factor(mod.km$cluster)
  dataRot.df$label <- labels
  plotData.df <- dataRot.df %>% group_by(cluster) %>% sample_n(size = n)
  plotData.df%>%
    ggplot()+
    geom_point(aes(PC1,PC2,color=cluster))+
    geom_text_repel(aes(PC1,PC2,color=cluster,label=label),size=2)+
    ##    guides(color=TRUE)+
    ggtitle("Clustering")
}

# showCluster(2, djt.numeric, djt$text, 10)
real_users <- read_csv("data/cresci-2017/datasets_full/genuine_accounts/users.csv")
real_users <- dplyr::select(real_users, -test_set_2)
real_user_tweets <- read_csv("data/cresci-2017/datasets_full/genuine_accounts/tweets.csv")

social_spambot_users_1 <- read_csv("data/cresci-2017/datasets_full/social_spambots_1/users.csv")
social_spambot_tweets_1 <- read_csv("data/cresci-2017/datasets_full/social_spambots_1/tweets.csv")

social_spambot_users_2 <- read_csv("data/cresci-2017/datasets_full/social_spambots_2/users.csv")
social_spambot_tweets_2 <- read_csv("data/cresci-2017/datasets_full/social_spambots_2/tweets.csv")



names(social_spambot_users_1)
names(real_users)

real_users <- real_users %>% 
  mutate(real_user = TRUE)
social_spambot_users_1 <- social_spambot_users_1 %>%
  mutate(real_user = FALSE)

combined_users <- rbind(real_users, social_spambot_users_1)
test_users <- sample(1:nrow(combined_users), nrow(combined_users)/4, replace = F)
combined_tweets <- rbind(real_user_tweets, social_spambot_tweets_1)
vars_in_both <- intersect(names(combined_users), names(combined_tweets))
combined_tweets <- dplyr::select(combined_tweets, -vars_in_both)

combined_both.test <- merge(combined_users[test_users,], combined_tweets, by.x = "id", by.y = "user_id")
combined_both.train <- merge(combined_users[-test_users,], combined_tweets, by.x = "id", by.y = "user_id")

# names(combined_both.train)[1] <- "user_id"
# num_nas <- map_dbl(1:ncol(combined_both.train), function (colNum) {
#   sum(is.na(combined_both.train[,colNum]))
# })
# 
# names(num_nas) <- names(combined_both.train)
# num_nas
# 
# cols_with_too_many_nas <- which(num_nas > 12662)
# combined_both.train.filtered <- combined_both.train[,-cols_with_too_many_nas]
# combined_both.train.filtered <- na.omit(combined_both.train.filtered)
combined_both.train.filtered <- filter_na_columns(combined_both.train, 12662)
# combined_both.train.filtered <- dplyr::select(combined_both.train.filtered, -c())
names(combined_both.train.filtered)

subset_train <- sample(1:nrow(combined_both.train.filtered), nrow(combined_both.train.filtered)/100, replace = F)
subset_test <- sample(1:nrow(combined_both.test), nrow(combined_both.test)/100, replace = F)
# test <- sample(1:nrow(combined_both.train.filtered.subset), nrow(combined_both.train.filtered.subset)/8, replace = F)
train.df <- combined_both.train.filtered[subset_train,]
test.df <- combined_both.test[subset_test,]
# test.df <- combined_both.train.filtered.subset[test,]

# test <- textfeatures(train.df$text)

## CLASSIFICATION
# train.preds <- dplyr::select(train.df, -real_user)
# test.preds <- dplyr::select(test.df, -real_user)
# train.class <- with(train.df, real_user)
# test.class <- with(test.df, real_user)
# tweets.knn <- knn(train.df, test.df, train.class, k=5)

train1.df <- dplyr::select_if(train.df, function (x) {is.numeric(x) || is.logical(x)})
train1.df <- dplyr::select(train1.df, -c(user_id, in_reply_to_status_id, retweeted_status_id, in_reply_to_user_id))
predictor_vars <- names(train1.df)
predictor_vars <- predictor_vars[!predictor_vars == "real_user"]
## RANDOM FOREST
tweets.rf <- randomForest(factor(real_user) ~ ., data = train1.df)
tweets.rf.preds <- predict(tweets.rf, newdata = test.df)
with(test.df, mean(factor(real_user) != tweets.rf.preds))
## LOGISTIC
tweets.log <- glm(real_user ~ ., family = "binomial", data = train1.df)
tweets.log.probs <- predict(tweets.log, newdata = test.df, type = "response")
tweets.log.preds <- tweets.log.probs > 0.5
with(test.df, mean(real_user != tweets.log.preds))


head(tweets.rf.preds)
head(test.df$real_user)

num_fake <- length(which(!test.df$real_user))
num_fake/nrow(test.df)

## CLUSTERING

test.cluster <- filter_na_columns(test.df, nrow(test.df)/10)
test.cluster.text <- test.cluster$text
test.cluster.class <- test.cluster$real_user
test.cluster <- dplyr::select_if(test.cluster, is.numeric)
test.cluster <- dplyr::select(test.cluster, -c(id, test_set_1, in_reply_to_status_id, in_reply_to_user_id, retweeted_status_id))
names(test.cluster)

tweets.pc <- prcomp(test.cluster)

fviz_pca_var(tweets.pc)

k.values <- 2:8

avg_si <- function(k) {
  print(k)
  mod.km<- kmeans(test.cluster, centers = k, nstart = 25)
  ss <- silhouette(mod.km$cluster, get_dist(test.cluster))
  mean(ss[, 3])
}
avg_si_vals <- map_dbl(k.values, avg_si)

data.frame(k=k.values,si=avg_si_vals)%>%
  ggplot()+
  geom_point(aes(k,si))+
  geom_line(aes(k,si))+
  scale_x_continuous(breaks=k.values)+
  ggtitle("Average silhoutte values")

showClusterWithClass <- function (K, data.df, labels, classes, n) {
  mod.pc <- prcomp(data.df)
  rot.mat <- mod.pc$rotation
  data.mat <- as.matrix(data.df)
  data.rot <-  data.mat%*% rot.mat
  dataRot.df <- data.frame(data.rot)
  mod.km <- kmeans(dataRot.df,K,nstart=25)
  dataRot.df$cluster <- factor(mod.km$cluster)
  dataRot.df$label <- labels
  dataRot.df$class <- classes
  print(table(dataRot.df$cluster, dataRot.df$class))
  plotData.df <- dataRot.df %>% group_by(cluster) %>% sample_n(size = n)
  plotData.df%>%
    ggplot()+
    geom_point(aes(PC1,PC2,color=cluster, shape=class))+
    geom_text_repel(aes(PC1,PC2,color=cluster,label=label),size=2)+
    ##    guides(color=TRUE)+
    ggtitle("Clustering")
}

showClusterWithClass(2, test.cluster, test.cluster.text, test.cluster.class, 20)

## Am I a bot?

hannes_tweets <- get_timeline("hanboy612", n = 100)

hannes_user_info.filtered <- dplyr::select(hannes_tweets, predictor_vars)

names(hannes_tweets)
names(hannes_user_info)
names(hannes_combined)

## NLP Prediction

train.textfeatures <- textfeatures(train.df$text) ## This takes a while
train.textfeatures$real_user <- train.df$real_user
train.textfeatures <- train.textfeatures[ , colSums(is.na(train.textfeatures)) == 0]
test.textfeatures <- textfeatures(test.df$text)
test.textfeatures$real_user <- test.df$real_user
test.textfeatures <- test.textfeatures[ , colSums(is.na(test.textfeatures)) == 0]

text_features_in_both <- intersect(names(train.textfeatures), names(test.textfeatures))
names(train.textfeatures)
names(test.textfeatures)
train.textfeatures <- train.textfeatures[,text_features_in_both]
test.textfeatures <- train.textfeatures[,text_features_in_both]

# textfeatures.rf <- randomForest(factor(real_user) ~ ., data = train.textfeatures)
names(train.textfeatures)

## FULL RANGER MODEL WITH ALL TEXTFEATURES

textfeatures.rf <- ranger(factor(real_user) ~ ., data = train.textfeatures)

textfeatures.rf.pred <- predict(textfeatures.rf, data = test.textfeatures)
textfeatures.rf.preds <- predictions(textfeatures.rf.pred)

with(test.textfeatures, mean(factor(real_user) != textfeatures.rf.preds))

# showCluster(2, test.textfeatures, )

## Using model on tweets we pull ourselves

## Ranger model limited to features that can be extracted from Trump's tweets

djt <- get_timeline("realDonaldTrump", n = 100)
djt.text_features <- textfeatures(djt$text)
dim(djt.text_features)
dim(train.textfeatures)
text_features_in_trump <- intersect(names(train.textfeatures), names(djt.text_features))
train.textfeatures_in_trump <- train.textfeatures[,text_features_in_trump]
train.textfeatures_in_trump$real_user <- train.textfeatures$real_user
djt.text_features <- djt.text_features[,text_features_in_trump]
textfeatures_in_trump.rf <- ranger(factor(real_user) ~ ., data = train.textfeatures_in_trump)
djt.rf.pred <- predict(textfeatures_in_trump.rf, data = djt.text_features)
djt.rf.preds <- predictions(djt.rf.pred)
mean(!as.logical(djt.rf.preds))

textfeatures_in_trump.rf$forest$independent.variable.names

how_bot_like <- function (user_name, n, model.rf) {
  tweets.df <- get_timeline(user_name, n = n, check = FALSE)
  text_features <- textfeatures(tweets.df$text)
  features_in_model <- model.rf$forest$independent.variable.names
  text_features <- text_features[,features_in_model]
  pred <- predict(model.rf, data = text_features)
  preds <- predictions(pred)
  mean(!as.logical(preds))
}

how_bot_like("realDonaldTrump", 100, textfeatures_in_trump.rf)
how_bot_like("SenAmyKlobuchar", 100, textfeatures_in_trump.rf)

amy <- get_timeline("SenAmyKlobuchar", n = 3200)
amy.tf <- textfeatures(amy$text)

legislators_bot_like <- map_dbl(pol_accounts.df$twitter, function (screen_name) {
  how_bot_like(screen_name, 100, textfeatures_in_trump.rf)
})

plotData <- data.frame(name = pol_accounts.df$full_name, bot_score = legislators_bot_like)

plotData %>%
  top_n(20) %>%
  arrange(bot_score) %>%
  mutate(name = factor(name, levels = .$name)) %>%
  ggplot( aes(name, bot_score) ) +
    geom_segment( aes(x=name, xend = name, y=0, yend = bot_score), color="skyblue") +
    geom_point( color="blue", size=2, alpha=0.6) +
    theme_light() +
    coord_flip() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      axis.ticks.y = element_blank()
    ) 
