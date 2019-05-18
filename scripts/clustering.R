library(purrr)
library(rtweet)
library(textfeatures)
# pol_accounts.df <- read.csv("pol_accounts.csv", sep = ';')

pol_accounts.df <- read.csv("data/legislators-current.csv")

politician_account_ids <- pol_accounts.df$twitter

pol_account_data <- lookup_users(politician_account_ids)

pol_account_data_numeric <- select_if(pol_account_data, is.numeric)
pol_account_data_numeric$id <- pol_account_data$user_id
pol_account_data_numeric$name <- pol_account_data$name
pol_account_data_numeric$screen_name <- pol_account_data$screen_name

filter_na_columns <- function (data.df, max_nas) {
  num_nas <- map_dbl(1:ncol(data.df), function (colNum) {
    sum(is.na(data.df[,colNum]))
  })
  names(num_nas) <- names(data.df)
  print(num_nas)
  cols_with_too_many_nas <- which(num_nas > max_nas)
  data.filtered <- data.df[,-cols_with_too_many_nas]
  (na.omit(data.filtered))
}

pol_account_data_numeric <- filter_na_columns(pol_account_data_numeric, 508)
pol_account_data_numeric <- na.omit(pol_account_data_numeric)
pol_account_data_numeric <- pol_account_data_numeric %>%
  filter(name != "Barack Obama")

showCluster <- function (K, data.df, labels, num) {
  # data.df <- pol_account_data_numeric.preds
  # K <- 2
  # labels <- pol_account_data_numeric$name
  # num <- 30
  mod.pc <- prcomp(data.df)
  rot.mat <- mod.pc$rotation
  data.mat <- as.matrix(data.df)
  data.rot <-  data.mat%*% rot.mat
  dataRot.df <- data.frame(data.rot)
  mod.km <- kmeans(dataRot.df,K,nstart=25)
  dataRot.df$cluster <- factor(mod.km$cluster)
  dataRot.df$label <- labels
  plotData.df <- dataRot.df %>% group_by(cluster) %>% add_tally() ##%>% sample_n(size = n)
  plotData.df <- plotData.df %>% sample_n(size = min(c(mean(n), num)))
  plotData.df%>%
    ggplot()+
    geom_point(aes(PC1,PC2,color=cluster))+
    geom_text_repel(aes(PC1,PC2,color=cluster,label=label),size=2)+
    ##    guides(color=TRUE)+
    ggtitle("Clustering")
}

pol_account_data_numeric.preds <- dplyr::select(pol_account_data_numeric, -c(id, name, screen_name))

showCluster(3, pol_account_data_numeric.preds, pol_account_data_numeric$name, 30)

avg_si <- function(k, data.df) {
  mod.km<- kmeans(data.df, centers = k, nstart = 25)
  ss <- silhouette(mod.km$cluster, get_dist(data.df))
  mean(ss[, 3])
}

# plot_silhouettes <- function (data.df, max_k) {
#   k.values <- 2:max_k
#   avg_si_vals <- map_dbl(k.values, function (k) {
#     avg_si(k, data.df)
#   })
#   data.frame(k=k.values,si=avg_si_vals)%>%
#     ggplot()+
#     geom_point(aes(k,si))+
#     geom_line(aes(k,si))+
#     scale_x_continuous(breaks=k.values)+
#     ggtitle("Average silhoutte values")
# }
# 
# plot_twiss <- function (data.df, max_k) {
#   M <- max_k
#   twissVals <- numeric(M)
#   for(k in 1:M){
#     mod.kmeans <- kmeans(data.df,centers=k,nstart=25)
#     twissVals[k] <- mod.kmeans$tot.withinss
#   }
#   
#   data.frame(k=1:M,
#              twiss=twissVals) %>%
#     ggplot()+
#     geom_point(aes(k,twiss))+
#     geom_line(aes(k,twiss))+
#     scale_x_continuous(breaks=1:M)
# }

plot_twiss_and_silhouette <- function (data.df, max_k) {
  k.values <- 2:max_k
  avg_si_vals <- map_dbl(k.values, function (k) {
    avg_si(k, data.df)
  })
  twissVals <- map_dbl(k.values, function (k) {
    mod.kmeans <- kmeans(data.df,centers=k,nstart=25)
    mod.kmeans$tot.withinss
  })
  
  si_plot <- data.frame(k=k.values, avg_si=avg_si_vals) %>%
    ggplot() +
    geom_point(aes(k, avg_si)) +
    geom_line(aes(k,avg_si))+
    scale_x_continuous(breaks=k.values)
  twiss_plot <- data.frame(k=k.values, twiss=twissVals) %>%
    ggplot() +
    geom_point(aes(k, twiss)) +
    geom_line(aes(k,twiss))+
    scale_x_continuous(breaks=k.values)
  grid.arrange(si_plot, twiss_plot, nrow = 2)
}

get_twitter_user_nlp_data <- function (screen_name) {
  tweets.df <- get_timeline(as.character(screen_name), n = 100, check = FALSE)
  if (nrow(tweets.df) == 0) {
    ret <- as.data.frame(t(rep(NA, 54)))
  } else {
    nlp_features <- textfeatures(tweets.df$text)
    modes <- apply(nlp_features, 2, getmode)
    ret <- as.data.frame(t(modes))
  }
  ret
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

pol_nlp_data <- map_dfr(pol_account_data$screen_name[1:181], get_twitter_user_nlp_data)
pol_nlp_data$screen_name <- pol_account_data$screen_name[1:181]
pol_nlp_data_filtered <- na.omit(pol_nlp_data)

pol_nlp_cluster <- dplyr::select(pol_nlp_data_filtered, -screen_name)
pol_nlp_cluster <- scale(pol_nlp_cluster)

plot_twiss_and_silhouette(pol_nlp_cluster, 50)
showCluster(2, pol_nlp_cluster, pol_nlp_data_filtered$screen_name, 50)
mod.pc <- prcomp(pol_nlp_cluster)
fviz_pca_var(mod.pc)

tweets.df <- get_timeline("SenatorFischer", n = 100)
nlp_features <- textfeatures(tweets.df$text)
modes <- apply(nlp_features, 2, getmode)
test1 <- as.data.frame(t(modes))
# 
# names(test1)
# 
# tweets2.df <- get_timeline("SenatorRounds", n = 100)
# nlp_features <- textfeatures(tweets2.df$text, word_dims = 0)
# modes <- apply(nlp_features, 2, getmode)
# test2 <- as.data.frame(t(modes))
# 
# names(test2)

## WITH PARTY

showClusterWithClass <- function (K, data.df, labels, classes, num) {
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
  plotData.df <- dataRot.df %>% group_by(cluster) %>% add_tally()
  plotData.df <- plotData.df %>% sample_n(size = min(c(mean(n), num)))
  plotData.df%>%
    ggplot()+
    geom_point(aes(PC1,PC2,color=cluster, shape=class))+
    geom_text_repel(aes(PC1,PC2,color=cluster,label=label),size=2)+
    ##    guides(color=TRUE)+
    ggtitle("Clustering")
}

pol_nlp_data <- map_dfr(pol_accounts.df$twitter, get_twitter_user_nlp_data)
save(pol_nlp_data, file = "pol_nlp_data.Rdata")
load("pol_nlp_data.Rdata")
pol_nlp_data_filtered <- pol_nlp_data %>%
  mutate(screen_name = pol_accounts.df$twitter) %>%
  mutate(party = pol_accounts.df$party) %>%
  mutate(gender = pol_accounts.df$gender) %>%
  na.omit()

pol_nlp_cluster <- dplyr::select(pol_nlp_data_filtered, -c(screen_name, party, gender))

plot_twiss_and_silhouette(pol_nlp_cluster, 50)
showCluster(2, pol_nlp_cluster, pol_nlp_data_filtered$screen_name, 50)
showClusterWithClass(2, pol_nlp_cluster, pol_nlp_data_filtered$screen_name, pol_nlp_data_filtered$gender, 50)
mod.pc <- prcomp(pol_nlp_cluster)
fviz_pca_var(mod.pc)

## NO Word2Vec

get_twitter_user_nlp_data_no_w2v <- function (screen_name) {
  tweets.df <- get_timeline(as.character(screen_name), n = 100, check = FALSE)
  if (nrow(tweets.df) == 0) {
    ret <- as.data.frame(t(rep(NA, 34)))
  } else {
    nlp_features <- textfeatures(tweets.df$text, word_dims = 0)
    modes <- apply(nlp_features, 2, getmode)
    ret <- as.data.frame(t(modes))
  }
  ret
}

test <- get_twitter_user_nlp_data_no_w2v("SenSherrodBrown")

##TAKES A WHILE
# pol_nlp_data_nw2v <- map_dfr(pol_accounts.df$twitter, get_twitter_user_nlp_data_no_w2v)
# save(pol_nlp_data_nw2v, file = "pol_nlp_data_nw2v.Rdata")
load("pol_nlp_data_nw2v.Rdata")
pol_nlp_data_nw2v_filtered <- pol_nlp_data_nw2v[,1:34]
pol_nlp_data_nw2v_filtered <- pol_nlp_data_nw2v_filtered %>%
  mutate(screen_name = pol_accounts.df$twitter) %>%
  mutate(party = pol_accounts.df$party) %>%
  mutate(gender = pol_accounts.df$gender) %>%
  na.omit()

pol_nlp_nw2v_cluster <- dplyr::select(pol_nlp_data_nw2v_filtered, -c(screen_name, party, gender))
plot_twiss_and_silhouette(pol_nlp_nw2v_cluster, 50)
showCluster(2, pol_nlp_nw2v_cluster, pol_nlp_data_nw2v_filtered$screen_name, 50)
showClusterWithClass(2, pol_nlp_nw2v_cluster, pol_nlp_data_nw2v_filtered$screen_name, pol_nlp_data_nw2v_filtered$party, 50)
mod.pc <- prcomp(pol_nlp_nw2v_cluster)
fviz_pca_var(mod.pc)
