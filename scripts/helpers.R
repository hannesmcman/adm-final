library(purrr)
library(dplyr)
library(rtweet)
library(textfeatures)
library(cluster)
library(factoextra)
library(ggrepel)
library(class)
library(ranger)
library(grid)
library(gridExtra)

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

showCluster <- function (K, data.df, labels, num) {
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

avg_si <- function(k, data.df) {
  mod.km<- kmeans(data.df, centers = k, nstart = 25)
  ss <- silhouette(mod.km$cluster, get_dist(data.df))
  mean(ss[, 3])
}

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

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
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