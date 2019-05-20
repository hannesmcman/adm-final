library(purrr)
library(rtweet)
library(textfeatures)
library(grid)
library(gridExtra)
library(outliers)

source(file = "scripts/helpers.R")
# pol_accounts.df <- read.csv("pol_accounts.csv", sep = ';')

pol_accounts.df <- read.csv("data/legislators-current.csv")

pol_account_data <- lookup_users(as.character(pol_accounts.df$twitter))

pol_account_data_numeric <- select_if(pol_account_data, is.numeric)
pol_account_data_numeric$id <- pol_account_data$user_id
pol_account_data_numeric$name <- pol_account_data$name
pol_account_data_numeric$screen_name <- pol_account_data$screen_name

pol_account_data_numeric <- filter_na_columns(pol_account_data_numeric, 445)
pol_account_data_numeric <- na.omit(pol_account_data_numeric)
pol_account_data_numeric <- pol_account_data_numeric %>%
  filter(!(screen_name %in% c("SenSanders", "SenWarren")))
pol_account_data_numeric.preds <- dplyr::select(pol_account_data_numeric, -c(id, name, screen_name))
plot_twiss_and_silhouette(pol_account_data_numeric.preds, 50)
showCluster(2, pol_account_data_numeric.preds, pol_account_data_numeric$name, 30)
mod.pca <- prcomp(pol_account_data_numeric.preds)
fviz_pca_var(mod.pca)

showClusterWithClass()

pol_nlp_data <- map_dfr(pol_account_data$screen_name[1:181], get_twitter_user_nlp_data)
pol_nlp_data$screen_name <- pol_account_data$screen_name[1:181]
pol_nlp_data_filtered <- na.omit(pol_nlp_data)

pol_nlp_cluster <- dplyr::select(pol_nlp_data_filtered, -screen_name)
pol_nlp_cluster <- scale(pol_nlp_cluster)

plot_twiss_and_silhouette(pol_nlp_cluster, 50)
showCluster(2, pol_nlp_cluster, pol_nlp_data_filtered$screen_name, 50)
mod.pc <- prcomp(pol_nlp_cluster)
fviz_pca_var(mod.pc)

## WITH PARTY

pol_nlp_data <- map_dfr(pol_accounts.df$twitter, get_twitter_user_nlp_data)
save(pol_nlp_data, file = "cache/pol_nlp_data.Rdata")
load("cache/pol_nlp_data.Rdata")
pol_nlp_data_filtered <- pol_nlp_data[,1:54]
pol_nlp_data_filtered <- pol_nlp_data_filtered %>%
  mutate(screen_name = pol_accounts.df$twitter) %>%
  mutate(party = pol_accounts.df$party) %>%
  mutate(gender = pol_accounts.df$gender) %>%
  na.omit()

pol_nlp_cluster <- dplyr::select(pol_nlp_data_filtered, -c(screen_name, party, gender))

plot_twiss_and_silhouette(pol_nlp_cluster, 50)
showCluster(2, pol_nlp_cluster, pol_nlp_data_filtered$screen_name, 50)
showClusterWithClass(2, pol_nlp_cluster, pol_nlp_data_filtered$screen_name, pol_nlp_data_filtered$party, 50)
showClusterWithClass(2, pol_nlp_cluster, pol_nlp_data_filtered$screen_name, pol_nlp_data_filtered$gender, 50)
mod.pc <- prcomp(pol_nlp_cluster)
fviz_pca_var(mod.pc)

## NO Word2Vec

test <- get_twitter_user_nlp_data_no_w2v("SenSherrodBrown")

##TAKES A WHILE
# pol_nlp_data_nw2v <- map_dfr(pol_accounts.df$twitter, get_twitter_user_nlp_data_no_w2v)
# save(pol_nlp_data_nw2v, file = "cache/pol_nlp_data_nw2v.Rdata")
load("cache/pol_nlp_data_nw2v.Rdata")
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
