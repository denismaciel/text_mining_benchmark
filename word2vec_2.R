rm(list = ls(all = TRUE))

library(data.table)
library(tidyverse)
library(stringr)
library(rword2vec)
library(Rcpp)
library(RcppArmadillo)
library(tm)
library(rpart)


to_be_cleaned <- as_tibble(readRDS("data_new/amazonBooks_train.RDS"))


# CHOOSE
ds_name <- "amazonBooks"
# ds_name <- "amazonfinefood"
#ds_name <- "imdb"
# ds_name <- "twitter"

# CHOOSE
# type <- "test"
type <- "train"

# CHOOSE
#size = 100000
# size = 50000
# size = 20000
#size = 10000
#size = 5000
#size = 2500
size = 1000


technique <- "word2vec"
file_name <- paste0("data_new/", ds_name, "_", type, ".RDS")

to_log <- list(
  technique = technique, 
  file_name = file_name,
  ds_name = ds_name,
  type = type,
  size = size
)

# CLEAN DATA ----
start_cleaning <- Sys.time()
source("dw_cleaning.R")
end_cleaning <- Sys.time()
df <- cleaned_out
remove(cleaned_out)



#Subsetting-----
train_ids <- sample(1:nrow(df), size)
df_sub <- df[train_ids,]

sub_ids <- sample(1:nrow(df_sub), 0.8*nrow(df_sub))

train <- df_sub[sub_ids,]
test <- df_sub[-sub_ids,]



train1 <- df_sub$review_text

write(train1,"text_data.txt") 
model <- word2vec(train_file = "text_data.txt",output_file ="model1.bin",layer1_size = 300,min_count = 40,num_threads = 4,window = 10,sample = 0.001,binary=1)
bin_to_txt("model1.bin","model1text.txt") 



sourceCpp("converter.cpp")
vocab <- as.data.frame(read.table("model1text.txt",header = F,stringsAsFactors = F,skip=1))
colnames(vocab)[1] <- "word"

num_clusters <- floor(nrow(vocab)/10)

vocab[is.na(vocab)] <- 0

kmean_model <- kmeans(vocab[,2:ncol(vocab)], centers = num_clusters, iter.max = 150)
write.csv(kmean_model$cluster, file="kmeans_clusters2.csv")
write.csv(kmean_model$centers, file="kmeans_centers2.csv")
kmean_model <- NULL

clusters <- read.csv("kmeans_clusters2.csv")
clusters$word <- vocab$word
colnames(clusters) <- c("word_no","cluster","word")

### bag of centroids for training data----
start_features <- Sys.time()

d <- NULL
d <- strsplit(train$review_text," ")
for(i in 1:nrow(train))
{
  d[[i]] <- gsub("^\\s+|\\s+$", "",d[[i]])
}

final <- get_bag_of_centroids (d ,vocab$word,clusters$cluster,num_clusters)
end_features <- Sys.time()

final <- as.data.frame(final)
final$rating <- train$rating


#Save thw train data-----
saveRDS(final, paste0("./features/", to_log$technique, "_", ds_name, "_", type, "_", size, ".RDS"))

#save training log file----
to_log$computation_time <- list(
  cleaning = list(end_cleaning = end_cleaning, 
                  start_cleaning = start_cleaning,
                  duration_cleaning = as.numeric(difftime(end_cleaning, start_cleaning, units = "secs"))),
    features = list(end_features = end_features, 
                  start_features = start_features,
                  duration_features = as.numeric(end_features - start_features, units = "secs"))
)
to_log

date <- str_replace_all(strftime(Sys.time() , "%Y-%m-%dT%H:%M:%S"), ":", "")
jsonlite::toJSON(to_log, pretty = T) %>% 
  write(paste0("log_features/", to_log$technique, "_", ds_name, "_", type, "_", size, "_", date, ".log"))


### bag of centroids for test data----

type <- "test"
to_log <- list(
  technique = technique, 
  file_name = file_name,
  ds_name = ds_name,
  type = type,
  size = size
)

start_features <- Sys.time()
d <- NULL
d <- strsplit(test$review_text," ")
for(i in 1:nrow(test))
{
  d[[i]] <- gsub("^\\s+|\\s+$", "",d[[i]])
}
final <- get_bag_of_centroids(d,vocab$word,clusters$cluster,num_clusters)
end_features <- Sys.time()

final <- as.data.frame(final)

#save test features----
saveRDS(final, paste0("./features/", to_log$technique, "_", ds_name, "_", type, "_", size, ".RDS"))

#save test log file----
to_log$computation_time <- list(
  cleaning = list(end_cleaning = end_cleaning, 
                  start_cleaning = start_cleaning,
                  duration_cleaning = as.numeric(difftime(end_cleaning, start_cleaning, units = "secs"))),
    features = list(end_features = end_features, 
                  start_features = start_features,
                  duration_features = as.numeric(end_features - start_features, units = "secs"))
)
to_log

date <- str_replace_all(strftime(Sys.time() , "%Y-%m-%dT%H:%M:%S"), ":", "")
jsonlite::toJSON(to_log, pretty = T) %>% 
write(paste0("log_features/", to_log$technique, "_", ds_name, "_", type, "_", size, "_", date, ".log"))