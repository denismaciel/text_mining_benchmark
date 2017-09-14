#Bag of centroids

train <- readRDS("feats_dotlic/yelp_clean_train")
test <- readRDS("feats_dotlic/yelp_clean_test")



# CHOOSE
#ds_name <- "amazonBooks"
##ds_name <- "amazonfinefood"
#ds_name <- "imdb"
#ds_name <- "twitter"
ds_name <- "yelp"

# CHOOSE
# type <- "test"
type <- "train"

# CHOOSE
size = 100000
#size = 50000
#size = 20000
#size = 10000
#size = 5000
#size = 2500
#size = 1000
for (i in 1:7){
  
  ds_name <- "amazonBooks"
  type <- "train"
  list_cuts <- as.list(c(1000,2500,5000,10000,20000,50000,100000))
  
  
  train <- readRDS("data_new/amazonBooks_train.RDS")
  test <- readRDS("data_new/amazonBooks_test.rds")
  
  
  size <- 100000
  

technique <- "word2vec_centroids"
file_name <- paste0("data_new/", ds_name, "_", type, ".RDS")

to_log <- list(
  technique = technique, 
  file_name = file_name,
  ds_name = ds_name,
  type = type,
  size = size
)

#Subsetting-----
start_cleaning <- Sys.time()

train_ids <- sample(1:nrow(train), size)
df_sub <- train[train_ids,]

sub_ids <- sample(1:nrow(df_sub), nrow(df_sub))

train <- df_sub[sub_ids,]

to_be_cleaned <- data.frame(row.names = (1:(nrow(train)+nrow(test))))
to_be_cleaned$review_id <- NA
to_be_cleaned$binary_rating <- NA
to_be_cleaned$rating <- NA
to_be_cleaned$review_text <- NA
to_be_cleaned[1:nrow(train),] <- train 
to_be_cleaned[(nrow(train)+1):(nrow(train)+nrow(test)),] <- test

test_dummy <- data.frame(row.names = (1:nrow(to_be_cleaned)))
test_dummy$review_id <- NA
test_dummy$review_id <- to_be_cleaned$review_id
test_dummy$test <- NA
test_dummy$test[1:size] <- 0
test_dummy$test[(size+1):nrow(test_dummy)] <- 1

source("dw_cleaning.R")
df <- cleaned_out



df <- merge.data.frame(df,test_dummy, by = "review_id")

remove(cleaned_out, df_sub, test_dummy)

df_int <- split ( df,df$test)
train <- df_int$`0`
test <- df_int$`1`

remove(df_int)


train1 <- df$review_text
write(train1,"text_data.txt") 
model <- word2vec(train_file = "text_data.txt",output_file ="model1.bin",layer1_size = 300,min_count = 40,num_threads = 4,window = 10,sample = 0.001,binary=1)
bin_to_txt("model1.bin","model1text.txt") 

sourceCpp("converter.cpp")
vocab <- as.data.frame(read.table("model1text.txt",header = F,stringsAsFactors = F,skip=1))


colnames(vocab)[1] <- "word"

num_clusters <- floor(nrow(vocab)/10)
vocab[is.na(vocab)] <- 0
end_cleaning <- Sys.time()

start_kmean <- Sys.time()
kmean_model <- kmeans(vocab[,2:ncol(vocab)], centers = num_clusters, iter.max = 150)
write.csv(kmean_model$cluster, file="kmeans_clusters2.csv")
write.csv(kmean_model$centers, file="kmeans_centers2.csv")
kmean_model <- NULL

clusters <- read.csv("kmeans_clusters2.csv")
clusters$word <- vocab$word
colnames(clusters) <- c("word_no","cluster","word")
end_kmean <- Sys.time()

#Mixed approach----
### train: bag of centroids
start_features <- Sys.time()
d <- NULL
d <- strsplit(df$review_text," ")
for(i in 1:nrow(df))
{
  d[[i]] <- gsub("^\\s+|\\s+$", "",d[[i]])
}

final <- get_bag_of_centroids (d ,vocab$word,clusters$cluster,num_clusters)
end_features <- Sys.time()

final <- as.data.frame(final)
final$rating <- NA
final$rating[1:nrow(train)] <- train$binary_rating
final$rating[(nrow(train)+1):nrow(final)] <- test$binary_rating


#Save thw train data
saveRDS(final, paste0("./features/mixed/", to_log$technique, "_", ds_name, "_", type, "_", size, ".RDS"))

#save training log file----
type <- "train"
to_log$computation_time <- list(
    features = list(end_features = end_features, 
                  start_features = start_features,
                  duration_features = as.numeric(end_features - start_features, units = "secs")),
    cleaning = list(end_cleaning = end_cleaning, 
                    start_cleaning = start_cleaning,
                    duration_cleaning = as.numeric(difftime(end_cleaning, start_cleaning, units = "secs"))),
    kmeans = list(end_kmean = end_kmean, 
                  start_kmean = start_kmean,
                  duration_kmeans = as.numeric(difftime(end_kmean, start_kmean, units = "secs")))
    
    )
to_log

date <- str_replace_all(strftime(Sys.time() , "%Y-%m-%dT%H:%M:%S"), ":", "")
jsonlite::toJSON(to_log, pretty = T) %>% 
  write(paste0("log_features/mixed/", to_log$technique, "_", ds_name, "_", type, "_", size, "_", date, ".log"))

### Blind approach----
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
saveRDS(final, paste0("./features/blind/", to_log$technique, "_", ds_name, "_", type, "_", size, ".RDS"))

#save training log file----
to_log$computation_time <- list(
  cleaning = list(end_cleaning = end_cleaning, 
                  start_cleaning = start_cleaning,
                  duration_cleaning = as.numeric(difftime(end_cleaning, start_cleaning, units = "secs"))),
  features = list(end_features = end_features, 
                  start_features = start_features,
                  duration_features = as.numeric(end_features - start_features, units = "secs")),
  kmeans = list(end_kmean = end_kmean, 
                start_kmean = start_kmean,
                duration_kmeans = as.numeric(difftime(end_kmean, start_kmean, units = "secs")))
  )
to_log

date <- str_replace_all(strftime(Sys.time() , "%Y-%m-%dT%H:%M:%S"), ":", "")
jsonlite::toJSON(to_log, pretty = T) %>% 
  write(paste0("log_features/blind/", to_log$technique, "_", ds_name, "_", type, "_", size, "_", date, ".log"))

rm(list = ls(all = TRUE))

}
