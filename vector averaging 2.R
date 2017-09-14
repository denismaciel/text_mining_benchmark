#vector averaging

train <- readRDS("feats_dotlic/amazonBooks_clean_train")
test <- readRDS("feats_dotlic/amazonBooks_clean_test")



# CHOOSE
ds_name <- "amazonBooks"
##ds_name <- "amazonfinefood"
#ds_name <- "imdb"
#ds_name <- "twitter"
#ds_name <- "yelp"

# CHOOSE
# type <- "test"
type <- "train"

# CHOOSE
#size = 100000
#size = 50000
#size = 20000
size = 10000
#size = 5000
#size = 2500
#size = 1000

for (i in 5:7){
  
  ds_name <- "yelp"
  type <- "train"
  list_cuts <- as.list(c(1000,2500,5000,10000,20000,50000,100000))
  
  
  train <- readRDS("data_new/yelp_train.RDS")
  test <- readRDS("data_new/yelp_test.RDS")
  
  
  size <- list_cuts[[i]]



technique <- "word2vec_vector_averaging"
file_name <- paste0("data_new/", ds_name, "_", type, ".RDS")

to_log <- list(
  technique = technique, 
  file_name = file_name,
  ds_name = ds_name,
  type = type
)
  size = size

#Subsetting-----
start_cleaning <- Sys.time()

train_ids <- sample(1:nrow(train), size)
df_sub <- train[train_ids,]

sub_ids <- sample(1:nrow(df_sub), nrow(df_sub))

train <- df_sub[sub_ids,]

df <- data.frame(row.names = (1:(nrow(train)+nrow(test))))
df$review_id <- NA
df$binary_rating <- NA
df$rating <- NA
df$review_text <- NA
df[1:nrow(train),] <- train 
df[(nrow(train)+1):(nrow(train)+nrow(test)),] <- test

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
end_cleaning <- Sys.time()

### VECTOR AVERAGING
sourceCpp("converter.cpp")
vocab <- as.data.frame(read.table("model1text.txt",header = F,stringsAsFactors = F,skip=1))
vocab[is.na(vocab)] <- 0
colnames(vocab)[1]="word"
print(str(vocab))

#Mixed approach----
### From Words to Paragraphs, Attempt 1:Vector Averaging 
start_features <- Sys.time()
d=NULL
d=strsplit(df$review_text," ")
for(i in 1:nrow(df))
{
  d[[i]]=gsub("^\\s+|\\s+$", "",d[[i]])
}

df_data <- get_average_vectors(d,matrix(as.numeric(unlist(vocab[,2:ncol(vocab)])),nrow=nrow(vocab)),vocab$word,stopwords())
end_features <- Sys.time()

df_data <- as.data.frame(df_data)
df_data$rating <- NA
df_data$rating[1:nrow(train)] <- train$binary_rating
df_data$rating [(nrow(train)+1):nrow(df_data)] <- test$binary_rating


#train <- df_data[1:size,]
#test <- df_data[(size + 1):nrow(df_data),]


#Save thw train data
saveRDS(df_data, paste0("./features/mixed/", to_log$technique, "_", ds_name, "_", type, "_", size, ".RDS"))
#type <- "test"
#saveRDS(test, paste0("./features/", to_log$technique, "_", ds_name, "_", type, "_", size, ".RDS"))

#save training log file
type <- "train"
to_log$computation_time <- list(
  features = list(end_features = end_features, 
                  start_features = start_features,
                  duration_features = as.numeric(end_features - start_features, units = "secs")),
  cleaning = list(end_cleaning = end_cleaning, 
                  start_cleaning = start_cleaning,
                  duration_cleaning = as.numeric(difftime(end_cleaning, start_cleaning, units = "secs")))
  
  )
to_log

date <- str_replace_all(strftime(Sys.time() , "%Y-%m-%dT%H:%M:%S"), ":", "")
jsonlite::toJSON(to_log, pretty = T) %>% 
  write(paste0("log_features/mixed/", to_log$technique, "_", ds_name, "_", type, "_", size, "_", date, ".log"))

#mixed approach----

start_features <- Sys.time()
d=NULL
d=strsplit(train$review_text," ")
for(i in 1:nrow(train))
{
  d[[i]]=gsub("^\\s+|\\s+$", "",d[[i]])
}

df_data <- get_average_vectors(d,matrix(as.numeric(unlist(vocab[,2:ncol(vocab)])),nrow=nrow(vocab)),vocab$word,stopwords())
end_features <- Sys.time()

df_data <- as.data.frame(df_data)
df_data$rating <- NA
df_data$rating[1:nrow(train)] <- train$binary_rating


#train <- df_data[1:size,]
#test <- df_data[(size + 1):nrow(df_data),]


#Save thw train data
saveRDS(df_data, paste0("./features/blind/", to_log$technique, "_", ds_name, "_", type, "_", size, ".RDS"))
#type <- "test"
#saveRDS(test, paste0("./features/", to_log$technique, "_", ds_name, "_", type, "_", size, ".RDS"))

#save training log file
type <- "train"
to_log$computation_time <- list(
  features = list(end_features = end_features, 
                  start_features = start_features,
                  duration_features = as.numeric(end_features - start_features, units = "secs")),
  cleaning = list(end_cleaning = end_cleaning, 
                  start_cleaning = start_cleaning,
                  duration_cleaning = as.numeric(difftime(end_cleaning, start_cleaning, units = "secs")))
  
)
to_log

date <- str_replace_all(strftime(Sys.time() , "%Y-%m-%dT%H:%M:%S"), ":", "")
jsonlite::toJSON(to_log, pretty = T) %>% 
  write(paste0("log_features/blind/", to_log$technique, "_", ds_name, "_", type, "_", size, "_", date, ".log"))


rm(list = ls(all = TRUE))


}
