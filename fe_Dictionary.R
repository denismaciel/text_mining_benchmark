### PREPARATION ----
# DOWNLOAD PACKAGES
if(!require("data.table")) install.packages("data.table"); library("data.table")
if(!require("tm")) install.packages("tm"); library(tm)
if(!require("tidytext")) install.packages("tidytext"); library(tidytext)
if(!require("lsa")) install.packages("lsa"); library("lsa")
if(!require("caret")) install.packages("caret"); library("caret")
if(!require("stringr")) install.packages("stringr"); library("stringr")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
if(!require("reshape")) install.packages("reshape"); library("reshape")

# SPECIFY SETTINGS----
# NB!!! Select the dataset you need

# CHOOSE
# ds_name <- "amazonBooks"
 ds_name <- "amazonfinefood"
# ds_name <- "imdb"
# ds_name <- "twitter"
# ds_name <- "yelp"

# CHOOSE
type <- "test"
# type <- "train"

# CHOOSE
 size = 5000
# size = 100000
# size = 50000
# size = 20000
# size = 10000
# size = 5000
# size = 2500
# size = 1000

 
technique <- "Dictionary"
file_name <- paste0("data_new/", ds_name, "_", type, ".RDS")

to_log <- list(
  technique = technique, 
  file_name = file_name,
  ds_name = ds_name,
  type = type,
  size = size
)


# DOWNLOAD DATA ----
raw_data <- readRDS(file_name)


# SPLIT DATA ----
ind_sample <- sample(1:nrow(raw_data), size = to_log$size)
to_be_cleaned <- raw_data[ind_sample, ]
rm(raw_data)

# CLEAN DATA ----
start_cleaning <- Sys.time()
source("dw_cleaning.R")
end_cleaning <- Sys.time()


# CREATE TIDYTEXT ---
start_dtm <- Sys.time()
source("dw_tt_dictionary.R")
end_dtm <- Sys.time()

# ============================= Creating Features ===================================
start_features <- Sys.time()
#INPUT DICTIONARY
AFINN <- sentiments %>%
  filter(lexicon == "AFINN") %>%
  select(word, afinn_score = score)

#LOOK UP IN DICTIONARY
reviews_sentiment1 <- tidytext %>%
  inner_join(AFINN, by = "word") %>%
  group_by(review_id, binary_rating)
reviews_sentiment2<-aggregate(reviews_sentiment1[, 5], 
                               list(reviews_sentiment1$review_id), mean)
reviews_sentiment2 <- rename(reviews_sentiment2, c(Group.1="review_id"))
total <- merge(cleaned_out,reviews_sentiment2,by="review_id")

#THRESHOLD
mean(total$afinn_score) #1.056463
total$sentimentcategory<-ifelse(total$afinn_score>1.056463,1,0)
ratings_labeled <- total$binary_rating
ratings_predicted<-total$sentimentcategory
table(ratings_predicted)
confusionMatrix(ratings_predicted, ratings_labeled)
end_features <- Sys.time()

#RESULT DESCRIPTIONS
boxplot(total$afinn_score~ total$rating, data = data.frame(total$rating,total$afinn_score), 
        xlab = "Star rating", ylab = "Sentiment score")


# SAVE LOG ----
to_log$computation_time <- list(
  cleaning = list(end_cleaning = end_cleaning, 
                  start_cleaning = start_cleaning,
                  duration_cleaning = as.numeric(difftime(end_cleaning, start_cleaning, units = "secs"))),
  dtm = list(end_dtm = end_dtm, 
             start_dtm = start_dtm,
             duration_dtm = as.numeric(difftime(end_dtm, start_dtm, units = "secs"))),
  features = list(end_features = end_features, 
                  start_features = start_features,
                  duration_features = as.numeric(end_features - start_features, units = "secs"))
)
to_log

date <- str_replace_all(strftime(Sys.time() , "%Y-%m-%dT%H:%M:%S"), ":", "")
jsonlite::toJSON(to_log, pretty = T) %>% 
  write(paste0("log_features/", to_log$technique, "_", "_", ds_name, "_", type, "_", size, "_", date, ".log"))
