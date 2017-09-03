library(jsonlite)
library(data.table)
library(tidyverse)
memory.limit(size=30000)
#1. load and process data
all_reviews <- stream_in(file("./yelp_academic_dataset_review.json"),pagesize = 10000)
setDT(all_reviews)

#create subset of 50000 reviews and add binary good/bad rating
all_reviews[stars >3,][sample(1:nrow(all_reviews[stars >3,]), 25000,replace=FALSE),]
goods <- all_reviews[all_reviews$stars >3,][sample(1:nrow(all_reviews[all_reviews$stars >3,]), 25000,replace=FALSE),]
bads <- all_reviews[all_reviews$stars <3,][sample(1:nrow(all_reviews[all_reviews$stars <3,]), 25000,replace=FALSE),]
sreviews <- rbind(goods, bads)
sreviews$is_good <- 1 
sreviews[sreviews$stars < 3,]$is_good <- 0


#delete unwanted columns and add remove a non alphanumeric characters
sreviews$user_id <- NULL
sreviews$date <- NULL
sreviews$date <- NULL
sreviews$useful <- NULL
sreviews$funny <- NULL
sreviews$cool <- NULL
sreviews$business_id <-NULL
sreviews$type <-NULL
sreviews$n_rating <-NULL
sreviews$rating <- sreviews$stars
sreviews$stars <- NULL
sreviews$binary_rating <- sreviews$is_good
sreviews$is_good <- NULL
sreviews$line_ID <- NULL
sreviews$review_text <- sreviews$text
sreviews$text <- NULL
setcolorder(sreviews, c("review_id", "rating", "binary_rating","review_text"))
library(stringr)
prep_fun = function(x) {
  x %>% 
    # make text lower case
    str_to_lower %>% 
    # remove non-alphanumeric symbols
    str_replace_all("[^[:alnum:]]", " ") %>% 
    # collapse multiple spaces
    str_replace_all("\\s+", " ")
}
sreviews$review_text <- prep_fun(sreviews$review_text)

saveRDS(sreviews, file = "./data/yelp_reviews.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)





