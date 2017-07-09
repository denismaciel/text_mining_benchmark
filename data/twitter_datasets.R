# Create DATASETS ----
#================================ Header ================================
# 1) Kaggle competititon dataset
library(RCurl)
train_data_url <- "https://dl.dropboxusercontent.com/u/8082731/datasets/UMICH-SI650/training.txt"
train_data_file <- getURL(train_data_url)
a <- read.csv(
  text = train_data_file, 
  sep='\t', 
  header=FALSE, 
  quote = "",
  stringsAsFactor=F,
  col.names=c("Sentiment", "Text"))
a$review_id <- c(1:nrow(a))
a$rating <- a$Sentiment
a$binary_rating <- a$Sentiment
a$review_text <- a$Text
a$Text <- NULL
a$Sentiment <- NULL

library("tidyverse")
pos <- a[a$binary_rating == 1, ]
pos <- pos[sample(1:nrow(pos), 2500, replace = FALSE), ]
neg <- a[a$binary_rating == 0, ]
neg <- neg[sample(1:nrow(neg), 2500, replace = FALSE), ]
outA <- bind_rows(pos, neg)
summary(outA)
saveRDS(outA, "data/twitter_5K.rds")


# 2) Emodji Labeled
b <- read_csv("data/Copies for APA paper/Sentiment Analysis Dataset.csv")
b <- b[b$SentimentSource=="Sentiment140",]
colnames(b) <- c("review_id","rating","del","review_text")
b <- b %>% 
  mutate(binary_rating = rating) %>% 
  select(review_id, 
         rating,
         binary_rating,
         review_text)

pos <- b[b$binary_rating == 1, ]
pos <- pos[sample(1:nrow(pos), 25000, replace = FALSE), ]
neg <- b[b$binary_rating == 0, ]
neg <- neg[sample(1:nrow(neg), 25000, replace = FALSE), ]
outB <- bind_rows(pos, neg)
summary(outB)
saveRDS(outB, "data/twitter_50K.rds")

pos <- b[b$binary_rating == 1, ]
pos <- pos[sample(1:nrow(pos), 100000, replace = FALSE), ]
neg <- b[b$binary_rating == 0, ]
neg <- neg[sample(1:nrow(neg), 100000, replace = FALSE), ]
outB200 <- bind_rows(pos, neg)
summary(outB200)
saveRDS(outB200, "data/twitter_200K.rds")


# 3) Sanders
c <- read_csv("data/Copies for APA paper/full-corpus.csv")
c$Sentiment <- as.factor(c$Sentiment)
# To little, only 1K observations are labeled as positive or negative.

# 4) other sources sentences
d1 <- read_delim("data/Copies for APA paper/sentiment labelled sentences/amazon_cells_labelled.txt", col_names = c("review_text","rating"), delim = "\t")
d2 <- read_delim("data/Copies for APA paper/sentiment labelled sentences/yelp_labelled.txt", col_names = c("review_text","rating"), delim = "\t")
# d3 <- read_delim("data/Copies for APA paper/sentiment labelled sentences/imdb_labelled.txt", col_names = c("review_text","rating"), delim = "\t")
# To little, only 2K observations are labeled as positive or negative. d3 is problematic
# Attempt to solve for d3
# char_vector <- readLines("data/Copies for APA paper/sentiment labelled sentences/imdb_labelled.txt")
# e <- data.frame(char_vector)
