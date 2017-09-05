# Create DATASETS ----
#================================ Header ================================
# 1) Kaggle competititon dataset
library("tidyverse")
library(RCurl)
train_data_url <- "https://dl.dropboxusercontent.com/u/8082731/datasets/UMICH-SI650/training.txt"
train_data_file <- getURL(train_data_url)
a <- read.csv(
  text = train_data_file, 
  sep='\t', 
  header=FALSE, 
  quote = "",
  stringsAsFactor=F,
  col.names=c("rating", "review_text"))
unique(a$review_text) %>% tbl_df()

# x <- a %>% 
#   group_by(review_text) %>% 
#   summarise(n = n()) %>% 
#   arrange(desc(n))
x <- a %>% 
  group_by(review_text) %>% 
  filter(row_number() == 1)
# View(x)

x$review_id <- c(1:nrow(x))
x <- x %>% 
  mutate(binary_rating = rating) %>% 
  select(review_id, 
         rating,
         binary_rating,
         review_text)

pos <- x[x$binary_rating == 1, ]
pos <- pos[sample(1:nrow(pos), 625, replace = FALSE), ]
neg <- x[x$binary_rating == 0, ]
neg <- neg[sample(1:nrow(neg), 625, replace = FALSE), ]
outA <- bind_rows(pos, neg)
summary(outA)
# saveRDS(outA, "data/twitter_12H.rds")

# 1b) Sanders
a2 <- read_csv("data/Copies for APA paper/full-corpus.csv")
a2$rating <- a2$Sentiment
a2[a2$rating == "positive",]$rating <- "1"
a2[a2$rating == "negative",]$rating <- "0"
# a2$rating <- as.numeric(a2$rating)
a2$review_text <- a2$TweetText
a2$review_id <- a2$TweetId
a2$TweetText <- NULL
a2$Sentiment <- NULL
a2$TweetId <- NULL
a2$Topic <- NULL
a2$TweetDate <- NULL
pos <- a2[a2$rating == "1", ]
pos <- pos[sample(1:nrow(pos), 500, replace = FALSE), ]
neg <- a2[a2$rating == "0", ]
neg <- neg[sample(1:nrow(neg), 500, replace = FALSE), ]
outA2 <- bind_rows(pos, neg)
summary(outA2)
outA2$rating <- as.numeric(outA2$rating)
outA2$review_id <- as.numeric(outA2$review_id)
outA2 <- outA2 %>% 
  mutate(binary_rating = rating) %>% 
  select(review_id, 
         rating,
         binary_rating,
         review_text)
# saveRDS(outA2, "data/twitter_10H.rds")

ajoint <- bind_rows(outA,outA2)
saveRDS(ajoint, "data/twitter_22H.rds")


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
pos <- pos[sample(1:nrow(pos), 52500, replace = FALSE), ]
neg <- b[b$binary_rating == 0, ]
neg <- neg[sample(1:nrow(neg), 52500, replace = FALSE), ]
outB <- bind_rows(pos, neg)
summary(outB)
saveRDS(outB, "data_new/twitter_105K.rds")


pos <- b[b$binary_rating == 1, ]
pos <- pos[sample(1:nrow(pos), 100000, replace = FALSE), ]
neg <- b[b$binary_rating == 0, ]
neg <- neg[sample(1:nrow(neg), 100000, replace = FALSE), ]
outB200 <- bind_rows(pos, neg)
summary(outB200)
saveRDS(outB200, "data/twitter_200K.rds")


# 4) other sources sentences
d1 <- read_delim("data/Copies for APA paper/sentiment labelled sentences/amazon_cells_labelled.txt", col_names = c("review_text","rating"), delim = "\t")
d2 <- read_delim("data/Copies for APA paper/sentiment labelled sentences/yelp_labelled.txt", col_names = c("review_text","rating"), delim = "\t")
# d3 <- read_delim("data/Copies for APA paper/sentiment labelled sentences/imdb_labelled.txt", col_names = c("review_text","rating"), delim = "\t")
# To little, only 2K observations are labeled as positive or negative. d3 is problematic
# Attempt to solve for d3
# char_vector <- readLines("data/Copies for APA paper/sentiment labelled sentences/imdb_labelled.txt")
# e <- data.frame(char_vector)
