library(data.table)
library(tidyverse)
#================================ Header ================================

input_file_path <- "./data_new/AmazonBooks.RDS"
df <- read_rds(input_file_path)

# DO: define the name of the file
output_file_name <- "amazonBooks"

ind_test <- sample(1:nrow(df), size = 5000)

test <- df[ind_test, ]
train <- df[-ind_test, ]

saveRDS(test, paste0("./data_new/", output_file_name, "_test.RDS"))
saveRDS(train, paste0("./data_new/", output_file_name, "_train.RDS"))

