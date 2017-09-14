
to_log_foldin <- list(
  technique = technique,
  approach = approach,
  file_name = file_name,
  ds_name = ds_name,
  type = type,
  size = size
)

start_foldin <- Sys.time()
test_name_foldin <- paste0("data_new/", ds_name, "_test_clean", ".RDS")
cleaned_out_foldin <- readRDS(test_name_foldin)

dtm_name_foldin<- paste0("data_new/", ds_name, "_test_dtm", ".RDS")
dtm_foldin <- readRDS(dtm_name_foldin)

rowTotals_foldin <- apply(dtm_foldin, 1, sum) # Find the sum of words in each Document
sum(rowTotals_foldin == 0) # Check if there are reviews in the corpus that doesn't contain any frequent terms
reviews_foldin <- dtm_foldin[rowTotals_foldin > 0, ] # Delete empty reviews
rm(dtm_foldin)

newdata = reviews_foldin

ldafeatures_foldin <- posterior(fit, newdata)
ldafeatures_final_foldin <- ldafeatures_foldin$topics

mydata_foldin <- as.data.frame(cleaned_out_foldin)
features_foldin <- mydata_foldin[,c("review_id","rating","binary_rating","type")]

rowTotals_foldin[rowTotals_foldin==0]
features_foldin <- features_foldin[rowTotals_foldin > 0, ] # Needed when some reviews were deleted in the process as empty
final_foldin <- cbind(features_foldin,ldafeatures_final_foldin)
end_foldin <- Sys.time()

# SAVE FOLDIN RESULTS----
final_binded <- rbind(final,final_foldin)
saveRDS(final_binded, paste0("./features/", to_log$technique, "_", approach, "_", ds_name, "_", type, "_", size, ".RDS"))

to_log_foldin$computation_time <- list(
  foldin = list(end_foldin = end_foldin, 
                start_foldin = start_foldin,
                duration_foldin = as.numeric(end_foldin - start_foldin, units = "secs"))
)

date_foldin <- str_replace_all(strftime(Sys.time() , "%Y-%m-%dT%H:%M:%S"), ":", "")
jsonlite::toJSON(to_log_foldin, pretty = T) %>% 
  write(paste0("log_features/", to_log_foldin$technique, "_", approach, "_", ds_name, "_", type, "_", size, "_", date_foldin, ".log"))
