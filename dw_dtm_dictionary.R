tdm <- cleaned_out %>%
  select(review_id, binary_rating, rating, review_text) %>%
  unnest_tokens(word, review_text)
