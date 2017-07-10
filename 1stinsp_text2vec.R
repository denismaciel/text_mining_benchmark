library(text2vec)
library(stringr)
require(data.table) 


reviews <- readRDS("./yelp_reviews.rds", refhook = NULL)
setDT(reviews)


subs <- reviews[sample(1:nrow(reviews), 4000,replace=FALSE),]
subs$line_ID <- 1:nrow(subs)
prep_fun = function(x) {
  x %>% 
    # make text lower case
    str_to_lower %>% 
    # remove non-alphanumeric symbols
    str_replace_all("[^[:alnum:]]", " ") %>% 
    # collapse multiple spaces
    str_replace_all("\\s+", " ")
}
#subs$text = prep_fun(subs$text)
it = itoken(subs$review_text, progressbar = FALSE)
v = create_vocabulary(it) %>% 
  prune_vocabulary(doc_proportion_max = 0.1, term_count_min = 5)
vectorizer = vocab_vectorizer(v)
dtm = create_dtm(it, vectorizer)

tfidf = TfIdf$new()
lsa = LSA$new(n_topics = 5)

# pipe friendly transformation
dtm_tfidf_lsa = dtm %>% 
  fit_transform(tfidf) %>% 
  fit_transform(lsa)

dtm_tfidf_lsa_df <- data.frame(dtm_tfidf_lsa)
dtm_tfidf_lsa
#never mind 
plot(dtm_tfidf_lsa)
boo <- data.table(dtm_tfidf_lsa)
subs[subs$line_ID ==3999 ,]
boo[boo$V2 > boo$V1 && boo$V2 > boo$V3 && boo$V2 > boo$V4 && boo$V2 > boo$V5,]
ggplot(boo, aes(x=V1, y=V2, fill=V3,color=V4, size=V5^2)) +
  geom_point(shape=21) +
  scale_color_gradient(low="red", high="green") +
  scale_size_continuous(range=c(1,12))
