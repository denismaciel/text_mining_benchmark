
library(jsonlite)
#1. load and process data
#reviews <- stream_in(file("./yelp_academic_dataset_review.json"),pagesize = 10000)


require(data.table) 
#setDT(reviews)


#saveRDS(reviews, file = "./all_yelp_reviews.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

reviews <- readRDS("./yelp_reviews.rds", refhook = NULL)
setDT(reviews)



#sreviews <- reviews[sample(1:nrow(reviews), 100000,replace=FALSE),]
#we're only interested in restaurant reviews
#sreviews <- sreviews[grepl("restaurant|eat|food|sushi|pasta|burger", sreviews$text),]

library(SciencesPo)
library(lsa)
if(!require("foreach")) install.packages("foreach"); library("foreach")
if(!require("doParallel")) install.packages("doParallel"); library("doParallel")


#sreviews$n_rating <- Normalize(sreviews$stars, range=c(0,1))

#good reviews: stars>3, bad : stars<3
#good_sreviews <- sreviews[sreviews$stars>3,]
#bad_sreviews <- sreviews[sreviews$stars<3,]
#good_sreviews$is_good <- 1
#bad_sreviews$is_good <- 0
#sreviews <- rbind(good_sreviews, bad_sreviews)

#shuffle rows
#sreviews <- sreviews[sample(nrow(sreviews)),]

#make subset for testing, add line_ID
#sreviews$line_ID <- 1:nrow(sreviews)
# split known / class 70/30
#a <- round((max(sreviews$line_ID)*70)/100)
#b <- round(max(sreviews$line_ID))
#known <- sreviews[1:a,]
#class <- sreviews[round(max(known$line_ID)+1):max(sreviews$line_ID),]

#class$stars <- NULL
#class$n_rating <- NULL
#class$is_good <- NULL
#known_f_matr <- known[,c("line_ID", "text","is_good")]
#2. create matrices for lsa

known_f_matr_g <- known_f_matr[known_f_matr$is_good==1,]
known_f_matr_b <- known_f_matr[known_f_matr$is_good==0,]
td = tempfile()
dir.create(td)

#2.1 add each review and line id to tempfile
#setup parallel backend to use 4 processors
cl<-makeCluster(4)
registerDoParallel(cl)

  #start time
strt<-Sys.time()
foreach(k= 1:NROW(known_f_matr_g)) %do% {
  
  x<-iconv(toString(known_f_matr_g[k,2]), "latin1", "ASCII", sub="")
  write( x, file=paste(td, toString(known_f_matr_g[k,1]), sep="/"))
  
  print(Sys.time()-strt)
  k<-k+1
}
stopCluster(cl)

data(stopwords_en)

memory.limit(size=30000)
#2.2 build textmatric from tempfile
myMatrix = textmatrix(td,stopwords=stopwords_en,stemming=TRUE,language="english", minWordLength=1)
myLSAspace = lsa(myMatrix, dims=dimcalc_share())
myNewMatrix = as.textmatrix(myLSAspace)
myNewMatrix_2 = myNewMatrix



#3.same as 2 with small reviews only(nchar<140 characters)

known_f_matr_s <- reviews[nchar(reviews$review_text, type = "chars", allowNA = FALSE, keepNA = NA)<145,]
#for now, we use only 200 reviews
known_f_matr_s <- known_f_matr_s[sample(1:nrow(known_f_matr_s), 200,replace=FALSE),]

known_f_matr_s_g <- known_f_matr_s[known_f_matr_s$is_good==1,]
known_f_matr_s_b <- known_f_matr_s[known_f_matr_s$is_good==0,]
td = tempfile()
dir.create(td)

#3.1 add each review and line id to tempfile
#setup parallel backend to use 4 processors
cl<-makeCluster(4)
registerDoParallel(cl)

#start time
strt<-Sys.time()
foreach(k= 1:NROW(known_f_matr_s)) %do% {
  
  x<-iconv(toString(known_f_matr_s[k,2]), "latin1", "ASCII", sub="")
  write( x, file=paste(td, paste(toString(known_f_matr_s[k,1]), toString(known_f_matr_s[k,3]), sep="_"), sep="/"))
  
  print(Sys.time()-strt)
  k<-k+1
}
stopCluster(cl)

data(stopwords_en)
data(specialchars)
memory.limit(size=30000)
#3.2 build textmatrix from tempfile
myMatrix = textmatrix(td,stopwords=stopwords_en,stemming=FALSE,language="english", minWordLength=1)
myLSAspace = lsa(myMatrix, dims=dimcalc_share(share=0.5))
myLSAspace_2dim = lsa(myMatrix, dims=5)
myNewMatrix = as.textmatrix(myLSAspace)
myNewMatrix_2dim = as.textmatrix(myLSAspace)
#locations <-myLSAspace$dk %*% diag(myLSAspace$sk)

#plot(locations,type="n",xlim=c(-1,1),ylim=c(-1,1))
#text(locations, labels=rownames(myLSAspace$dk))

locations_docs <-myLSAspace_2dim$dk %*% diag(myLSAspace_2dim$sk)
plot(locations_docs,type="n",xlim=c(-1,2),ylim=c(-1,2))
text(locations_docs, labels=rownames(myLSAspace_2dim$dk))

# this is where the messy area starts
locations_df <- data.frame(locations_docs)


ggplot(locations_docs_df, aes(x=locations_docs_df$X1, y=locations_docs_df$X2, fill=locations_docs_df$X3, color=locations_docs_df$X4, size=locations_docs_df$X5^2)) +
  geom_point(shape=21) +
  scale_color_gradient(low="red", high="green") +
  scale_size_continuous(range=c(1,12))



labels(locations_docs_df[locations_docs_df$X2<=-0.5,])[[1]]

locations_docs_df[locations_docs_df$X1>-0.9,]
known_f_matr_s[known_f_matr_s$line_ID==35400,]

myNewMatrix_2 = myNewMatrix
known_f_matr_s[labels(myLSAspace_2dim$dk)[[1]]==known_f_matr_s$line_ID,]$is_good
known_f_matr_s[ labels(myLSAspace_2dim$dk)[[1]] %in% known_f_matr_s$line_ID]

merge(data frameA,data frameB,by.x=,by.y="ID")
#3.3 calc associations for good & bad
associate(myNewMatrix_2dim, "nice")
# this approach doesn't work, only shows semantic proximity to a single word of choice based on input documents
# can't turn it into a useful feature because the searchword must be part of the words from the documents.
# idea was to identify a set of terms with similar meaning to "good", then scan every document again for occurence of words from this set and
# create a weight as a feature: check each document for each term from the list, count Nr of ocurrences multiply that with each term's specific 
# proximity to "good". do the same for "bad"
myNewMatrix
print.textmatrix(myMatrix,1500,10)
cosine(myNewMatrix)
nchar(known_f_matr$text, type = "chars", allowNA = FALSE, keepNA = NA)<145
insp_mat <- data.frame(myMatrix)


subset(reviews, reviews$text %in% c("restaurant"))
rm(td)
  
toupload <- sreviews[sample(1:nrow(sreviews), 50000,replace=FALSE),]
toupload$text = prep_fun(toupload$text)

useful funny cool   type n_rating is_good line_ID

review_id || rating || binary_rating || review_text

toupload$user_id <- NULL
toupload$date <- NULL
toupload$date <- NULL
toupload$useful <- NULL
toupload$funny <- NULL
toupload$cool <- NULL
toupload$business_id <-NULL
toupload$type <-NULL
toupload$n_rating <-
  toupload$rating <- toupload$stars
toupload$stars <- NULL
toupload$binary_rating <- toupload$is_good
toupload$is_good <- NULL
toupload$line_ID <- NULL
toupload$review_text <- toupload$text
toupload$text <- NULL
setcolorder(toupload, c("review_id", "rating", "binary_rating","review_text"))


saveRDS(toupload, file = "./yelp_reviews.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
reviews <- toupload
