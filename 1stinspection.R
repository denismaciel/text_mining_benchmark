
library(jsonlite)
#1. load and process data
reviews <- stream_in(file("./yelp_academic_dataset_review.json"),pagesize = 10000)


require(data.table) 
setDT(reviews)


saveRDS(reviews, file = "./all_yelp_reviews.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

reviews <- readRDS("./all_yelp_reviews.rds", refhook = NULL)




sreviews <- reviews[sample(1:nrow(reviews), 120,replace=FALSE),]

library(SciencesPo)
library(lsa)
if(!require("foreach")) install.packages("foreach"); library("foreach")
if(!require("doParallel")) install.packages("doParallel"); library("doParallel")


sreviews$n_rating <- Normalize(sreviews$stars, range=c(0,1))

#good reviews: stars>3, bad : stars<3
good_sreviews <- sreviews[sreviews$stars>3,]
bad_sreviews <- sreviews[sreviews$stars<3,]
good_sreviews$is_good <- 1
bad_sreviews$is_good <- 0
sreviews <- rbind(good_sreviews, bad_sreviews)

#shuffle rows
sreviews <- sreviews[sample(nrow(sreviews)),]

#make subset for testing, add line_ID
sreviews$line_ID <- 1:nrow(sreviews)
# split known / class 70/30
a <- round((max(sreviews$line_ID)*70)/100)
b <- round(max(sreviews$line_ID))
known <- sreviews[1:a,]
class <- sreviews[round(max(known$line_ID)+1):max(sreviews$line_ID),]

class$stars <- NULL
class$n_rating <- NULL
class$is_good <- NULL

#2. create matrices for lsa
known_f_matr <- known[,c("line_ID", "text")]
td = tempfile()
dir.create(td)

#2.1 add each review and line id to tempfile
#setup parallel backend to use 4 processors
cl<-makeCluster(4)
registerDoParallel(cl)

  #start time
strt<-Sys.time()
foreach(k= 1:NROW(known_f_matr)) %do% {
  
  x<-iconv(toString(known_f_matr[k,2]), "latin1", "ASCII", sub="")
  write( x, file=paste(td, toString(known_f_matr[k,1]), sep="/"))
  
  print(Sys.time()-strt)
  k<-k+1
}
stopCluster(cl)
data(stopwords_fr)
data(stopwords_en)
data(specialchars)
memory.limit(size=30000)
#2.2 build textmatric from tempfile
myMatrix = textmatrix(td,language="english", minWordLength=1)
myLSAspace = lsa(myMatrix, dims=dimcalc_share())
myNewMatrix = as.textmatrix(myLSAspace)
myNewMatrix_2 = myNewMatrix
#2.3 calc associations for good & bad
associate(myNewMatrix, "good")
associate(myNewMatrix_2, "bad")


