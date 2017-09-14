sizelist <- c(100000,50000,20000,10000,5000,2500,1000)
dsnamelist <- c("amazonBooks", "amazonfinefood","imdb")#,"twitter","yelp" )
approachlist <- c("mix", "foldin" )


i=1
j=1
k=1


for (i in 1:NROW(sizelist)){
  
  for (j in 1:NROW(dsnamelist) ){
    
    for (k in 1:NROW(approachlist)){
      ds_name <- dsnamelist[j]
      size <- sizelist[i]
      approach <- approachlist[k]
      
      if (!(ds_name =="imdb" && size<20000)){
        print(paste(size, ds_name, approach, sep="/"))
        source("fe_LSA.R")
      }
      
      #clean workspace except for stuff needed here
      rm(list=ls()[! ls() %in% c("sizelist","dsnamelist","approachlist","i","j","k")])
      
      
    }
    
  }
  
}