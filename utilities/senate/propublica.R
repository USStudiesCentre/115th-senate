#############################
## propublica API
##
## simon jackman
## august 2016
#############################
library(RCurl)
key <- "z7naOWEkzB4tQlsaPccN216bsjhnGVBt7a4IcBoi"

baseURL <- "https://api.propublica.org/congress/v1/senate/votes"
v <- list()
counter <- 1
for(y in c(2015,2016)){
  for(m in 1:12){
    if(m<10){
      str <- paste("0",m,sep="")
    } else {
      str <- as.character(m)
    }
    url <- paste(baseURL,y,paste(str,".json",sep=""),sep="/")
    print(url)
    v[[counter]] <- getURL(url,httpheader=c("X-API-Key"=key))
    counter <- counter + 1
  }
}

v2 <- lapply(v,function(x){try(RJSONIO::fromJSON(x))})
v2 <- lapply(v2,function(x)RJSONIO::toJSON(x))
v2 <- lapply(v2,function(x){try(jsonlite::fromJSON(x))})
v2 <- lapply(v2,
             function(x){
               x$results$votes$democratic <- NULL
               x$results$votes$republican <- NULL
               x$results$votes$independent <- NULL
               x$results$votes$total <- NULL
               return(as.data.frame(x$results$votes))
             })
v2 <- plyr::rbind.fill(v2)
v2 <- v2[order(v2$session,as.numeric(v2$roll_call)),]
save("v2",file="data/v2.rda")

######################################################
## get the votes
getProPublicaVote <- function(obj){
  out <- NULL
  tmp <- try(RJSONIO::fromJSON(RCurl::getURL(obj$vote_uri,
                                       httpheader=c("X-API-Key"=key))
                                ),
             silent=TRUE)
  if(!inherits(tmp,"try-error")){
    out <- tmp$results$votes$vote$positions
    out <- lapply(zzz,
                  function(x){
                    tmp <- data.frame(a=x[1],b=x[2],c=x[3])
                    names(tmp) <- names(x)
                    return(tmp)
                  }
    )
    out <- plyr::rbind.fill(out)
  }
  return(out)
}

if(file.exists("data/votes.RData")){
  load("data/votes.RData")
} else {
  votes <- list()
}

for(i in 1:nrow(v2)){
  print(i)
  if(is.null(votes[i][[1]])){
    print(v2[i,])
    votes[[i]] <- getProPublicaVote(v2[i,])
    votes[[i]]$session <- v2$session[i]
    votes[[i]]$roll_call <- v2$roll_call[i]
    votes[[i]]$date <- v2$date[i]
    votes[[i]]$time <- v2$time[i]
  }
}

save("votes",file="data/votes.RData")



