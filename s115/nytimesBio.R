## scrape bio information from NYTimes

## get list of ids from master list
## do this by looping over state.abb

extractMemberData <- function(foo){
  require(XML)
  tmp <- foo$doc$children[["result_set"]][["results"]][["member"]]
  vals <- xmlSApply(tmp,xmlValue)
  vals$roles <- NULL
  vals <- c(vals,
            xmlSApply(tmp[["roles"]][[1]],xmlValue))
  vals$times_topics_url <-  NULL
  vals$times_tag <- NULL
  vals$most_recent_vote <- NULL
  ##vals$missed_votes_pct <- NULL
  ##vals$votes_with_party_pct <- NULL
  ##vals$bills_sponsored <- NULL
  ##vals$bills_cosponsored <- NULL
  vals$url_1 <- NULL
  vals$url.1 <- NULL
  vals$rss_url <- NULL
  vals$committees <- NULL
  vals$nameid <- vals$id
  vals$id <- NULL

  ## get class
  vals$class <- as.integer(gsub(vals$title,
                                pattern="^.*([1-3]).*$",
                                replacement="\\1"))
  vals$title <- strsplit(as.character(vals$title),
                         split=",")[[1]][1]
  ## NULL to NA
  vals <- lapply(vals,
                 function(x){
                   if(length(x)==0)
                     x <- NA
                   return(x)
                 })
  out <- as.data.frame(vals)
  return(out)
}


processBioURI <- function(uri){
  require(XML)
  theURL <- paste(uri,"?api-key=",key,sep="")
  foo <- try(xmlTreeParse(file=theURL,
                          isURL=TRUE,
                          useInternalNodes=FALSE),
             silent=TRUE)
  out <- NULL
  if(!inherits(foo,"try-error")){
    out <- extractMemberData(foo)
  }
  return(out)
}

key <- "0e350ae736cbb9849696c73cff1f445a:0:25021445"
baseURL <- "http://api.nytimes.com/svc/politics/v3/us/legislative/congress/114/senate/members.xml?&state=STATE&api-key=APIKEY" 

out <- list()

library(XML)
for(s in state.abb){
  cat(paste("looking up senators for state",s,"\n"))

  theURL <- gsub(baseURL,pattern="STATE",replacement=s)
  theURL <- gsub(theURL,pattern="APIKEY",replacement=key)

  foo <- try(xmlTreeParse(file=theURL,
                          isURL=TRUE,
                          useInternalNodes=FALSE),
             silent=TRUE)
  if(!inherits(foo,"try-error")){
    memberInfo <- foo$doc$children[["result_set"]][["results"]]["members",all=TRUE][[1]]
    uris <- xmlSApply(memberInfo,function(x)xmlValue(x[["api_uri"]]))
    cat(paste("we found",length(uris),"uris\n"))
    print(uris)
    n <- length(uris)
    for(i in 1:n){
      tmp <- processBioURI(uris[i])
      if(!is.null(tmp)){
        print(tmp)
        cat("writing to mysql\n")
        library(RMySQL)
        drv <- dbDriver("MySQL")
        con <- dbConnect(drv,group="ideal")
        dbWriteTable(conn=con,
                     name="nyTimesMemberInfo",
                     value=tmp,
                     row.names=FALSE,
                     append=TRUE)
        dbDisconnect(con)
      }
    }
  }
}

