parseSenateRollCall <- function(vn){
  i <- as.numeric(vn)
  ##if(checkForRollCall(session,i)){
    ##cat(paste("we appear to already have",
    ##          "rollcall",i,
    ##          "for congress",congress,
    ##          "session",session,
    ##          "in chamber",chamber,
    ##          "\n"))
  ##  return(invisible(NULL))
  ##}

  theURL <- paste("https://www.senate.gov/legislative/LIS/roll_call_votes/vote",
                  congress,session,
                  "/",
                  "vote_",
                  congress,"_",
                  session,"_",
                  vn,".xml",
                  sep="")
  tmpFile <- tempfile()
  ## test the URL
  con <- try(url(theURL,open="r"),silent=TRUE)
  if(inherits(con,"try-error")){
    cat("couldn't connect\n")
    return(NULL)
  }
  close(con)
  download.file(url=theURL,destfile=tmpFile)
  require(XML)
  foo <- xmlTreeParse(file=tmpFile,
                      useInternalNodes=FALSE)

  ## vote meta-data
  rawMetaData <- foo$doc$children$roll_call_vote
  len <- unlist(xmlApply(rawMetaData,length))
  ok <- len==1   
  ## take all atoms
  metaData <- lapply(rawMetaData[ok],xmlValue)
  metaData <- c(metaData,
                xmlSApply(rawMetaData[["amendment"]],xmlValue),
                xmlSApply(rawMetaData[["count"]],xmlValue),
                xmlSApply(rawMetaData[["tie_breaker"]],xmlValue))
  metaData$tie_breaker <- metaData$by_whom
  metaData$by_whom <- NULL
  metaData$source <- theURL
  metaData$rollcall.num <- metaData$vote_number
  metaData$vote_number <- NULL
  metaData <- lapply(metaData,
                     function(x){
                       if(length(x)==0){
                         return(NA)
                       } else {
                         return(x)
                       }
                     }
                     )
  names(metaData) <- gsub(names(metaData),pattern="_",replacement=".")
  metaData <- as.data.frame(metaData)
  
  ## parse vote data
  positionExtractor <- function(x){
    nms <- xmlSApply(x,xmlName)
    out <- unlist(xmlSApply(x,xmlValue))
    out <- as.vector(out)
    names(out) <- nms
    positions <<- rbind(positions,out)
    return(invisible(NULL))
  }
  
  positions <- NULL
  xmlTreeParse(file=tmpFile,handlers=list("member"=positionExtractor))
  unlink(tmpFile)
  positions <- data.frame(positions,row.names=positions[,1],stringsAsFactors=FALSE)
  
  positions$congress <- metaData$congress
  positions$session <- metaData$session
  positions$rollcall.num <- metaData$rollcall.num
  names(positions) <- gsub(names(positions),pattern="_",replacement=".")
  positions <- as.data.frame(positions)
  
  ## clean before pushing
  tab <- theTable()
  metaTab <- paste(tab,"meta",sep="")
  ##positions <- cleanBeforePushing(positions,tab)
  ##metaData <- cleanBeforePushing(metaData,metaTab)
  print(names(positions))
  print(names(metaData))
  
  ## now push to SQL server
  ##pushTab(tab,positions)
  ##pushTab(metaTab,metaData)
  
  ##return(invisible(NULL))
  
  return(positions)
}

## list of Senate roll calls
theURL <- paste("https://www.senate.gov/legislative/LIS/roll_call_lists/vote_menu_",
                congress,"_",
                session,
                ".xml",sep="")
con <- try(url(theURL,open="r"),silent=TRUE)
if(inherits(con,"try-error")){
  cat("couldn't connect\n")
} else {
  close(con)
  tmpFile <- tempfile()
  download.file(url=theURL,destfile=tmpFile)

  library(XML)
  votes <- NULL
  foo <- xmlTreeParse(file=tmpFile,
                      handlers=list("vote_number"=function(x){
                        votes <<- c(votes,xmlValue(x))
                      }
                        ),
                      useInternalNodes=FALSE)
  unlink(tmpFile)
  out <- list()
  counter <- 1
  for(vn in votes){
    cat(paste("processing vote",vn,"\n"))
    out[[counter]] <- parseSenateRollCall(vn)
    counter <- counter + 1
  }
}

out2 <- plyr::rbind.fill(out)

## make a roll call matrix
NVOTES <- length(out)
theLegis <- sort(unique(out2$member.full))
NLEGIS <- length(theLegis)
rc <- matrix(NA,NLEGIS,NVOTES)
for(j in 1:NVOTES){
  theIndex <- match(out[[j]]$member.full,theLegis)
  rc[theIndex,j] <- out[[j]]$vote.cast 
}
dimnames(rc) <- list(theLegis,1:NVOTES)

z <- matrix(NA,NLEGIS,NVOTES)
z[rc=="Yea"] <- 1
z[rc=="Nay"] <- 0
dimnames(z) <- dimnames(rc)

legis.data <- data.frame(names=theLegis,
                         party=out2$party[match(theLegis,out2$member.full)])


