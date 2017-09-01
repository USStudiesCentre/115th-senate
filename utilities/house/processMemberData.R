## build list of members
## check for duplicates and drop any duplicates
processMemberData <- function(data){
  require(RMySQL)
  drv <- dbDriver("MySQL")
  con <- dbConnect(drv,group="ideal")

  memberData <- getMemberData()
  if(!is.null(memberData)){
    ## have we found any new members (after checking for duplicates)
    oldN <- dim(memberData)[1]
    ##print(names(memberData))
    ##print(names(data))
    if(all(names(memberData)==names(data))){
      tmpData <- rbind(memberData,data)
      tmpData <- dropDupMemberRecs(tmpData)   ## check for duplicates?
      if(dim(tmpData)[1]>oldN){
        cat(paste("remaking h112Member table with",
                  dim(tmpData)[1],
                  "records\n"))
        res <- dbWriteTable(conn=con,
                            name="h112Member",
                            value=as.data.frame(tmpData),
                            row.names=FALSE,
                            overwrite=TRUE)
      }
    }
  } else {
    ## we have no member data, so write it
    cat("creating h112Member table\n")
    res <- dbWriteTable(conn=con,
                        name="h112Member",
                        value=data,
                        row.names=FALSE)
  }
  dbDisconnect(con)
  invisible(NULL)
}

dropDupMemberRecs <- function(memberData){
  idString <- paste(memberData$nameid,
                    memberData$party,
                    memberData$state,
                    sep=":")
  ok <- !(duplicated(idString))
  return(memberData[ok,])
}


getMemberData <- function(congress=112,chamber="House"){
  require(RMySQL)
  drv <- dbDriver("MySQL")
  con <- dbConnect(drv,group="ideal")

  memberData <- NULL

  ## do we have a member table?
  prefix <- "h"
  if(chamber=="Senate")
    prefix <- "s"
  theTable <- paste(prefix,congress,"Member",sep="")
  haveMemberData <- haveTable(theTable)
  
  if(haveMemberData){
    query <- paste("SELECT * FROM",theTable,sep=" ")
    res <- dbSendQuery(con,statement=query)
    memberData <- fetch(res,n=-1)
    names(memberData) <- gsub(names(memberData),
                              pattern="_",
                              replacement="-")
    dbClearResult(res)
  }

  dbDisconnect(con)
  
  return(memberData)
}

## check that we have data on all members from NYTimes
legisDataCheck <- function(congress=112,chamber="House"){
  legisData <- getMemberData(congress=congress,chamber=chamber)

  ## info from NYTimes
  con <- dbConnect(drv,group="ideal")
  query <- "SELECT * from nyTimesMemberInfo"
  res <- dbSendQuery(con,statement=query)
  nyTimes <- fetch(res,n=-1)
  dbClearResult(res)
  dbDisconnect(con)

  d <- setdiff(legisData$nameid,
               nyTimes$nameid)
  n <- length(d)
  if(n>0){
    cat(paste("will obtain the following",n,"records:\n"))
    print(d)
    
    n <- length
  }
  return(invisible(NULL))
}

###########################################################
## nytimes extraction functions
extractNYTimesMemberData <- function(id,congress=112,chamber="House"){
  nyTimesKey <- "0e350ae736cbb9849696c73cff1f445a:0:25021445"
  baseURL <- "http://api.nytimes.com/svc/politics/v3/us/legislative/congress/members/MEMBERID.xml?api-key=APIKEY"
  theURL <- gsub(baseURL,pattern="MEMBERID",replacement=id)
  theURL <- gsub(theURL,pattern="APIKEY",replacement=nyTimesKey)
  
  tmpFile <- tempfile()

  ## test the URL
  con <- try(url(theURL,open="r"),silent=TRUE)
  if(inherits(con,"try-error")){
    cat("couldn't connect\n")
    return(NULL)
  }
  close(con)
  download.file(url=theURL,destfile=tmpFile)
  foo <- xmlTreeParse(file=tmpFile,
                      useInternalNodes=FALSE)

  congresses <- list()
  extractCong <- function(x){
    congresses[[length(congresses)+1]] <<- xmlValue(x)
  }
  chambers <- list()
  extractChambers <- function(x){
    chambers[[length(chambers)+1]] <<- xmlValue(x)
  }
  xmlTreeParse(file=tmpFile,
               handlers=list("congress"=extractCong,"chamber"=extractChambers))
  theGoodRole <- 1
  if(length(unlist(congresses))>1){
    str <- cbind(unlist(congresses),
                 unlist(chambers))
    print(str)
    ok <- str[,1]==congress & str[,2]==chamber
    if(sum(ok)>0){
      theGoodRole <- which.max(ok)
    }
  }
  tmp <- xmlApply(foo$doc$children[["result_set"]][["results"]][["member"]][["roles"]][[theGoodRole]],
                  xmlValue)
  
  memberInfo <- foo$doc$children[["result_set"]][["results"]][["member"]]
  memberInfo <- memberInfo[-match("roles",names(memberInfo))]
  memberInfo <- lapply(memberInfo,xmlValue)
  memberInfo$nameid <- memberInfo$id
  memberInfo$id <- NULL

  vals <- c(memberInfo,tmp)
  
  vals$times_topics_url <-  NULL
  vals$times_tag <- NULL
  vals$most_recent_vote <- NULL

  vals$committees <- NULL
  
