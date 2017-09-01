## api keys
key <- list() 
key$nyt <- "8e11649ef99d4f86a41cb02423ff5454"
key$sunlight <- "c932e5da9a0d4671ac975b3bc6678dc9"
key$propublica <- "z7naOWEkzB4tQlsaPccN216bsjhnGVBt7a4IcBoi"

year <- format(Sys.time(),"%Y")

maketh <- function(number){
  n <- nchar(as.character(number))
  lastDigit <- substr(as.character(number),n,n)
  suffix <- switch(EXPR=lastDigit,
                   "1"="st",
                   "2"="nd",
                   "3"="rd",
                   "th")
  return(paste(number,suffix,sep=""))
}

getSession <- function(year=NULL){
  if(is.null(year)){
    year <- format(Sys.time(),"%Y")
  }
  
  dayOfYear <- format(Sys.time(),"%j")
  dayOfYear <- as.numeric(dayOfYear)
  
  yearMod2 <- as.numeric(year)%%2
  evenYear <- yearMod2 == 0
  
  session <- 2 - yearMod2
  if(!evenYear & dayOfYear < 3){
    session <- 2
  }
  
  return(session)
}

session <- getSession()
theSession <- c("1st","2nd")[session]

########################################
## do we have necessary globals defined?
errorCheck <- function(){
  haveChamber <- exists("chamber")
  if(!haveChamber){
    stop("chamber not defined")
  }
  haveCongress <- exists("congress")
  if(!haveCongress){
    stop("congress not defined")
  }
  return(invisible(NULL))
}

#################################
## sql utilities
haveTable <- function(tab){
  require(RMySQL)
  drv <- dbDriver("MySQL")
  con <- dbConnect(drv,group="ideal")
  out <- tab %in% dbListTables(con)
  dbDisconnect(con)
  return(out)
}

## build list of members
## check for duplicates and drop any duplicates
getMemberData <- function(){
  errorCheck()
  require(RMySQL)
  drv <- dbDriver("MySQL")
  con <- dbConnect(drv,group="ideal")
  
  memberData <- NULL
  
  ## do we have a member table?
  haveMemberData <- haveTable(theTable())
  
  if(haveMemberData){
    query <- paste("SELECT DISTINCT nameid as nameid FROM ",
                   theTable(),
                   " GROUP BY nameid")
    res <- dbSendQuery(con,statement=query)
    memberData <- fetch(res,n=-1)
    dbClearResult(res)
  }
  dbDisconnect(con)
  return(memberData)
}

processMemberData <- function(data){
  errorCheck()
  require(RMySQL)
  drv <- dbDriver("MySQL")
  con <- dbConnect(drv,group="ideal")

  memberData <- getMemberData()
  if(!is.null(memberData)){
    newMembers <- setdiff(data$nameid,memberData$nameid)
    if(length(newMembers)>0){
      cat("we seem to have new members\n")
      print(newMembers)
    }
  }
  dbDisconnect(con)
  return(invisible(NULL))
}

dropDupMemberRecs <- function(memberData){
  idString <- paste(memberData$nameid,
                    memberData$party,
                    memberData$state,
                    sep=":")
  ok <- !(duplicated(idString))
  return(memberData[ok,])
}

theTable <- function(){
  prefix <- "h"
  errorCheck()
  if(chamber=="Senate")
    prefix <- "s"
  theTable <- paste(prefix,congress,sep="")
  return(theTable)
}

## master list of rollcalls
lastRollCall <- function(year){
  theURL <- paste("http://clerk.house.gov/evs/",year,"/index.asp",sep="")
  require(XML)
  foo <- htmlTreeParse(file=theURL,
                       isURL=TRUE)
  rc <- xmlSApply(foo$children$html[["body"]][["table"]],
                  function(x)xmlSApply(x,xmlValue)[[1]])

  ow <- options("warn")
  options(warn=-1)
  rc <- max(as.numeric(rc),na.rm=TRUE)
  options(ow)

  return(rc)
}


cleanBeforePushing <- function(data,tab){
  if(haveTable(tab)){
    ## get column names from table we're adding to
    require(RMySQL)
    drv <- dbDriver("MySQL")
    con <- dbConnect(drv,group="ideal")
    nm <- dbListFields(conn=con,name=tab)
    nm <- gsub(nm,pattern="_",replacement=".",fixed=TRUE)
    dbDisconnect(con)

    bad <- setdiff(names(data),nm)
    n <- length(bad)
    if(n>0){
      cat(paste("cleanBeforePushing: dropping the following vars from",
                "data to be added to",
                tab,
                "\n"))
      print(bad)
      for(i in 1:n){
        dropThis <- match(bad[i],names(data))
        data[,dropThis] <- NULL
      }
    }
    
    ## change period to underscore
    names(data) <- gsub(names(data),pattern=".",
                        replacement="_",
                        fixed=TRUE)
  }
  return(data)
}

## parse data returned by clerk's site for 
## specific roll call
parseFunction <- function(theURL){
  cat(paste("parsing",theURL,"\n"))
  require(XML)
  currentTime <- Sys.time()

  con <- try(url(theURL,open="r"),silent=TRUE)
  if(inherits(con,"try-error")){
    cat("couldn't connect\n")
    return(invisible(NULL))
  }
  close(con)
  tmpFile <- tempfile()
  download.file(url=theURL,destfile=tmpFile)
  foo <- xmlParse(file=tmpFile)
  
  ## does this actually contains votes?
  votes <- unlist(xpathApply(foo,path="//recorded-vote/vote",xmlValue))
  if(length(votes)==0){
    return(invisible(NULL))
  }

  ## other data
  tmpData <- data.frame(nameid=unlist(xpathApply(foo,
                                              path="//legislator",
                                              xmlGetAttr,"name-id")),
                     party=unlist(xpathApply(foo,
                                             path="//legislator",
                                             xmlGetAttr,"party")))
  

  if(length(votes)==nrow(tmpData)){
    tmpData$vote <- votes
  }
  
  ## process member data, informative only
  processMemberData(tmpData)
  
  ## vote meta-data
  foo2 <- xmlTreeParse(file=tmpFile,isURL=FALSE)
  metaData <- xmlSApply(foo2$doc$children[[2]][[1]],xmlValue)
  nRecs <- length(metaData)
  metaData <- metaData[-nRecs]
  metaData <- as.data.frame(t(metaData))

  ## convert "committee" to "chamber"
  if("committee" %in% names(metaData)){
    names(metaData)[grep("committee",names(metaData))] <- "chamber"
  }

  ## vote totals
  metaData2 <- xmlToDataFrame(getNodeSet(foo,"//totals-by-vote"))
  metaData2$"total-stub" <- NULL
  names(metaData2) <- c("yea","nay","present","notvoting")
  
  ## meta meta data (!)
  metaData$source <- theURL
  metaData$timestamp <- currentTime
  
  ## make sure no elements of metaData are lists
  metaData <- lapply(metaData,
                     function(x){
                       if(class(x)[1]=="list")
                         x <- as.vector(unlist(x))
                       if(length(x)==0)
                         x <- NA
                       return(x)
                     })

  metaData <- as.data.frame(metaData)
  metaData <- cbind(metaData,metaData2)
  
  ## add some meta-data to microdata
  tmpData$congress <- metaData$congress
  tmpData$session <- metaData$session
  tmpData$rollcall.num <- metaData$rollcall.num

  ## clean before pushing
  tab <- theTable()
  metaTab <- paste(tab,"meta",sep="")
  tmpData <- cleanBeforePushing(tmpData,tab)
  metaData <- cleanBeforePushing(metaData,metaTab)
  
  cat(paste("parseFunction: names of variable being added to",tab,"\n"))
  print(names(tmpData))
  cat(paste("parseFunction: names of variable being added to",metaTab,"\n"))
  print(names(metaData))
  
  ## now push to SQL server
  pushTab(tab,tmpData)
  pushTab(metaTab,metaData)

  unlink(tmpFile)
  return(invisible(NULL))
}

pushTab <- function(tabName,data){
  require(RMySQL)
  drv <- dbDriver("MySQL")
  con <- try(dbConnect(drv,group="ideal"),silent=TRUE)
  if(inherits(con,"try-error")){
    cat(paste("could not connect to data base"))
  } else {
    if(haveTable(tabName)){
      cat(paste("appending",
                nrow(data),
                "records to",tabName,"..."))
      res <- dbWriteTable(conn=con,
                          name=tabName,
                          value=data,
                          row.names=FALSE,
                          append=TRUE)
    } else {
      cat(paste("creating table",tabName,"with fields:\n"))
      print(names(data))
      res <- dbWriteTable(conn=con,
                          name=tabName,
                          value=data,
                          row.names=FALSE,
                          overwrite=TRUE)
    }
    cat("done\n")
    print(res)
    dbDisconnect(con)
  }
  return(invisible(NULL))
}

checkForRollCall <- function(session,rollcall){
  errorCheck()
  tab <- theTable()
  metaTab <- paste(tab,"meta",sep="")

  out <- 0

  if(haveTable(metaTab)){
    require(RMySQL)
    drv <- dbDriver("MySQL")
    con <- dbConnect(drv,group="ideal")

    query <- paste("SELECT * from ",metaTab,
                   " where session='",session,"'",
                   " and ",
                   " rollcall_num=",rollcall,
                   sep="")
    
    res <- dbSendQuery(con,statement=query)
    data <- fetch(res,n=-1)
    dbClearResult(res)
    dbDisconnect(con)
    if(dim(data)[1L]>0)
      out <- 1
  }
  return(out)
}

## check that we have data on all members
legisDataCheck <- function(){
  errorCheck()
  legisData <- getMemberData()
  
  ## bioinfo
  if(!haveTable("memberInfo")){
    return(invisible(NULL))
  }
  require(RMySQL)
  con <- dbConnect(drv,group="ideal")
  query <- paste("SELECT nameid from memberInfo ",
                 "WHERE congress = ",congress,
                 " ",
                 "AND chamber = '",chamber,"'",
                 sep="")
  res <- dbSendQuery(con,statement=query)
  memberInfo <- fetch(res,n=-1)
  dbClearResult(res)
  dbDisconnect(con)
  
  ## missing members info
  d <- setdiff(legisData$nameid,
               memberInfo$nameid)
  n <- length(d)
  if(n>0){
    cat(paste("will look for the following",n,"records:\n"))
    print(d)
    
    ## get column names
    require(RMySQL)
    con <- dbConnect(drv,group="ideal")
    query <- "select * from memberInfo limit 0,1"
    res <- dbGetQuery(con,query)
    existingNames <- names(res)
    dbDisconnect(con)
    
    for(id in d){   ## loop over new records
      system("sleep 5")
      out <- extractSunlightMemberData(id)
      if(!is.null(out)){  
        ## fix column names
        out$nameid <- out$bioguide_id
        out$bioguide_id <- NULL
        out$date_of_birth <- out$birthday
        out$birthday <- NULL
        out$start_date <- out$term_start
        out$term_start <- NULL
        out$end_date <- out$term_end
        out$term_end <- NULL
        out$current_party <- out$party
        out$congress <- congress
        if(out$chamber=="house"){
          out$chamber <- "House"
          out$title <- "Representative"
        }
        if(out$chamber=="senate"){
          out$chamber <- "Senate"
        }
        keepCols <- !is.na(match(names(out),existingNames))
        out <- out[,keepCols]
        print(out)

        ## push to mysql table
        require(RMySQL)
        con <- dbConnect(drv,group="ideal")
        res <- dbWriteTable(conn=con,
                            name="memberInfo",
                            value=out,
                            row.names=FALSE,
                            append=TRUE)
        print(res)
        dbDisconnect(con)
      }
    }
  } else {
    cat("all nameids from vote data found in memberInfo\n")
  }
  return(invisible(NULL))
}

###########################################################
## sunlight foundation single member info
extractSunlightMemberData <- function(id){
  require(RMySQL)
  drv <- dbDriver("MySQL")
  con <- dbConnect(drv,group="ideal")
  query <- "select * from memberInfo limit 0,1"
  res <- dbGetQuery(con,query)
  existingNames <- names(res)
  dbDisconnect(con)
  
  baseURL <- "https://congress.api.sunlightfoundation.com"
  require(jsonlite)
  theURL <- paste(baseURL,
                  paste("/legislators?bioguide_id=",id,sep=""),
                  paste("&apikey=",key$sunlight,sep=""),
                  sep="")
  print(theURL)

  out <- NULL
  res <- try(fromJSON(theURL),silent = TRUE)
  if(!inherits(res,"try-error")){
    if(res$count==1){
      out <- res$results
    }
  }
  return(out)
}

###########################################################
## nytimes extraction functions, single member
extractNYTimesMemberData <- function(id){

  require(RMySQL)
  drv <- dbDriver("MySQL")
  con <- dbConnect(drv,group="ideal")
  query <- "select * from memberInfo limit 0,1"
  res <- dbGetQuery(con,query)
  existingNames <- names(res)
  dbDisconnect(con)

  ## now do the NYTimes lookup
  baseURL <- "http://api.nytimes.com/svc/politics/v3/us/legislative/congress/members/MEMBERID.xml?api-key=APIKEY"
  theURL <- gsub(baseURL,pattern="MEMBERID",replacement=id)
  theURL <- gsub(theURL,pattern="APIKEY",replacement=key)

  ## test the URL
  require(XML)
  flag <- TRUE
  while(flag){
    foo <- try(xmlParse(file=theURL,
                        isURL=TRUE),
               silent=TRUE)
    if(inherits(foo,"try-error")){
      cat("sleeping...")
      system("sleep 3")
      cat("done\n")
    } else {
      flag <- FALSE
    }
  }

  ## parse output
  xpathApply(foo,"//committees",removeNodes)
  roleData <- getNodeSet(foo,"//role[1]")
  xpathApply(foo,"//roles",removeNodes)
  bioData <- getNodeSet(foo,"//member")
  out <- data.frame(c(xmlToDataFrame(bioData),
                     xmlToDataFrame(roleData)))
  out$nameid <- out$id
  tmp <- out

  ## columns to drop
  delta <- setdiff(names(tmp),existingNames)
  if(length(delta>0)){
    print(delta)
    tmp <- tmp[,-match(delta,names(tmp))]
  }
  
  ## columns to pad
  delta <- setdiff(existingNames,names(tmp))
  if(length(delta>0)){
    print(delta)
    n <- dim(tmp)[1]
    for(d in delta){
      tmp[[d]] <- rep(NA,n)
    }
  }
    
  return(tmp)
}

## form a roll call matrix
makeRollCallObject <- function(congress,chamber){
  tab <- theTable()
  metaTab <- paste(tab,"meta",sep="")
  
  require(RMySQL)
  drv <- dbDriver("MySQL")
  con <- dbConnect(drv,group="ideal")
  
  query <- paste("SELECT * from",tab)
  res <- dbSendQuery(con,statement=query)
  data <- fetch(res,n=-1)
  dbClearResult(res)
  dbDisconnect(con)

  if(!is.null(data[["vote_cast"]]) & is.null(data[["vote"]])){
    data[["vote"]] <- data$vote_cast
    data$vote_cast <- NULL
  }
    
  data$numericVote <- rep(NA,dim(data)[1])
  data$numericVote[data$vote=="Yea"] <- 1
  data$numericVote[data$vote=="Nay"] <- 0
  data$numericVote[data$vote=="Aye"] <- 1
  data$numericVote[data$vote=="No"] <- 0
  data$session <- substr(data$session,1,1)     ## clean up session
  data$voteId <- paste(data$session,"-",
                       data$rollcall_num,sep="")

  ## make sure roll calls in ascending order
  indx <- cbind(as.numeric(data$rollcall_num),
                as.numeric(data$session))
  indx <- indx[match(unique(data$voteId),data$voteId),]
  indx <- order(indx[,1],indx[,2])
  
  theRollCalls <- sort(as.numeric(unique(data$rollcall_num)))
  M <- length(theRollCalls)

  keepThese <- match(c("numericVote","nameid","voteId"),
                     names(data))

  ## BRUTE FORCE ROLL CALL MATRIX
  theLegis <- unique(data$nameid)
  theVotes <- unique(data$voteId)
  N <- length(theLegis)
  M <- length(theVotes)
  y <- matrix(NA,N,M)
  for(legis in theLegis){
    theData <- data[data$nameid==legis,]
    theseVotes <- match(theData$voteId,theVotes)
    y[match(legis,theLegis),theseVotes] <- theData$numericVote
  }
  rownames(y) <- theLegis
  colnames(y) <- theVotes
  
  ## vote descriptions
  con <- dbConnect(drv,group="ideal")
  query <- paste("SELECT * from ",metaTab,sep="")
  res <- dbSendQuery(con,statement=query)
  voteData <- fetch(res,n=-1)
  dbClearResult(res)
  dbDisconnect(con)
  voteData$session <- substr(voteData$session,1,1)     ## clean up session
  voteLabs <- paste(voteData$session,"-",voteData$rollcall_num,sep="")
  rownames(voteData) <- voteLabs
  voteData <- voteData[match(colnames(y),voteLabs),]
  
  ## member info
  con <- dbConnect(drv,group="ideal")
  query <- paste("SELECT * ",
                 "from memberInfo ",
                 "where congress = ",congress,
                 " and ",
                 "chamber = '",chamber,"'",
                 " or ",
                 "chamber = '",tolower(chamber),"'",
                 sep="")
  res <- dbSendQuery(con,statement=query)
  legisData <- fetch(res,n=-1)
  dbClearResult(res)
  dbDisconnect(con)

  legisData <- legisData[match(rownames(y),legisData$nameid),]   ## this line fails if no match! 6/18/13
  legisData$sort_field <- NULL
  legisData$unaccented_name <- NULL

  ## put lastname and firstname in 1st two columns
  if(chamber=="House"){
    legisData$class <- NULL
    legisData$next_election <- NULL
    indx <- match(c("last_name","first_name","middle_name","state","district","party"),
                  names(legisData))
  }
  if(chamber=="Senate"){
    legisData$district <- NULL
    indx <- match(c("last_name","first_name","middle_name","state","party"),
                  names(legisData))
  }
    
  indx <- c(indx,(1:length(names(legisData)))[-indx])
  legisData <- legisData[,indx]

  ## cleaner names on rollcall matrix
  if(chamber=="House"){
    goodNames <- paste(legisData$last_name," (",
                       legisData$party," ",
                       legisData$state,"-",legisData$district,")",
                       sep="")
  }
  if(chamber=="Senate"){
    goodNames <- paste(legisData$last_name," (",
                       legisData$party,"-",
                       legisData$state,")",
                       sep="")
  }
  rownames(y) <- goodNames
  rownames(legisData) <- goodNames
  names(legisData) <- gsub(names(legisData),pattern="_",replacement=".")
  names(voteData) <- gsub(names(voteData),pattern="_",replacement=".")
  
  source <- ifelse(chamber=="House",
                   "Clerk of the House (clerk.house.gov)",
                   "Senate Legislative Information System (senate.gov)")
  
  require(pscl)
  rc <- rollcall(data=y,
                 desc=paste(maketh(congress),chamber),
                 legis.names=dimnames(y)[[1]],
                 vote.names=dimnames(y)[[2]],
                 vote.data=voteData,
                 legis.data=legisData,
                 source=source)
  return(rc)
}




