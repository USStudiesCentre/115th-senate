#########################################
## this file updates roll call data
## with new member_id from senate.gov
##
## code at end updates memberinfo table etc
#########################################

## read Senate list of current members
if(exists("members")){
  rm(members)
}

theURL <- "http://senate.gov/general/contact_information/senators_cfm.xml"

con <- try(url(theURL,open="r"),silent=TRUE)

if(inherits(con,"try-error")){
  cat("couldn't connect\n")
} else {
  close(con)
  tmpFile <- tempfile()
  download.file(url=theURL,destfile=tmpFile)

  library(XML)
  members <- xmlToDataFrame(tmpFile,
                            stringsAsFactors=FALSE)
  members <- data.frame(lapply(members,gsub,pattern="\n",replacement=""),
                        stringsAsFactors=FALSE)
  members <- members[!is.na(members$bioguide_id),]
  rownames(members) <- members$bioguide_id
  current.ids <- members$bioguide_id

  ## foo <- xmlTreeParse(file=tmpFile,
  ##                     handlers=list("member"=function(x){
  ##                       nms <- unlist(xmlSApply(x,xmlName))
  ##                       out <- xmlSApply(x,xmlValue)
  ##                       out <- lapply(out,
  ##                                     function(x){
  ##                                       if(length(x)==0){
  ##                                         return(NA)
  ##                                       } else {
  ##                                         return(x)
  ##                                       }
  ##                                     }
  ##                                     )
  ##                       if(length(out)==length(nms)){
  ##                         names(out) <- nms
  ##                       }
  ##                       out <- data.frame(out,stringsAsFactors=FALSE)
  ##                       if(exists("members")){
  ##                         d <- setdiff(names(members),
  ##                                      names(out))
  ##                         if(length(d)>0){
  ##                           print(d)
  ##                           for(n in d){
  ##                             out[[n]] <- NA
  ##                           }
  ##                         }
  ##                         d <- setdiff(names(out),
  ##                                      names(members))
  ##                         if(length(d)>0){
  ##                           print(d)
  ##                           for(n in d){
  ##                             members[[n]] <- NA
  ##                           }
  ##                         }
  ##                         members <<- rbind(members,out)
  ##                       } else {
  ##                         members <<- out
  ##                       }
  ##                     }
  ##                       ),
  ##                     useInternalNodes=FALSE)

  ## all missing
  allMissing <- unlist(apply(members,1,function(x){all(is.na(x))}))
  members <- members[!allMissing,]
  
  rownames(members) <- members$bioguide_id
  unlink(tmpFile)

  ## put nameid onto rollcall data table, use senate database as match
  library(RMySQL)
  drv <- dbDriver("MySQL")
  con <- dbConnect(drv,group="ideal")
  dbWriteTable(con,name="mdata",value=members,overwrite=TRUE,row.names=TRUE)

  ## do the JOIN
  if(!("nameid" %in% dbListFields(con,theTable()))){
    res <- dbSendQuery(con,paste(
                                 "ALTER TABLE",
                                 theTable(),
                                 "ADD nameid text"))
    print(dbGetRowsAffected(res))
    dbClearResult(res)
  }
  query <- paste("UPDATE",
                 theTable(),
                 "LEFT JOIN mdata USING (member_full)",
                 "SET",
                 paste(theTable(),".nameid=mdata.bioguide_id",sep=""),
                 "WHERE",
                 paste(theTable(),".nameid",sep=""),
                 "is NULL",
                 "OR",
                 paste(theTable(),".nameid",sep=""),
                 "= ''")
  res <- dbSendQuery(con,statement=query)
  cat("updateBioInfo: after looking at senate list, we're changing records:\n")
  print(dbGetRowsAffected(res))

  dbRemoveTable(con,"mdata")
  dbDisconnect(con)
  rm(res)
}

#####################################################
## any new records for memberinfo???
library(RMySQL)
drv <- dbDriver("MySQL")
con <- dbConnect(drv,group="ideal")
query <- paste("select nameid from memberInfo where congress = ",congress)
res <- dbSendQuery(con,statement=query)
existing.ids <- fetch(res,n=-1)
existing.ids <- existing.ids$nameid
dbClearResult(res)
dbDisconnect(con)

bad <- is.na(match(current.ids,existing.ids))

if(any(bad)){
  ## we have current Senators not in memberInfo database
  newData <- members[match(current.ids[bad],rownames(members)),]
  print(newData)
  
  ## write new member info to memberInfo
  members$nameid <- members$bioguide_id
  members$congress <- congress
  members$chamber <- chamber
  library(RMySQL)
  drv <- dbDriver("MySQL")
  con <- dbConnect(drv,group="ideal")
  res <- dbSendQuery(con,statement="select * from memberInfo limit 0,1")
  colNames <- fetch(res,n=-1)
  dbClearResult(res)
  keepCols <- names(members) %in% names(colNames)
  d <- plyr::rbind.fill(colNames,newData[,keepCols])
  d <- d[-1,]
  rownames(d) <- NULL
  dbWriteTable(con,"memberInfo",value=d,append=TRUE,row.names=FALSE)
  dbDisconnect(con)
}
