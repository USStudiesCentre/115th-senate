## any maintenance on names
library(RMySQL)
drv <- dbDriver("MySQL")
con <- dbConnect(drv,group="ideal")

theTab <-  theTable()
theMetaTab <- paste(theTab,"meta")

query <- paste("SELECT count(*) as num from",
               theTable(),
               "where nameid is null")
res <- dbSendQuery(con,statement=query)
tmpData <- fetch(res,n=-1)
dbClearResult(res)
dbDisconnect(con)

## self-lookup of missing nameid via temporary table mdata
if(tmpData$num[1]!=0){
  cat(paste("adding nameids to",tmpData$num[1],"new voting records\n"))
  library(RMySQL)
  drv <- dbDriver("MySQL")
  con <- dbConnect(drv,group="ideal")
  if("mdata" %in% dbListTables(con)){
    dbRemoveTable(con,"mdata")
  }
  query <- paste("create table mdata select distinct member_full, nameid from",
                 theTab,
                 "where nameid is not null")
  res <- dbSendQuery(con,statement=query)
  query <- paste("update",
                 theTab,
                 "left join mdata using (member_full)",
                 "set",
                 paste(theTab,"nameid",sep="."),
                 " = mdata.nameid",
                 "where",
                 paste(theTab,"nameid",sep="."),
                 "is NULL")
  res <- dbSendQuery(con,statement=query)
  cat("self-lookup\n")
  cat("how many records are we updating?:\n")
  print(dbGetRowsAffected(res))
  rm(res)
  dbRemoveTable(con,"mdata")
  dbDisconnect(con)
}

## any more missing nameid - these will usually be new Senators
## trigger an update of senate.gov lookups
library(RMySQL)
drv <- dbDriver("MySQL")
con <- dbConnect(drv,group="ideal")
query <- paste("SELECT count(*) as num from",
               theTab,
               "where nameid is null")
res <- dbSendQuery(con,statement=query)
tmpData <- fetch(res,n=-1)
dbClearResult(res)
dbDisconnect(con)

## check for and/or add Senate bio info from senate.gov
if(tmpData$num[1]!=0){
  source("updateBioInfo.R")
}
