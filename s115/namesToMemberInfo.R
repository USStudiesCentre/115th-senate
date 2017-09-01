#############################################
## one time job
## push unique identifying info to SQL tables
#############################################

congress <- 114
chamber <- "Senate"

source("../utilities/simpleNYTimesParse.R")

## append to NYTimes table (once only)
library(RMySQL)
drv <- dbDriver("MySQL")
con <- dbConnect(drv,group="ideal")
dbWriteTable(conn=con,
             name="nyTimesMemberInfo",
             value=tmp,
             row.names=FALSE,
             append=TRUE)
dbDisconnect(con)

#########################################
## append to memberInfo table (once only)
options(stringsAsFactors = FALSE)

library(RMySQL)
drv <- dbDriver("MySQL")
con <- dbConnect(drv,group="ideal")
query <- "select * from memberinfo limit 0,1"
res <- dbGetQuery(con,query)
existingNames <- names(res)
dbDisconnect(con)

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

library(RMySQL)
drv <- dbDriver("MySQL")
con <- dbConnect(drv,group="ideal")
dbWriteTable(conn=con,
             name="memberinfo",
             value=tmp,
             row.names=FALSE,
             append=TRUE)
dbDisconnect(con)

