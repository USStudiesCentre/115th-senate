key <- "0184552500dcb2c10fd56b228aaf7a33:9:62106066"
baseURL <- paste("http://api.nytimes.com/svc/politics/v3/us/legislative/congress/",
                 congress,"/",
                 tolower(chamber),"/",
                 "members.xml?&api-key=KEY",
                 sep="")

if(exists("nyTimes")){
  rm(nyTimes)
}

source("../utilities/simpleNYTimesParse.R")
nyTimes <- tmp
rm(tmp)
nyTimes$congress <- as.character(congress)
nyTimes$chamber <- chamber

## write to SQL temporary table
library(RMySQL)
drv <- dbDriver("MySQL")
con <- dbConnect(drv,group="ideal")
dbWriteTable(con,"mdata",value=nyTimes,overwrite=TRUE)

## loop over common columns, replace memberinfo with latest from NYTimes
d <- intersect(dbListFields(con,"memberInfo"),
               dbListFields(con,"mdata"))
dontChange <- c("nameid","congress","chamber",
                "last_name","state")
d <- d[-match(dontChange,d)]
for(nm in d){
  query <- paste("UPDATE memberInfo ",
                 "LEFT JOIN mdata USING (nameid, congress, chamber) ",
                 "set ",
                 "memberInfo.",nm,
                 "=",
                 "mdata.",nm,
                 " ",
                 "where ",
                 "memberInfo.",nm,
                 "!=",
                 "mdata.",nm,
                 sep="")
  print(query)
  res <- dbSendQuery(con,statement=query)
  cat(paste("rows changed:",dbGetRowsAffected(res),"\n"))
}
dbDisconnect(con)


