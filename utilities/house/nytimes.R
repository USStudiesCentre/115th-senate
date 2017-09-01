## loop over the unique entries in member data base
## get NYTimes bio info

## loop over ids in h112Member
## member info
source("processMemberData.R")
legisData <- getMemberData()

counter <- 1
library(RMySQL)
for(id in legisData$nameid){
  out <- extractNYTimesMemberData(id,congress=112,chamber="House")
  if(!is.null(out)){
    con <- dbConnect(drv,group="ideal")
    if(haveTable("nyTimesMemberInfo")){
      res <- dbWriteTable(conn=con,
                          name="nyTimesMemberInfo",
                          value=out,
                          row.names=FALSE,
                          append=TRUE)
    } else {
      res <- dbWriteTable(conn=con,
                          name="nyTimesMemberInfo",
                          value=tmp[[i]],
                          row.names=FALSE,
                          overwrite=TRUE)
    }
    dbDisconnect(con)
  }
}



