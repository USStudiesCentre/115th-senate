options(stringsAsFactors = FALSE)

library(RMySQL)
drv <- dbDriver("MySQL")
con <- dbConnect(drv,group="ideal")
query <- "select * from nyTimesMemberInfo limit 0,1"
res <- dbGetQuery(con,query)
existingNames <- names(res)
dbDisconnect(con)

######################################################
key <- "0184552500dcb2c10fd56b228aaf7a33:9:62106066"
baseURL <- paste("http://api.nytimes.com/svc/politics/v3/us/legislative/congress/",
                 congress,"/",
                 tolower(chamber),"/",
                 "members.xml?&api-key=KEY",
                 sep="")
theURL <- gsub(baseURL,pattern="KEY",replacement=key)

library(XML)
foo <- try(xmlParse(file=theURL,
                    isURL=TRUE),
           silent=TRUE)

uris <- unlist(xpathApply(foo,"//api_uri",xmlValue))

out <- list()
counter <- 1
for(uri in uris){
  cat(paste("trying uri",uri,"\n"))
  theURL <- paste(uri,"?api-key=",key,sep="")
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
      cat("success\n")
      flag <- FALSE
    }
  }

  ## parse output
  xpathApply(foo,"//committees",removeNodes)
  roleData <- getNodeSet(foo,"//role[1]")
  xpathApply(foo,"//roles",removeNodes)
  bioData <- getNodeSet(foo,"//member")
  print(bioData)
  out[[counter]] <- data.frame(c(xmlToDataFrame(bioData),
                                 xmlToDataFrame(roleData)))
  counter <- counter + 1
}

## stack
library(gtools)
tmp <- do.call("smartbind",out)
tmp$nameid <- tmp$id

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
