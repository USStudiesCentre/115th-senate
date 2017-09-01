## clean up h112 table
library(RMySQL)
if(haveTable("h112")){  
  drv <- dbDriver("MySQL")
  con <- dbConnect(drv,group="ideal")
  query <- paste("SELECT CONCAT(session, rollcall_num) as z, count(nameid) from h112 group by z")
  res <- dbSendQuery(con,statement=query)
  data <- fetch(res,n=-1)
  names(data) <- c("id","n")
  dbClearResult(res)

  ## too many records
  bad <- data$n>436
  n <- sum(bad)
  if (n>0){
    badRollCalls <- data$z[bad]
    cat(paste("found",sum(bad),"bad records (too many records)\n"))
    cat("will delete these rollcalls\n")

    for(i in 1:n){
      query <- paste("DELETE from h112 where concat(session,rollcall_num) =",
                     badRollCalls[i])
      print(query)
      res <- dbSendQuery(con,statement=query)
      print(dbGetRowsAffected(res))
      dbClearResult(res)
    
      query <- paste("DELETE from h112meta where rollcall_num =",
                     badRollCalls[i],
                     "and session = '",
                     theSession,
                     "'")
      print(query)
      res <- dbSendQuery(con,statement=query)
      print(dbGetRowsAffected(res))
      dbClearResult(res)
    }
  }
  dbDisconnect(con)
}


## align h112 and h112meta
if(haveTable("h112meta") & haveTable("h112")){
  library(RMySQL)
  drv <- dbDriver("MySQL")
  con <- dbConnect(drv,group="ideal")

  query <- paste("SELECT DISTINCT session, rollcall_num from h112")
  res <- dbSendQuery(con,statement=query)
  rollcalls <- fetch(res,n=-1)
  rollcalls <- as.data.frame(rollcalls)
  rollcalls <- as.vector(apply(as.matrix(rollcalls),1,paste,collapse=":"))
  dbClearResult(res)

  query <- paste("SELECT DISTINCT session, rollcall_num from h112meta")
  res <- dbSendQuery(con,statement=query)
  rollcallsMeta <- fetch(res,n=-1)
  rollcallsMeta <- as.data.frame(rollcallsMeta)
  rollcallsMeta <- as.vector(apply(as.matrix(rollcallsMeta),1,paste,collapse=":"))
  dbClearResult(res)

  inboth <- intersect(rollcalls,rollcallsMeta)

  badRollCalls <- unique(c(rollcalls[!(rollcalls %in% inboth)],
                           rollcallsMeta[!(rollcallsMeta %in% inboth)]))

  n <- length(badRollCalls)
  if(n>0){
    for(i in 1:n){
      query <- paste("DELETE from h112 where rollcall_num =",
                     badRollCalls[i])
      print(query)
      res <- dbSendQuery(con,statement=query)
      print(dbGetRowsAffected(res))
      dbClearResult(res)

      query <- paste("DELETE from h112meta where rollcall_num =",
                     badRollCalls[i])
      print(query)
      res <- dbSendQuery(con,statement=query)
      print(dbGetRowsAffected(res))
      dbClearResult(res)
    }
  }
  dbDisconnect(con)
}


