###########################
## clean up tables if a mess, unaligned
##source("../utilities/house/clean.R")

session <- getSession()
theSession <- c("1st","2nd")[session]

####################################################
## when 2nd session starts
## hard code M to known last rollcall of 1st session
## last roll call the Clerk has
M <- lastRollCall(year)
theSeq <- paste("1st",1:M,sep="")
####################################################

if(session==2){
  theSeq <- c(theSeq,
              paste("2nd",1:M,sep=""))
}
            
###################################################
## check that we have all of these roll calls
if(haveTable(theTable())){
  library(RMySQL)
  drv <- dbDriver("MySQL")
  con <- dbConnect(drv,group="ideal")
  query <- paste("SELECT DISTINCT CONCAT(session, rollcall_num) from",theTable())
  res <- dbSendQuery(con,statement=query)
  rollcalls <- as.vector(fetch(res,n=-1)[[1]])
  dbClearResult(res)
  dbDisconnect(con)
  if(length(rollcalls)>0){
    theSeq <- setdiff(theSeq,rollcalls)
  }     
}  
##################################################

#################################################
## download rollcalls

cat("we will attempt to download the following rollcalls\n")
print(theSeq)

baseURL <- paste("http://clerk.house.gov/evs/","YEAR","/roll",sep="")

for(i in theSeq){
  number <-  as.numeric(gsub(i,pattern="1st|2nd",replacement=""))
  session <- substring(i,1,3)
  cat(paste("checking",
            session,
            "session, rollcall number",
            number,"\n",sep=" "))
  zeros <- paste(rep("0",2-floor(log10(number))),collapse="")
  theURL <- paste(baseURL,zeros,number,".xml",sep="")
  if(session=="1st")
    theURL <- gsub(theURL,pattern="YEAR",replacement="2015")
  if(session=="2nd")
    theURL <- gsub(theURL,pattern="YEAR",replacement="2016")
  
  ## check again, do we already have the rollcall in the database?
  res <- 0
  res <- checkForRollCall(session,number)
  
  if(!res){
    ## test the URL
    cat(paste("trying",theURL,"\n"))
    urltest <- try(url(theURL,open="r"),silent=TRUE)
    if(inherits(urltest,"try-error")){
      break
    } else {
      close(urltest)
      parseFunction(theURL)
    }
  }
  cat("\n\n")
}
