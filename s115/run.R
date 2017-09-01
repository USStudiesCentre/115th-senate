## scrape vote information from NYTimes
chamber <- "Senate"
congress <- 115

source("../utilities/utilities.R")

source("readVotes.R")
source("../utilities/senate/checkNames.R")

######################################
## pull roll calls and meta data etc
######################################
library(pscl)

rc <- rollcall(data=z,
               legis.names=rownames(z),
               legis.data=legis.data)

dropMin <- 5
s115 <- dropRollCall(rc,
                     dropList=list(lop=0,
                                   legisMin=dropMin))
save("s115",file="s115.rda")                       
print(summary(s115))

## load number of rollcalls
if(file.exists("m.rda")){
  load("m.rda")
}

## if same number of rollcalls as before, then quit
if(exists("mOld")){
  if(mOld == s115$m){
    quit("no")
  }
}

## start values
x0 <- as.numeric(s115$legis.data$party=="R") - as.numeric(s115$legis.data$party=="D")
x0[is.na(x0)] <- 0

M <- 1E6
## run ideal
id1 <- ideal(s115,
             d=1,
             maxiter=M,
             burnin=1e4,
             thin=M/5E5,
             verbose=TRUE,
             startvals=list(x=x0),
             normalize=TRUE,
             priors=list(bpv=.04))
save("id1",file="id1.rda")

## dump current number of rollcalls
mOld <- s115$m
save("mOld",file="m.rda")

s <- summary(id1)
xbar <- cbind(s$xm,s$xHDR[,,"D1"])
save("xbar",file="xbar.rda")

##########################################
## pad some identifying info to what gets dumped
##source("outData.R")

###########################################
## archiving
dateString <- format(Sys.time(),"%Y_%m_%d")
save("rc",
     #"outData",
     file=paste("data/",dateString,".RData",sep=""))

##########################
## plotting
##source("plot.R")
##source("d3Write.R")

