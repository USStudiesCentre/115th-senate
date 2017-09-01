library(pscl)
load("s115.rda")
load("id1.rda")
load("xbar.rda")

## time stamp
write.csv(data.frame(date=as.character(file.info("id1.rda")$mtime)),
          row.names=FALSE,
          quote=TRUE,
          file="data/timeStamp.csv")  

## number of non-unanimous votes
write.csv(data.frame(nvotes=id1$m),
          row.names=FALSE,
          file="data/nvotes.csv")

## minimum number of votes for plotting
plotMin <- 30
if(id1$m<plotMin){
  plotMin <- 1       ## for early in the Congress
}

legisNames <- rownames(s115$votes)

## drop legislators with lots of missed votes from the plotting
bad <- apply(s115$votes,1,function(x)sum(!is.na(x))<plotMin)
badNames <- legisNames[bad]

plotData <- merge(x=data.frame(y=xbar[,1],
                               lo=xbar[,2],
                               up=xbar[,3],
                               legisNames=rownames(xbar),
                               stringsAsFactors=FALSE),
                  y=data.frame(legisNames=rownames(s115$legis.data),
                               party=s115$legis.data$party,
                               stringsAsFactors=FALSE),
                  all=TRUE,
                  stringsAsFactors=FALSE)
plotData <- plotData[!(plotData$legisNames %in% badNames),]

################################################
## plot against Trump vote share in 2016
library(pscl)
library(stringr)
load("~/Dropbox/Projects/Election_2016/data/DavidLeip_states.RData")

plotData <- data.frame(y=id1$xbar[,1],
                       nm=rownames(id1$xbar),
                       party=s115$legis.data$party,
                       state.abb=str_extract(s115$legis.data$name,pattern="[A-Z]{2}"))
rownames(plotData) <- rownames(id1$xbar)

plotData$col <- rep("green",dim(plotData)[1])
plotData$col[plotData$party=="R"] <- "red"
plotData$col[plotData$party=="D"] <- "blue"

plotData <- merge(plotData,
                  data.frame(state=state.name,
                             state.abb=state.abb),
                  by="state.abb")

plotData <- merge(plotData,
                  tab,
                  by.x="state.abb",
                  by.y="st")
plotData$x <- plotData$trump.p
plotData$demVote <- NULL
rownames(plotData) <- plotData$nm


indx <- order(plotData$y)          ## sort ideal point estimates
plotData <- plotData[indx,]

## accents etc
if(is.null(plotData$legisNames)){
  plotData$legisNames <- rownames(plotData)
}
Encoding(plotData$legisNames) <- "latin1"

aicc.loess <- function(fit) {
  # compute AIC_C for a LOESS fit, from:
  # 
  # Hurvich, C.M., Simonoff, J.S., and Tsai, C. L. 1998. Smoothing 
  # parameter selection in nonparametric regression using an improved 
  # Akaike Information Criterion. Journal of the Royal Statistical 
  # Society B 60: 271–293.
  # 
  # @param fit        loess fit
  # @return           'aicc' value
  stopifnot(inherits(fit, 'loess'))
  # parameters
  n <- fit$n
  trace <- fit$trace.hat
  sigma2 <- sum(resid(fit) ^ 2) / (n - 1)
  return(log(sigma2) + 1 + 2 * (2 * (trace + 1)) / (n - trace - 2))
}

gcv.loess <- function(m){
  e <- resid(m)
  n <- length(e)
  denom <- 1 - m$enp/n
  gcv <- mean((e/denom)^2)
  return(gcv)
}


autoloess <- function(fit, span=c(.1, .9)) {
  # compute loess fit which has span minimizes AIC_C
  # 
  # @param fit        loess fit; span parameter value doesn't matter
  # @param span       a two-value vector representing the minimum and 
  #                   maximum span values
  # @return           loess fit with span minimizing the AIC_C function
  stopifnot(inherits(fit, 'loess'), length(span) == 2)
  # loss function in form to be used by optimize
  f <- function(span) gcv.loess(update(fit, span=span))
  # find best loess according to loss function
  return(update(fit, span=optimize(f, span)$minimum))
}


ll <- list()
ll[[1]] <- loess(y ~ x,
               data=plotData,
               degree=1,
               family="symmetric",
               subset=party=="R",
               span=.2)
ll[[2]] <- loess(y ~ x,
               data=plotData,
              degree=1,
               family="symmetric",
               subset=party=="D",
               span=.2)

ll <- lapply(ll,autoloess)

yhat <- lapply(ll,
               function(obj){
                 xseq <- seq(from=min(obj$x),to=max(obj$x),length=101)
                 yhat <- predict(obj,newdata=data.frame(x=xseq))
                 return(data.frame(x=xseq,yhat=yhat))
               }
)

write.csv(file="data/repLoess.csv",
          row.names=FALSE,
          quote=TRUE,
          yhat[[1]])

write.csv(file="data/demLoess.csv",
          row.names=FALSE,
          quote=TRUE,
          yhat[[2]])

write.csv(file="data/plotData.csv",
          row.names=FALSE,
          quote=TRUE,
          plotData)

