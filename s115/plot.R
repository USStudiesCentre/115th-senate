library(pscl)
load("s115.rda")
load("id1.rda")
load("xbar.rda")

plotMin <- NULL
plotMin <- s115$dropInfo$dropList$legisMin

legisNames <- rownames(s115$votes)

## drop legislators with lots of missed votes from the plotting
bad <- apply(s115$votes,1,function(x)sum(!is.na(x))<plotMin)
badNames <- legisNames[bad]

plotData <- merge(x=data.frame(x=xbar[,1],
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
                             
plotData$col <- rep("green",dim(plotData)[1])
plotData$col[plotData$party=="R"] <- "red"
plotData$col[plotData$party=="D"] <- "blue"

indx <- order(plotData$x)          ## sort ideal point estimates
plotData <- plotData[indx,]

######################################################################
## one column
source("~/fonts.R")
quartz(file="x1.pdf",                ## open PDF file
       type="pdf",
       width=11.7,
       height=16.5,
       family="roboto")
par(mar=c(2,0.1,2,0.1),
    oma=c(1,0,6,.5),
    mgp=c(2,.25,0),
    family="roboto",
    lend=3)

plotlims <- range(plotData$lo,plotData$up)
axislims <- c(-3,3)
axisticks <- seq(from=axislims[1],to=axislims[2],by=1)
axislabs <- as.character(axisticks)
textloc <- 1.01*plotData$lo
textloc[textloc > -3] <- 1.01 * -3

n <- dim(plotData)[1]

plot(x=plotlims,
     y=c(1,n),       ## empty plotting region
     type="n",
     cex=.5,  
     ylim=c(.5,n+.5),yaxs="i",
     axes=FALSE,
     xlab="",ylab="")

abline(v=-2:2,lwd=3,col=gray(.90))

for(i in 1:n){     ## loop over senators
  thisAdj <- 1.1*(plotData$x[i]>0) + -.1*(plotData$x[i]<0) 
  lines(y=c(i,i),  ## line for confidence interval
        x=c(plotData$lo[i],plotData$up[i]),
        lwd=2.5,
        lend=3,
        col=gray(.50))
  points(y=i,
         x=plotData$x[i],
         col=plotData$col[i],
         pch=16,
         cex=1.65)

  thiscex <- .850
  if(plotData$x[i]<0){
    text(plotData$up[i],i,
         plotData$legisNames[i],
         family="roboto",
         cex=thiscex,adj=thisAdj)
  } else {
    text(plotData$lo[i],i,
         plotData$legisNames[i],
         family="roboto",
         cex=thiscex,adj=thisAdj)
  }
}
##axis(1,at=axisticks,labels=axislabs,cex=.5,lwd=0,lwd.tick=0)
##axis(3,at=axisticks,labels=axislabs,cex=.5,lwd=0,lwd.tick=0)

text("115th U.S. Senate: ideal points & 95% credible intervals",
     x=grconvertX(18,from="device",to="user"),
     y=grconvertY(24,from="device",to="user"),
     cex=2,
     pos=4,
     family="roboto",
     xpd=NA)

text(paste("Analysis conducted at ",
           format(Sys.time(), "%H:%M %a %b %d %Y %Z"),
           "\n",
           id1$m," non-unanimous roll calls from senate.gov\n",
           ifelse(is.null(plotMin),
                  "",
                  paste("Analysis drops senators with fewer than ",
                        plotMin,
                        " recorded votes on non-unanimous rollcalls",
                        sep="")
                  ),
           sep=""),
     x=grconvertX(20,from="device",to="user"),
     y=grconvertY(72,from="device",to="user"),
     cex=.9,
     pos=4,
     ##family="roboto",
     xpd=NA)

legend(bty="n",
       x="bottomright",
       bg="white",
       legend=rev(c("Democratic","Independent","Republican")),
       pch=16,
       col=rev(c("blue","green","red")))

graphics.off()

################################################
## plot against Trump vote share in 2016
library(pscl)
load("~/Dropbox/Projects/Election_2016/data/DavidLeip_states.RData")

plotData <- data.frame(y=id1$xbar[,1],
                       nm=rownames(id1$xbar),
                       party=s115$legis.data$party,
                       state.abb=str_extract(s115$legis.data$name,pattern="[A-Z]{2}"))
rownames(plotData) <- rownames(id1$xbar)

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

watermark <- function(){
  xlims <- grid::convertX(grid::unit(0:1, "npc"), "native", valueOnly=TRUE)
  ylims <- grid::convertY(grid::unit(0:1, "npc"), "native", valueOnly=TRUE)
  panel.text(x=xlims[2],
             y=.04*ylims[1] + .96*ylims[2],
             pos=2,
             cex=.45,
             paste("IDEAL POINT ESTIMATES AS OF ",
                   format(Sys.time(), "%H:%M %a %b %d %Y %Z"),"\n",
                   "CURVES ARE LOCAL LINEAR REGRESSIONS\n",
                   "(LOESS, SPAN=2/3, OUTLIER ROBUST)\n",
                   "ROLL CALL DATA FROM SENATE.GOV",
                   sep="")
             )
  invisible(NULL)
}


idResid <- function(group.number,subscripts,xjittered){
  lo <- loess(y~x,
              span=2/3,
              family="symmetric",
              degree=1,
              data=plotData[subscripts,],
              y=TRUE,x=TRUE)
  r <- resid(lo)
  rabs <- abs(r)
  print(sort(r,decreasing=TRUE,na.last=TRUE)[1:5])
  theOnes.indx <- order(rabs,decreasing=TRUE,na.last=TRUE)[1:5]
  
  ##extra <- match(c("Brown (R-MA)","Kirk (R-IL)"),
  ##               names(r))
  ##print(extra)
  ##if(all(!is.na(extra)))
  ##   theOnes.indx <- c(theOnes.indx,extra)

  ##if(group.number==1){
  ##  extra <- grep("BOXER",names(r))
  ##  theOnes.indx <- c(theOnes.indx,extra)
  ##}
  
  theOnes <- names(rabs)[theOnes.indx]
  print(theOnes)
  n <- length(theOnes)
  pos <- rep(2,n)
  if(group.number==1){
    pos[grepl("Rockefeller",theOnes)] <- 4
    pos[grepl("Carper",theOnes)] <- 4 
  }
  else{
    pos <- rep(4,n)
  }
  for(i in 1:n){
    panel.text(xjittered[theOnes.indx[i]],
               plotData$y[subscripts][theOnes.indx[i]],
               pos=pos[i],
               cex=.6,
               theOnes[i],
               xpd=NA)
#     panel.points(xjittered[theOnes.indx[i]],
#                  plotData$y[subscripts][theOnes.indx[i]],
#                  pch=16,
#                  col="black",
#                  cex=.6)
  }
}

library(lattice)
pdf("TrumpVote.pdf")
trellis.par.set(grid.pars=list(lineend="butt"),
                par.main.text=list(just="left",
                                   x = grid::unit(3, "mm"),
                                   font=1,
                                   cex=1.15),
                plot.line=list(col=gray(.50),lwd=7),
                add.line=list(lwd=1,
                              col=gray(.80)),
                axis.line=list(lwd=0,
                               col="#0000004c"))
                
foo <- xyplot(y ~ x,
              data=plotData,
              xlab="Trump 2016 Vote in State (%)",
              ylab="Estimated Ideal Points",
              main="115th U.S. Senate: estimated ideal points by Trump 2016 vote share",
              group=party,
              panel=panel.superpose,
              panel.group=function(x,y,group.number,subscripts,...){
                xjittered <- x
                if(group.number==1){  ## Dems
                  panel.grid(v=0,
                             h=-1,col=gray(.92))
                  panel.abline(v=seq(20,90,by=10),col=gray(.92))
                  panel.loess(x,y)
                  panel.xyplot(xjittered,y,fill=rgb(0,0,1,alpha=.75),pch=22,col=gray(.35))
                  idResid(group.number,subscripts,xjittered)
                  watermark()
                }
                if(group.number==2){   ## Indeps
                  panel.xyplot(xjittered,y,fill=rgb(0,1,0,alpha=.75),pch=22,col=gray(.35))
                }
                if(group.number==3){   ## Repubs
                  panel.loess(x,y)
                  panel.xyplot(xjittered,y,fill=rgb(1,0,0,alpha=.75),pch=22,col=gray(.35))
                  idResid(group.number,subscripts,xjittered)
                }
              },
              scales=list(alternating=3,
                          y=list(draw=FALSE),
                          x=list(at=seq(20,90,by=10))),
              key=list(columns=1,
                       x=.01,
                       y=.99,
                       corner=c(0,1),
                       points=list(pch=c(22,22,22),cex=.7,alpha=.7),
                       text=list(rev(c("Democrats",
                                       c("King & Sanders"),
                                       "Republicans")),
                                 col="black",cex=.7),
                       type="p",
                       fill=rev(c("blue","green","red"))
              )
)
print(foo)
dev.off()
