## pad some identifying info to what gets dumped
outData <- s114$legis.data[,c("last.name", "first.name",
                              "state",
                              "party",
                              "gender",
                              "nameid",
                              "icpsr.id","govtrack.id","cspan.id","thomas.id",
                              "class","next.election")]
outData$name <- rownames(s114$legis.data)
m <- summary(s114,verbose=TRUE)$legisTab[,"missing"]
outData <- merge(x=outData,
                 y=data.frame(missingVotes=m,name=names(m)))
outData <- merge(x=outData,
                 y=data.frame(idealPoint=s$xm[,"D1"],
                   lo=s$xHDR[,"lower","D1"],
                   up=s$xHDR[,"upper","D1"],
                   name=rownames(s$xm)))
outData$label <- outData$name
outData$name <- NULL
outData$indx <- rank(outData$idealPoint)
outData$firstnm <- outData$first.name
outData$lastnm <- outData$last.name
outData$first.name <- NULL
outData$last.name <- NULL


write.csv(outData[order(outData$indx),],
          file="data/estimates.csv",
          na=" ",
          quote=1:(dim(outData)[2]),  
          row.names=FALSE,
          fileEncoding="UTF-8")
