

yr.month.coverage.task2.r<-function(tdata=t2ce,which.gear='LL',which.flag='EU.Portugal'){
  
  #3D plot to explore temporal confounding
  #tdata <- t2ce
  #which.flag   <- 'EU.Portugal'
  fdata <- tdata[tdata$flagname == which.flag & tdata$geargrpcode == which.gear & fdata$timeperiodid < 13,]
  fdata$trend <- trend.r(year=fdata$yearc,month=fdata$timeperiodid,start.year=1950)
  
  
  par(mfrow=c(1,1))
  ymc <- table(fdata$yearc,fdata$timeperiodid) # Number of observations by year and month
  dimnames(ymc)[[1]] <- sort(unique(fdata$yearc))
  dimnames(ymc)[[2]] <- month.abb
  
  image(as.numeric(sort(unique(row.names(ymc)))),1:12,ymc,xaxt='n',yaxt='n',xlab="",ylab="",col=terrain.colors(100),
        xlim=range(tdata$yearc,na.rm=T),ylim=range(fdata$timeperiodid,na.rm=T))
  contour(as.numeric(sort(unique(row.names(ymc)))),1:12,ymc,add=T)
  
  axis(side=1,at=min(tdata$yearc,na.rm=T):max(tdata$yearc,na.rm=T),label=sort(unique(tdata$yearc)))
  ms <- range(fdata$timeperiodid,na.rm=T)
  axis(side=2,at=ms[1]:ms[2],label=month.abb[ms[1]:ms[2]])
  title(paste(which.flag,which.gear))
  
}

