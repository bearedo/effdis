   

yr.month.coverage.task2.r<-function(tdata=t2ce,start.year=1950,end.year=2015,which.gear='LL',which.flag='EU.Portugal'){
  
  #3D plot to explore temporal confounding
  #tdata <- t2ce; which.flag   <- 'U.S.A.'; which.gear <- 'LL'
  tdata1 <- tdata[tdata$timeperiodid < 13,]
  fdata <- tdata1[tdata1$flagname == which.flag & tdata1$geargrpcode == which.gear,]
  
  dd <- dim(fdata)
  print(dd)
  if(dd[1] == 0)
  {
    print('There are no data')
    }
  
  else{
  
  par(mfrow=c(1,1))
  
    fmat <- matrix(NA, length(start.year:end.year),12)
    dimnames(fmat) <- list(c(start.year:end.year),1:12)
    
    ymc <- table(fdata$yearc,fdata$timeperiodid) # Number of observations by year and month
  dimnames(ymc)[[1]] <- sort(unique(fdata$yearc))
  dimnames(ymc)[[2]] <- month.abb
  mm <- match(dimnames(ymc)[[1]],dimnames(fmat)[[1]])
  fmat[mm,] <- ymc
  
  image(start.year:end.year,1:12,fmat,xaxt='n',yaxt='n',xlab="",ylab="",col=terrain.colors(100),
        xlim=c(start.year,end.year),ylim=range(fdata$timeperiodid,na.rm=T))
  contour(start.year:end.year,1:12,fmat,add=T)
  
  axis(side=1,at=start.year:end.year,label=as.character(start.year:end.year))
  ms <- range(fdata$timeperiodid,na.rm=T)
  axis(side=2,at=ms[1]:ms[2],label=month.abb[ms[1]:ms[2]])
  title(paste(which.flag,which.gear, sep= ' - '))
  }
}

