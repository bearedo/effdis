   

yr.month.coverage.task2.r<-function(tdata=t2ce,start.year=1950,end.year=2010,which.gear='LL',which.region ='AT', which.flag='EU.Portugal'){
  
  #3D plot to explore temporal confounding
  #tdata <- out; which.flag   <- 'Japan'; which.gear <- 'LL'
  #start.year <- 1960; end.year <- 2005
  tdata1 <- tdata[tdata$month < 13,]
  fdata <- tdata1[tdata1$flagname == which.flag & tdata1$geargrpcode == which.gear & tdata1$region == which.region,]
  
  dd <- dim(fdata)
  #print(dd)
  if(dd[1] == 0)
  {
   #print('There are no data')
    }
  
  else{
  
  #par(mfrow=c(1,1),mar=c(3,3,3,3),oma=c(4,4,4,4))
  
    fmat <- matrix(NA, length(1950:2015),12)
    dimnames(fmat) <- list(c(1950:2015),1:12)
    
    ymc <- table(fdata$year,fdata$month) # Number of observations by year and month
  dimnames(ymc)[[1]] <- sort(unique(fdata$year))
  dimnames(ymc)[[2]] <- month.abb
  mm <- match(dimnames(ymc)[[1]],dimnames(fmat)[[1]])
  fmat[mm,] <- ymc
  
  image(1950:2015,1:12,fmat,xaxt='n',yaxt='n',xlab="",ylab="",col=terrain.colors(100),
        xlim=c(start.year,end.year),ylim=range(fdata$month,na.rm=T))
  contour(1950:2015,1:12,fmat,add=T)
  
  axis(side=1,at=start.year:end.year,label=as.character(start.year:end.year))
  ms <- range(fdata$month,na.rm=T)
  axis(side=2,at=ms[1]:ms[2],label=month.abb[ms[1]:ms[2]])
  title(paste(which.flag,which.gear, sep= ' - '))
  }
}

