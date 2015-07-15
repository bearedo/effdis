
spatial.coverage.by.year.task2.r <- function(tdata=t2ce,start.year=1950, end.year=2010, which.gear='LL',which.flag='EU.Portugal'){
 # tdata <- t2ce
 # which.gear <- 'LL'
 # which.flag   <- 'Chinese Taipei'
  
  fdata <- tdata[tdata$flagname == which.flag & tdata$geargrpcode == which.gear,]
  
  ys <- start.year:end.year
  ly <- length(ys) # 43 years
  par(mfrow=c(6,10),mar=c(0,0,1,0))
  for (i in min(ys,na.rm=T):max(ys,na.rm=T))
  {
    dat <- fdata[fdata$yearc == i,]
    if(length(dat[,1])==0){
      plot(fdata$longitude,fdata$latitude,type='n',xaxt='n',yaxt='n',ylim=range(tdata$latitude,na.rm=T),xlim=range(tdata$longitude,na.rm=T))
      map('world',col='green',fill=T,add=T)
      title(i,cex.main=.75)}
    else{
      plot(dat$longitude,dat$latitude,type='n',xaxt='n',yaxt='n',ylim=range(tdata$latitude,na.rm=T),xlim=range(tdata$longitude,na.rm=T))
      points(dat$longitude,dat$latitude,pch=16,col='red')
      map('world',add=T,col='green',fill=T)
      title(i,cex.main=.75)
    }
    mtext(side=3,outer=T,paste(which.flag,which.gear, sep=' - '))  

  }
}
