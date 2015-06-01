spatial.coverage.by.year.task2.f <- function(tdata=t2ceLL,which.region = 'AT',which.flag='EU.Portugal'){
  #tdata <- t2ceLL
  #which.region <- 'AT'
  #which.flag   <- 'EU.Portugal'
  fdata <- tdata[tdata$FlagName == which.flag & tdata$Region == which.region,]
  
  par(mfrow=c(6,10),mar=c(0,0,1,0),oma=c(1,1,3,1)) # 60 years
  ys <- sort(unique(tdata$YearC))
  ly <- length(ys) # 43 years
  
  for (i in min(ys):max(ys))
  {
    dat <- fdata[fdata$YearC == i,]
    if(length(dat[,1])==0){
      plot(dat$lon,dat$lat,type='n',xaxt='n',yaxt='n',ylim=range(tdata$lat),xlim=range(tdata$lon))
      map('world',col='green',fill=T,add=T)
      title(i,cex.main=.75)}
    else{
      plot(dat$lon,dat$lat,type='n',xaxt='n',yaxt='n',ylim=range(tdata$lat),xlim=range(tdata$lon))
      points(dat$lon,dat$lat,pch=16,col='red')
      map('world',add=T,col='green',fill=T)
      title(i,cex.main=.75)
    }
    mtext(side=3,outer=T,which.flag)
  }
}
