

spatial.coverage.by.month.task2.r <- function(tdata=t2ceLL,which.region = 'AT',which.flag='EU.Portugal')
{
  
  fdata <- tdata[tdata$FlagName == which.flag & tdata$Region == which.region,]
par(mfrow=c(3,4),mar=c(0,0,2,0),oma=c(1,1,3,1))
ms <- sort(unique(fdata$TimePeriodID))
lm <- length(ms)
for (i in min(ms):max(ms))
{
  dat <- fdata[fdata$TimePeriodID == i,]
  plot(tdata$lon,tdata$lat,type='n',xaxt='n',yaxt='n')
  points(dat$lon,dat$lat,col='red',pch=16)
  map('world',add=T,col='green',fill=T)
  title(month.abb[i])
}
mtext(side=3,outer=T,which.flag)
}

