

spatial.coverage.by.month.task2 <- function(tdata=t2ce,which.gear = 'LL', which.region = 'AT',which.year=2005,which.flag='EU.Portugal')
{
  
  fdata <- tdata[tdata$flagname == which.flag & tdata$geargrpcode == which.gear & tdata$region == which.region & tdata$timeperiodid < 13 & tdata$yearc == which.year,]
par(mfrow=c(3,4),mar=c(1,1,2,1),oma=c(1,1,3,1))
ms <- sort(unique(fdata$timeperiodid))
lm <- length(ms)
for (i in min(ms):max(ms))
{
  dat <- fdata[fdata$timeperiodid == i,]
  plot(fdata$longitude,fdata$latitude,type='n',xaxt='n',yaxt='n')
  points(dat$longitude,dat$latitude,col='red',pch=16)
  map('world',add=T,col='green',fill=T)
  title(month.abb[i])
}
mtext(side=3,outer=T,which.flag)
}

