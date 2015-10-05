
effort.by.year.task2 <- function(tdata = t2ce, which.gear = 'LL', which.flag = 'Japan'){

  #tdata <- t2ce
  #which.region <- 'AT'
  #which.flag   <- 'EU.Portugal'
  fdata <- tdata[tdata$geargrpcode == which.gear & tdata$flagname == which.flag & tdata$timeperiodid < 13,]
  fdata$trend <- trend.r(year=fdata$yearc,month=fdata$timeperiodid,start.year=1950)
  #par(mfrow=c(1,1),mar=c(2,4,2,1))
  plot(fdata$trend,log(fdata$eff1),pch='.',xaxt='n',ylab='Number of hooks',xlab="year",xlim=range(tdata$trend))
lines(supsmu(fdata$trend,log(fdata$eff1)),col='darkblue',lwd=5)
xl <- seq(min(tdata$yearc),max(tdata$yearc),by=5)
axis(1,at=seq(min(tdata$trend),max(tdata$trend),by=60),labels=as.character(xl))
abline(v=seq(min(tdata$trend),max(tdata$trend),by=60),lty=2,col='green')
title(paste(which.flag, which.gear))

}

