
effort.by.year.task2.f <- function(tdata = t2ceLL, which.variable = 'Eff1', which.region = 'AT', which.flag = 'Japan'){

  #tdata <- t2ceLL
  #which.region <- 'AT'
  #which.flag   <- 'EU.Portugal'
  fdata <- tdata[tdata$Region == which.region & tdata$FlagName == which.flag,]
  #par(mfrow=c(1,1),mar=c(2,4,2,1))
  plot(fdata$trend,log(fdata[,which.variable]),pch='.',xaxt='n',ylab='Number of hooks',xlab="year",xlim=range(tdata$trend))
lines(supsmu(fdata$trend,log(fdata[,which.variable])),col='green')
xl <- seq(min(tdata$YearC),max(tdata$Year),by=5)
axis(1,at=seq(min(tdata$trend),max(tdata$trend),by=60),labels=as.character(xl))
abline(v=seq(min(tdata$trend),max(tdata$trend),by=60),lty=2,col='cyan')
title(which.flag)

}
