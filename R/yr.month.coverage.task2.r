yr.month.coverage.task2.r<-function(tdata=t2ceLL,which.region = 'AT',which.flag='EU.Portugal'){

#3D plot to explore temporal confounding
 #tdata <- t2ceLL
 #which.region <- 'AT'
 #which.flag   <- 'EU.Portugal'
 fdata <- tdata[tdata$Region == which.region & tdata$FlagName == which.flag,]

ymc <- table(fdata$YearC,fdata$TimePeriodID) # Number of observations by year and month
dimnames(ymc)[[1]] <- c(min(fdata$YearC):max(fdata$YearC)) 
dimnames(ymc)[[2]] <- month.abb

image(min(fdata$YearC):max(fdata$YearC),1:12,ymc,xaxt='n',yaxt='n',xlab="",ylab="",col=terrain.colors(100),
      xlim=range(tdata$YearC),ylim=range(tdata$TimePeriodID))
contour(min(fdata$YearC):max(fdata$YearC),1:12,ymc,add=T)

axis(side=1,at=min(tdata$YearC):max(tdata$YearC),label=sort(unique(tdata$YearC)))
ms <- range(fdata$TimePeriodID)
axis(side=2,at=ms[1]:ms[2],label=month.abb[ms[1]:ms[2]])
title(which.flag)

}

