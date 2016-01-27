spatial.coverage.by.year.task2 <-
function(tdata=t2ce,start.year=1950, end.year=2010, which.region = 'AT', which.gear='LL',which.flag='EU.Portugal'){
  #tdata <- out
  #which.gear <- 'LL'
  #which.flag   <- 'Brasil'
  #which.region <- 'AT';start.year <- 1990; end.year <- 1995;
  fdata <- tdata[tdata$flagname == which.flag & tdata$geargrpcode == which.gear & tdata$region == which.region,]
  cl <- 1.5
  ys <- start.year:end.year
  ly <- length(ys) # 43 years
  #par(mfrow=c(9,7),mar=c(0,0,1,0))
  for (i in min(ys,na.rm=T):max(ys,na.rm=T))
  {
    dat <- fdata[fdata$year == i,]
    #print(dim(dat))
    if(length(dat[,1])==0){
      plot(fdata$longitude,fdata$latitude,type='n',xaxt='n',yaxt='n',ylim=range(tdata$latitude,na.rm=T),
           xlim=range(tdata$longitude,na.rm=T),xlab="",ylab="")
      data("eez");plot(eez,add=T,col="thistle")
      map('world',col='green',fill=T,add=T)
      title(i,cex.main=cl)
      # map.scale(-18,-49,relwidth = 0.15, metric = TRUE, ratio = TRUE)
      #map("world",c("Ghana","Senegal","Côte D'Ivoire","South Africa","Brazil","Venezuela","Spain","France","Portugal","USA"),
       #   add=T,col="darkgreen",fill=T)
      
      
      }
    else{
      plot(dat$longitude,dat$latitude,type='n',xaxt='n',yaxt='n',ylim=range(tdata$latitude,na.rm=T),
           xlim=range(tdata$longitude,na.rm=T),xlab="",ylab="")
      data("eez");plot(eez,add=T,col="thistle")
       points(dat$longitude,dat$latitude,pch='*',col='red')
      map('world',add=T,col='green',fill=T)
      title(i,cex.main=cl)
      # map.scale(-18,-49,relwidth = 0.15, metric = TRUE, ratio = TRUE)
     # map("world",c("Ghana","Senegal","Côte D'Ivoire","South Africa","Brazil","Venezuela","Spain","France","Portugal","USA"),
      #    add=T,col="darkgreen",fill=T)
      
    }
    mtext(side=3,outer=T,paste(which.flag,which.gear, sep=' - '))  

  }
  
  
}
