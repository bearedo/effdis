predict.effdis.t2.data <- function (cmod=mods, effmod=emod,grid.res=5,start.year=1995,end.year=2010,which.flag='All',which.gear='LL')
  {
  
  #cmod  <- alb.ps;effmod <- emod;grid.res <-1;start.year <- 1990; end.year <- 2010;which.flag ='EU.España';which.gear='PS'
  
  ## Use the GAM models fitted in the previous step to predict over a relevant-sized grid
  
  ## Build grid for predictions ## 
 
  ## NB. Use the data fitted to the Bernouilli  model.
  
  pmod.data <- cmod$pmod.data
  
  min.lat <- min(pmod.data$latitude)
  max.lat <- max(pmod.data$latitude)
  min.lon <- min(pmod.data$longitude)
  max.lon <- max(pmod.data$longitude)
  

  t1 <- min(pmod.data$trend)
  t2 <- max(pmod.data$trend)
  lonnie <- seq(min.lon,max.lon,by=grid.res)
  lattie <- seq(min.lat,max.lat,by=grid.res)
  lo <- length(lonnie)
  la <- length(lattie)
  
  grd <-data.frame(expand.grid(longitude=lonnie,latitude=lattie))
  grd <- find.ocean.r(input=grd[,c(1,2)])
  
  #plot(grd$longitude[grd$which.ocean=='atl'],grd$latitude[grd$which.ocean=='atl'],pch='.')
  
  #grd <- add.covariates.r(input=grd)$output
  #grd <- as.data.frame(grd)
  
  #grd$ldepth_m <- grd$depth_m*-1
  #grd$ldepth_m <- log(grd$ldepth_m)
  
  lyrs       <- length(start.year:end.year)
  lloc <- lo*la
  
  ltrnd     <- lyrs*12
  
  ngrd <- data.frame(longitude=rep(grd$longitude,ltrnd),
                     latitude=rep(grd$latitude,ltrnd),
                     which.ocean=rep(grd$which.ocean,ltrnd),
                     year = rep(start.year:end.year,rep((lo*la*12),lyrs)), 
                     month = rep(rep(1:12,rep(lo*la,12)),lyrs)
                     )
  
  ngrd$trend <- trend.r(ngrd$year,ngrd$month,start.year=1950)
  ngrd$flagname <- which.flag
  ngrd$geargrp <- which.gear
  
  #Set up the harmonics
  
  
  dat0 <- ngrd # input data is dat0
  
  ss = cc = matrix(NA,nr=length(dat0[,1]), nc=6)
  
  for (i in 1:6)
  { cc[,i] <- cos(2*pi*i*dat0$trend/12)                                                              
  ss[,i] <- sin(2*pi*i*dat0$trend/12) } # set up the regressors
  
  ss <- ss[,-6]
  dat1 <- cbind(dat0,ss,cc)
  dd <- dim(dat0)
  dimnames(dat1)[[2]][(dd[2]+1):(dim(dat1)[2])] <- c(paste('sin',1:5,sep=''),paste('cos',1:6,sep=''))
  

  ngrd <- dat1
  
  # Do the predictions over the grid #
  
  prob <- predict(cmod$pmod,ngrd,type="response")
  measured_catch <- predict(cmod$gmod,ngrd,type="response")
  eff <- predict(effmod$emod,ngrd,type="response")
  
  # Block out the land #
  
  prob[ngrd$which.ocean %in% c('land','med','pac')] <- NA
  measured_catch[ngrd$which.ocean %in% c('land','med','pac')] <- NA
  eff[ngrd$which.ocean %in% c('land','med','pac')] <- NA
  
   # Convert to vectors
  
  ngrd$prob <- round(as.vector(prob),3)
  ngrd$measured_catch <- round(as.vector(measured_catch),3)
  ngrd$eff <- round(as.vector(eff),3)
  
  #print(head(ngrd),20)
  
  which.species <- as.character(cmod$pmod.data$species[1])
  print(which.species)
  
  ngrd$species <-which.species
  
  ngrd$catch <- round(ngrd$prob * ngrd$measured_catch,3)
  
  ngrd$cpue <- round(ngrd$catch/ngrd$eff,3)
  
  # It doesn't make sense to plot over the entire grid because you don't always have data in every combination of 
  # longitude, latitude, month, and trend. So we need to make an index to tell us what numbers to use when summing the catches
  # over space and time
  
  mm.dat <- paste(pmod.data$longitude,pmod.data$latitude,pmod.data$trend,pmod.data$month)
  mm.grd <- paste(ngrd$longitude,ngrd$latitude,ngrd$trend,ngrd$month)
  mm <- match(mm.grd,mm.dat)
  mm <- ifelse(is.na(mm),F,T)
  ngrd$observation <- mm 
  
  model.data <- ngrd
  
  #print(summary(model.data$catch[ngrd$observation == T]/1000))
  
  filename <- paste('model-data-',which.species,'-',which.flag,'-',which.gear,'-',start.year,'-',end.year,'.csv',sep='')
  
  #print(filename)
  
  write.table(model.data[model.data$observation == TRUE,],file=filename,sep=',',row.names=F,col.names=F)
  
  
  model.data
  

}