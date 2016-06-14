find.EEZ <-
  function(input=skj)
  {
    
    
    tdata1 <- input
    # Function takes a data frame of lats and longs and adds the relevant EEZ as a column. That is all. It does no aggregation.
    
    coords      <- SpatialPointsDataFrame(cbind(x=as.numeric(as.character(tdata1$longitude)),y=as.numeric(as.character(tdata1$latitude))),data=tdata1[,c(4,5)])
    
    geogWGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # Make sure proj is what we think it is.
    
    coords@proj4string <- geogWGS84
    
    
    
    
    data(eez) # included in EffdisR library
    
    sp      <- SpatialPolygons(eez@polygons)
    sp@proj4string <- geogWGS84
    idx.eez                       <- over(coords,sp)
    tdata1$EEZ <-eez$EEZ[idx.eez]
    
    #t1<- tapply(tdata1$measured_catch[tdata1$year==2014],tdata1$EEZ[tdata1$year==2014],sum,na.rm=T)
    #t2 <- t1[!is.na(t1)]
    
    tdata1
  }

#skj1 <- find.EEZ(input=skj)
#tapply(skj1$measured_catch,list(skj1$EEZ,skj1$geargrpcode),sum,na.rm=T)
