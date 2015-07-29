
#######3D Plotting routine for EFFDIS #############

three.d.effort.by.year.r <- function(tdata = task2.lf, what.gear = 'LL', what.year = 2005, gridx=5,gridy=5,effort.type='NO.HOOKS', what.flag = 'All',scaling.f=1000000)
{
  #tdata<-t2ce_lf_ll[t2ce_lf_ll$species == 'alb',]; what.gear <- 'LL'; what.year <- 2005; 
  #gridx <- 5; gridy <- 5; what.flag <- 'EU.Cyprus'

  
  tdata1 <- aggregate(list(eff1=tdata$eff1), 
                     by=list(trend=tdata$trend,month=tdata$month,
        flagname=tdata$flagname,longitude=tdata$longitude,latitude=tdata$latitude),unique)
  
  
  
  if(what.flag == 'All'){
  tdata1 <- tdata1[tdata1$geargrpcode == what.gear & tdata1$month < 13 & tdata1$year == what.year & tdata1$eff1type == effort.type,]
  }
  else{
    
    tdata1 <- tdata1[tdata1$geargrpcode == what.gear & tdata1$month < 13 & tdata1$year == what.year & tdata1$eff1type == effort.type & tdata1$flagname == what.flag,]
  }
  
  dd <- dim(tdata1)
  ulocs <- length(unique(tdata1$longitude))
  if (dd[1] < 1 | ulocs < 6)
    {
    print('No Data')
  }
  else{
    
  coords      <- SpatialPointsDataFrame(cbind(x=an(ac(tdata1$longitude)),y=an(ac(tdata1$latitude))),data=tdata1[,c(4,5)])
  
  geogWGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # Make sure proj is what we think it is.
  
  coords@proj4string <- geogWGS84
  
  #plot(coords)
  
#- Define grid cell area
resx        <- gridx
resy        <- gridy

cl          <- 1.1  #cex.lab
ca          <- 1    #cex.axis
fonts       <- 2    #font
xl          <- list(label="Longitude",font=fonts,cex=cl) #x-label
yl          <- list(label="Latitude",font=fonts,cex=cl)  #y-label
zl          <- list(font=fonts,cex=cl) #z-label (if any)
colintens   <- brewer.pal(6,"YlOrRd")  #colour for fishing intensity
colland     <- brewer.pal(9,"PiYG")[8] #colour of land
colgrey     <- brewer.pal(9,"Greys")   #colour of grey shades
figtype     <- "tiff"                  #figure extension type
parmar      <- rep(2,4)                #panel settings
paroma      <- (c(6,6,2,2)+0.1)        #panel settings
reso        <- 1                       #dpi setting (1=100dpi)

#-Obtain outer region of data. This helps to create maps later on with the same dimensions.

bbox        <- bbox(coords)
spatBound   <- list(xrange = c(floor(range(bbox["x",])[1]),ceiling(range(bbox["x",])[2])),
                    yrange = c(floor(range(bbox["y",])[1]),ceiling(range(bbox["y",])[2])))
grd         <- createGrid(spatBound$x,spatBound$y,resx,resy,type="SpatialGridDataFrame",exactBorder=T)

grd@proj4string <- geogWGS84

#- Reset values
grd@data[] <- 0


#-Create column to aggregate over (most often, this column already exists and is output from your previous analyses)
#tacsat                        <- intervalTacsat(tacsat,level="vessel",fill.na=T)

idx                           <- over(as(coords,"SpatialPoints"),as(grd,"SpatialGrid"))
tdata1$gridID                 <- idx


#- Here we aggregate data to the grid cell. You can aggregate any column you like, we use INTV as an example here.
grd@data[names(table(idx)),1] <- aggregate(tdata1$eff1,by=list(tdata1$gridID),FUN=sum,na.rm=T)$x

#Look at value ranges:
rr <- range(grd@data[an(names(table(idx))),1])

cutbreaksval  <- list(ALL = c(-1,0,10,25,50,100,150,200))
legval        <- list(ALL = c("0","1 <= 10","10 <= 25", "25 <= 50","50 <= 100","100 <= 200","200 <= 400"))
#- Potentially, divide your data by a certain constant and add this constant to the legend title
valdiv        <- scaling.f # scaling the data
unitval       <- paste('x',valdiv,'hooks')


plot(1,1,col="white",xlim=spatBound$xrange,ylim=spatBound$yrange,xlab="",ylab="",
     xaxt="n",yaxt="n",las=1,cex.lab=xl$cex,font=xl$font,
     asp=1/lonLatRatio(mean(spatBound$xrange),mean(spatBound$yrange)))
coordGrd  <- coordinates(grd)[an(names(table(idx))),]

#-Here we turn each of the grid cells into a polygon. All polygons together make the picture

grdPols <- 
  lonLat2SpatialPolygons(lst=lapply(as.list(1:nrow(coordGrd)),function(x){data.frame(SI_LONG=c(coordGrd[x,"s1"]-resx/2,rep(coordGrd[x,"s1"]+resx/2,2),coordGrd[x,"s1"]-resx/2),
                                                                                     SI_LATI=c(rep(coordGrd[x,"s2"]-resy/2,2),rep(coordGrd[x,"s2"]+resy/2,2)))}))

cols<- c("white",colintens)[cut(grd@data[an(names(table(idx))),1]/valdiv,breaks=cutbreaksval$ALL)]
plot(grdPols,col=cols,add=T,border='transparent')
#plot(atl.countries,add=T,col=colland)
map("world",resolution=1,add=T,fill=TRUE,col=colland);map.axes();#box()
#axis(1);axis(2,las=1); box()


#-Add a legend
legend(x='topright',fill=c('white',colintens),legend=legval$ALL,bg='white',title=unitval,box.lty=1)


#- Add axis and title
title(main=paste(what.flag,what.year,what.gear,effort.type),outer=F,cex=cl)
#mtext(xl$label,side=1,outer=T,line=-3,at=0.5,font=xl$font,cex=xl$cex)
#mtext(yl$label,side=2,outer=T,line=-1.5,at=0.5,font=yl$font,cex=yl$cex)                                                                                       


grdPolsDF              <- as(grdPols,"SpatialPolygonsDataFrame")
grdPolsDF@data         <- data.frame(value=grd@data[an(names(table(idx))),1],color=cols)
proj4string(grdPolsDF) <- CRS("+proj=longlat +ellps=WGS84")
#dir.create("/home/doug/effdis/shp_files/")
#setwd("/home/doug/effdis/shp_files/longline")
#layer.name <- paste(what.flag,what.year,what.gear,effort.type,sep="_")
#writeOGR(grdPolsDF, dsn = '.', layer = layer.name, overwrite_layer=TRUE,driver = "ESRI Shapefile")


}
}






