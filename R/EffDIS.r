

### Read in EffDIS data from ICCAT ####

install.packages('pbapply')
install.packages('XML')
install.packages('gdata')
#install.packages('rJava')
#install.packages('xlsx')
install.packages('maps')
install.packages('data.table')
install.packages('cluster')
install.packages('doBy')
install.packages('mapdata')
install.packages('maptools')
install.packages('PBSmapping')
install.packages('plyr')
install.packages('rtools')
install.packages('rgdal')
install.packages('rio')

###Load packages

library(gdata)
library(rJava)
#library(xlsx)
library(XML)
library(maps)
library(plyr)
library(vmstools)
library(mapdata)
library(RColorBrewer)
library(rgdal)


source("https://gist.github.com/schaunwheeler/5825002/raw/3526a15b032c06392740e20b6c9a179add2cee49/xlsxToR.r")

source("c:/Users/Doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Scripts/utilityDB.R")

setwd("c:/Users/Doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data")

list.files()

#effdis <- read.table('EffDIS_V0.csv',sep=",",header=T)

# Convert the lats and longs

# df <- data.frame(quad=effdis$QuadID,lat=effdis$Lat5,lon=effdis$Lon5,square=NA)
# df$square<-ac(ifelse(effdis$GearGrpCode=='PS','1x1','5x5'))
# 
# 
# df1<- data.frame(quad=rep(NA,length(df[,1])),lat=rep(NA,length(df[,1])),lon=rep(NA,length(df[,1])),square=rep(NA,length(df[,1])))
# 
# for(i in 1:length(df[,1]))
# {
#   
#   df1[i,] <- latLon(x=df[i,])
# }
# 

#effdis$lon <- df1$lon
#effdis$lat <- df1$lat

write.table(effdis,'effdis.csv',sep=',')

# Read in data

effdis <- read.table('effdis.csv',sep=',')

colnames(effdis)

# 1] "Region"       "RankID"       "ScoreFlag"    "StatusCPC"    "FlagName"     "GearGrpCode"  "YearC"        "TimePeriodID" "QuadID"      
# [10] "Lat5"         "Lon5"         "obsHooks"     "estHooks"     "tot9sp"       "rf"           "CPUE"         "remarks"      "lon"         
# [19] "lat" 


table(effdis$Region)

# AT     MD 
# 188085   5780 
# 

table(effdis$RankID)
table(effdis$ScoreFlag)

# N      Y 
# 79086 114779 

table(effdis$StatusCPC)

# CP   NCC   NCO 
# 86227 22024 85614

table(effdis$FlagName)

# AT_oth                     Belize                     Brasil 
# 76446                        272                       5735 
# China P.R.             Chinese Taipei                       Cuba 
# 1607                      22024                       3145 
# EU.Cyprus                  EU.Espa?a                  EU.Greece 
# 368                      10530                        326 

table(effdis$GearGrpCode)

#LL 
#193865 

table(effdis$TimePeriodID)

# 1     2     3     4     5     6     7     8     9    10    11    12 
# 15333 15275 16113 16201 16587 16625 16230 16317 16403 16903 16084 15794 

table(effdis$TimePeriodID,effdis$YearC)

# Goes back to 1950

table(effdis$QuadID)

# 1      2      3      4 
# 5725  23768  50235 114137 

table(effdis$QuadID)
#  1      2      3      4 
#5725  23768  50235 114137 

table(effdis$Lat5)
# 0     5    10    15    20    25    30    35    40    45    50    55    60    65 
# 26180 26058 24402 17800 17285 20878 19885 23638 13180  2885   943   547   180     4 

table(effdis$Lon5)

#   0     5    10    15    20    25    30    35    40    45    50    55    60    65    70    75    80    85    90    95 
#17129 15709 16682 17936 15928 14999 13850 12316 11526 10755  8368  5828  6025  6449  5262  5263  3217  2054  2678  1891 



#-Load the data and convert them into a 'SpatialPointsDataFrame'

coords      <- SpatialPointsDataFrame(cbind(x=an(ac(effdis$lon)),y=an(ac(effdis$lat))),data=effdis)

geogWGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # Make sure proj is what we think it is.

coords@proj4string <- geogWGS84

plot(coords)


#### Useful global spatial data from here ###

#http://www.vdstech.com/world-data.aspx

#Shapefiles for oceans

oceans <- readOGR(dsn="F:/grouper-backup/input_data_files/WorldSpatialData", layer="OceanSeas") # World seas and oceans

geogWGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # Make sure proj is what we think it is.

oceans@proj4string <- geogWGS84

class(oceans)

head(oceans@data)

table(oceans@data$NAME)

wo <- grep('ATLANTIC',oceans@data$NAME)

ocean.polys <- as.character(sort(unique(oceans@data$NAME)))
ocean.polys[1:10]

atlantic <- oceans[oceans@data$NAME %in% ocean.polys[c(97,117)],]

#Shapefile for countries

world <- readOGR(dsn="F:/grouper-backup/input_data_files/WorldSpatialData/WorldCountryBoundaries", layer="g2008_0") # World seas and oceans

geogWGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # Make sure proj is what we think it is.

world@proj4string <- geogWGS84

class(world)

head(world@data)

table(world@data$ADM0_NAME)

world.polys <- as.character(sort(unique(world@data$ADM0_NAME)))
world.polys

table(world@data$CONTINENT)
table(world@data$REGION)

continent.polys <- unique(world@data$CONTINENT)

atl.countries <- world[world@data$CONTINENT %in% c('Americas','Europe','Africa'),]

africa <- world[world@data$CONTINENT %in% 'Africa',]
w.africa <- world[world@data$REGION %in% 'Western Africa',]


# Get rid of EFFDIS observations on land #

idx <- over(coords,atl.countries)
land      <- rep(NA,length(effdis[,1]))

land[which(is.na(idx[,1])==FALSE)] <- 1
land[which(is.na(idx[,1])==TRUE)] <- 0

effdis$land <- land

effdis<- effdis[effdis$land ==0,]

coords      <- SpatialPointsDataFrame(cbind(x=an(ac(effdis$lon)),y=an(ac(effdis$lat))),data=effdis)

geogWGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # Make sure proj is what we think it is.

coords@proj4string <- geogWGS84

plot(coords)



#Take out effdis data for one year
edis09 <- effdis[effdis$YearC == 2009,]
plot(edis09$lon,edis09$lat)
plot(w.africa,add=T,col='green')


#######Plotting routines#############


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


#- Define grid cell area
resx        <- 5
resy        <- 5

#-Define the area of interest and some reference area


#-Obtain outer region of my areas and VMS positions. This helps to create maps later on with the same dimensions.
bbox        <- bbox(atlantic)
spatBound   <- list(xrange = c(floor(range(bbox["x",])[1]),ceiling(range(bbox["x",])[2])),
                    yrange = c(floor(range(bbox["y",])[1]),ceiling(range(bbox["y",])[2])))
grd         <- createGrid(spatBound$x,spatBound$y,resx,resy,type="SpatialGridDataFrame",exactBorder=T)

grd@proj4string <- geogWGS84

cutbreaksval  <- list(ALL = c(-1,0,10,25,50,100,200,400))
legval        <- list(ALL = c("0","0 <= 10","10 <= 25", "25 <= 50","50 <= 100","100 <= 200","200 <= 400"))
#- Potentially, divide your data by a certain constant and add this constant to the legend title
valdiv        <- 1000 * 60 #combination of converting from minutes to hours and getting a legend value per 1000 hours
unitval       <- c('x 1000 hours per year')


#- Reset values
grd@data[] <- 0


#-Create column to aggregate over (most often, this column already exists and is output from your previous analyses)
#tacsat                        <- intervalTacsat(tacsat,level="vessel",fill.na=T)


idx                           <- over(as(coords,"SpatialPoints"),as(grd,"SpatialGrid"))
effdis$gridID                 <- idx


#- Here we aggregate data to the grid cell. You can aggregate any column you like, we use INTV as an example here.
grd@data[names(table(idx)),1] <- aggregate(effdis$obsHooks,by=list(effdis$gridID),FUN=sum,na.rm=T)$x


plot(1,1,col="white",xlim=spatBound$xrange,ylim=spatBound$yrange,xlab="",ylab="",las=1,cex.lab=xl$cex,font=xl$font,
     asp=1/lonLatRatio(mean(spatBound$xrange),mean(spatBound$yrange)))
coordGrd  <- coordinates(grd)[an(names(table(idx))),]

#-Here we turn each of the grid cells into a polygon. All polygons together make the picture

grdPols <- 
lonLat2SpatialPolygons(lst=lapply(as.list(1:nrow(coordGrd)),function(x){data.frame(SI_LONG=c(coordGrd[x,"s1"]-resx/2,rep(coordGrd[x,"s1"]+resx/2,2),coordGrd[x,"s1"]-resx/2),
                                                                                          SI_LATI=c(rep(coordGrd[x,"s2"]-resy/2,2),rep(coordGrd[x,"s2"]+resy/2,2)))}))
#Look at value ranges:
range(grd@data[an(names(table(idx))),1])

cols<- c("white",colintens)[cut(grd@data[an(names(table(idx))),1]/valdiv,breaks=cutbreaksval$ALL)]
plot(grdPols,col=cols,add=T,border='transparent')
#plot(atl.countries,add=T,col=colland)
map("worldHires",resolution=1,add=T,fill=TRUE,col=colland);map.axes();box()
axis(1);axis(2,las=1); box()




#-Add a legend
legend(x='topright',fill=c('white',colintens),legend=legval$ALL,bg='white',title=unitval,box.lty=1)


#- Add axis and title
title(main="Fishing intensity",outer=F,cex=ct)
mtext(xl$label,side=1,outer=T,line=-3,at=0.5,font=xl$font,cex=xl$cex)
mtext(yl$label,side=2,outer=T,line=-1.5,at=0.5,font=yl$font,cex=yl$cex)                                                                                       


## Pivot table - sum estimated hook numbers over year and flag state

attach(effdis)
effdis <- effdis[effdis$Region=="AT",]

hksbyYrFlag <- aggregate(list(estHooks=estHooks),list(FlagName=FlagName,YearC=YearC),FUN="sum")

map()
points(Lon5*-1,Lat5,pch='.',col='red')



