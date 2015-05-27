### R-script to import effdis data ###

#install.packages('rio')

library(rio)
library(spatial)
library(sp)
library(doBy)
library(rgdal)
library(RODBC)
library(RColorBrewer)
library(ggplot2)
library(vmstools)


#### Read in data for 2011 ###

# Set working directory #

setwd('/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/effdis_2011/input')

source("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Scripts/utilityDB.R")
source("/home/doug/effdis/R/utilityDB.R")
source("/home/doug/effdis/R/trend.r")

# List files in directory #

list.files()

mw <- import("MeanWeights2011.xlsx")
fr <- import("revisedFleetRanks.xlsx")
t1det9sp <- import("t1det_9sp.xlsx")
t2ceLL <- import("t2ce_LL_raw5x5.xlsx")

# First five rows of Task 2 data

head(t2ceLL)

#How many regions

table(t2ceLL$Region)

#AT    MD 
#99488  2026 

# How many years

ys <- table(t2ceLL$YearC)

summary(t2ceLL$YearC)

#Remove the missing value

t2ceLL <- t2ceLL[!is.na(t2ceLL$YearC),]

#How many Flag states 

table(t2ceLL$FlagName)

# Belize                     Brasil                 China P.R. 
# 238                       8894                       1067 
# Chinese Taipei                       Cuba                  EU.Cyprus 
# 21908                       2288                        157 
# EU.España                  EU.Greece                   EU.Italy 
# 9210                        262                         77 
# EU.Malta                EU.Portugal                      Japan 
# 44                        600                      34298 
# Korea Rep.                      Maroc                     Mexico 
# 6693                         65                        442 
# Namibia                     Panama                Philippines 
# 727                       2763                        338 
# South Africa St. Vincent and Grenadines        Trinidad and Tobago 
# 824                        874                         60 
# Uruguay                     U.S.A.                   U.S.S.R. 
# 896                       6550                         53 
# Vanuatu                  Venezuela 
# 201                       1985 

# Some countries report numbers, some weights and some both

table(t2ceLL$DSetType)
# n-    nw    -w 
# 44290 21437 35787 

table(t2ceLL$DSetType,t2ceLL$FlagName)

# Belize Brasil China P.R. Chinese Taipei  Cuba EU.Cyprus EU.España EU.Greece EU.Italy
# n-      0    813          0              0  2247         0         0         0        0
# nw      0      0         79          11152     0         0      8881        44        0
# -w    238   8081        988          10756    41       157       329       218       77
# 
# EU.Malta EU.Portugal Japan Korea Rep. Maroc Mexico Namibia Panama Philippines South Africa
# n-        0           0 34298        194     0      0       0      0           0            0
# nw       18           0     0        212     0     19       0      0           0            0
# -w       26         600     0       6287    65    423     727   2763         338          824
# 
# St. Vincent and Grenadines Trinidad and Tobago Uruguay U.S.A. U.S.S.R. Vanuatu Venezuela
# n-                          0                   0       0   6550        0       0       188
# nw                          0                   0     831      0        0     201         0
# -w                        874                  60      65      0       53       0      1797
# 



# Year month coverage in the Atlantic##

ymc <- table(t2ceLL$YearC[t2ceLL$Region=='AT'],t2ceLL$TimePeriodID[t2ceLL$Region=='AT'])
dimnames(ymc)[[1]] <- c(min(t2ceLL$YearC):max(t2ceLL$YearC)) 
dimnames(ymc)[[2]] <- month.abb

image(min(t2ceLL$YearC):max(t2ceLL$YearC),1:12,ymc,xaxt='n',yaxt='n',xlab="",ylab="",col=terrain.colors(100))
contour(min(t2ceLL$YearC):max(t2ceLL$YearC),1:12,ymc,add=T)
axis(side=1,at=min(t2ceLL$YearC):max(t2ceLL$YearC),label=row.names(ymc))
axis(side=2,at=1:12,label=month.abb[1:12])

# Note - data coverage has increased markely with what appear to be steps in around 1974 and 1997.

# Add trend column (useful for time-series analysis)

t2ceLL$trend <- trend.f(year=t2ceLL$YearC,month=t2ceLL$TimePeriodID,start.year=1956)


### convert spatial information to grid centroids with Laurie's code ##

# Note: apparently even though it says 1x1 in the original data they have actually been converted to 5x5.

table(t2ceLL$SquareTypeCode)

#1x1   5x5 
# 11874 89640 

df <- data.frame(quad=t2ceLL$QuadID,lat=t2ceLL$Lat5,lon=t2ceLL$Lon5,square=ac(t2ceLL$SquareTypeCode))

df1<- data.frame(quad=rep(NA,length(df[,1])),lat=rep(NA,length(df[,1])),lon=rep(NA,length(df[,1])),square=rep(NA,length(df[,1])))

for(i in 1:length(df[,1]))
{
  df1[i,] <- latLon(x=df[i,])
}

t2ceLL$lon <- df1$lon
t2ceLL$lat <- df1$lat


#Task 1: 

head(t1det9sp)

# Gear Groups
table(t1det9sp$GearGrp)

# BB    GN    HL    HP    HS    LL    PS    RR    SP    SU    TL    TN    TP    TR    TW    UN 
# 2719  1143   860   271    81 11683  2661  1022   361   719    90     9   766   527   363  2008

table(t1det9sp$Flag)
t1ct <- t1det9sp[t1det9sp$Flag == 'Chinese Taipei' & t1det9sp$Region == 'AT',]
table(t1ct$Species)
#Just long-lines

t1ct <- t1ct[t1ct$GearGrp == 'LL',]

t1ct1 <- aggregate(Qty_t ~ YearC+Species,data=t1ct,sum)

library(lattice)

xyplot(Qty_t~YearC|Species,data=t1ct1)



## Task2: Total number of hooks observed ##

table(t2ceLL$Eff1Type)

#NO.HOOKS 
#101514 

aggregate(Eff1~FlagName,FUN=sum,data=t2ceLL)

# 1                       Belize   10312448
# 2                      Brasil  227942668
# 3                  China P.R.  264406176
# 4              Chinese Taipei 3486467164
# 5                        Cuba  126234246
# 6                   EU.Cyprus   21714358
# 7                   EU.España 1098584600
# 8                   EU.Greece   30651295
# 9                    EU.Italy   23771520
# 10                   EU.Malta    4603479
# 11                EU.Portugal   13525244
# 12                      Japan 3408944634
# 13                 Korea Rep.  322310615
# 14                      Maroc    5661000
# 15                     Mexico   20837767
# 16                    Namibia   28378124
# 17                     Panama  355904246
# 18                Philippines   25861865
# 19               South Africa   12708100
# 20 St. Vincent and Grenadines  143177015
# 21        Trinidad and Tobago     482647
# 22                    Uruguay   28173255
# 23                     U.S.A.  162048107
# 24                   U.S.S.R.    6376104
# 25                    Vanuatu   21941130
# 26                  Venezuela   34653748

# Chinese Taipei in Atlantic #

ct <- t2ceLL[t2ceLL$FlagName == 'Chinese Taipei'& t2ceLL$Region == 'AT',]

plot(ct$trend,log(ct$Eff1),pch='.',xaxt='n',ylab='Number of hooks',xlab="year")
lines(supsmu(ct$trend,log(ct$Eff1)          ),col='green')
xl <- seq(min(ct$YearC),max(ct$Year),by=5)
axis(1,at=seq(min(ct$trend),max(ct$trend),by=60),labels=as.character(xl))
abline(v=seq(min(ct$trend),max(ct$trend),by=60))


#Sum by year and month

ct1 <- aggregate(Eff1~trend+TimePeriodID,data=ct,sum)
ct1 <- orderBy(~trend+TimePeriodID,data=ct1)


plot(ct1$trend,log(ct1$Eff1),type='l',xaxt='n',ylab='Number of hooks',xlab="year")
lines(supsmu(ct1$trend,log(ct1$Eff1)          ),col='green')
xl <- seq(min(ct$YearC),max(ct$Year),by=5)
axis(1,at=seq(min(ct$trend),max(ct$trend),by=60),labels=as.character(xl))
abline(v=seq(min(ct$trend),max(ct$trend),by=60),lty=2,col='blue')

#Have a look at the relationship between n hooks and species weights caught

cor(cbind(ct$Eff1,ct$ALB,ct$BFT,ct$BET,ct$SKJ,ct$YFT,ct$SWO,ct$BUM,ct$SAI,ct$WHM))

par(mfrow=c(1,1))
plot(ct$ALB,sqrt(ct$Eff1),pch='.')
plot(ct$BFT,sqrt(ct$Eff1),pch='.')
plot(ct$BET,sqrt(ct$Eff1),pch='.')
plot(ct$SKJ,sqrt(ct$Eff1),pch='.')
plot(ct$YFT,sqrt(ct$Eff1),pch='.')
plot(ct$SWO,sqrt(ct$Eff1),pch='.')
plot(ct$BUM,sqrt(ct$Eff1),pch='.')
plot(ct$SAI,sqrt(ct$Eff1),pch='.')
plot(ct$WHM,sqrt(ct$Eff1),pch='.')
plot(ct$Total,sqrt(ct$Eff1),pch='.')
plot(log(ct$Total),log(ct$Eff1),pch='.')

#### Plotting in spatial dimension ###

nct <- ct[ct$YearC == 1990,]

coords      <- SpatialPointsDataFrame(cbind(x=an(ac(nct$lon)),y=an(ac(nct$lat))),data=nct)

geogWGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # Make sure proj is what we think it is.

coords@proj4string <- geogWGS84

plot(coords,type='p',pch='.')
map('worldHires',add=T,fill=T,col='green')

#### Useful global spatial data from here ###

#http://www.vdstech.com/world-data.aspx

#Shapefiles for oceans

oceans <- readOGR(dsn="/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data", layer="OceanSeas") # World seas and oceans

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

world <- readOGR(dsn="/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data", layer="world") # World seas and oceans

geogWGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # Make sure proj is what we think it is.

world@proj4string <- geogWGS84

class(world)

head(world@data)

table(world@data$NAME)

world.polys <- as.character(sort(unique(world@data$NAME)))
world.polys

atl.countries <- world[world@data$NAME %in% world.polys[c(3,6,7,9,23,25,27,29,35,
                                                          37,39,41,43,47,49,53,55,59,61,63,65,67,69,
                                                          75,79,81,83,85,87,89,91,93,101,105,107,111,123,
                                                          133,137,147,153,159,171,175,181,183,185,187,189,191,
                                                          197,199,201,203,209,211,213,215,217,219,223,237,239,2,
                                                          10,12,14,16,20,24,28,30,34,38,4,42,46,50,52,54,56,
                                                          58,60,64,66,68,70,74,78,80,82,84,86,88,90,92,94,96,
                                                          102,104,110,120,122,124,126,128,130,134,136,138,140,144,
                                                          146,148,150,154,158,168,170,174,178,180,182,184,186,188,
                                                          196,198,202,204,206,216,220,222,224,228,230,234,236,238
                                                          )],]


plot(coords,col='red',pch='.',axes=T)
plot(world,fill=T,col='green',add=T,axes=T)
plot(atlantic,add=T)

# Get rid of EFFDIS observations on land #

idx <- over(coords,atl.countries)
land      <- rep(NA,length(nct[,1]))

land[which(is.na(idx[,1])==FALSE)] <- 1
land[which(is.na(idx[,1])==TRUE)] <- 0

nct$land <- land

nct<- nct[nct$land ==0,] # Chuck out data on land.

coords      <- SpatialPointsDataFrame(cbind(x=an(ac(nct$lon)),y=an(ac(nct$lat))),data=nct)

geogWGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # Make sure proj is what we think it is.

coords@proj4string <- geogWGS84

plot(coords)

#######Plotting routines#############
#- Define grid cell area
resx        <- 5
resy        <- 5


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

#-Obtain outer region of my areas and VMS positions. This helps to create maps later on with the same dimensions.
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
nct$gridID                 <- idx


#- Here we aggregate data to the grid cell. You can aggregate any column you like, we use INTV as an example here.
grd@data[names(table(idx)),1] <- aggregate(nct$Eff1,by=list(nct$gridID),FUN=sum,na.rm=T)$x

#Look at value ranges:
rr <- range(grd@data[an(names(table(idx))),1])

cutbreaksval  <- list(ALL = c(-1,0,10,25,50,100,150,200))
legval        <- list(ALL = c("0","0 <= 10","10 <= 25", "25 <= 50","50 <= 100","100 <= 200","200 <= 400"))
#- Potentially, divide your data by a certain constant and add this constant to the legend title
valdiv        <- 1000000 #combination of converting from minutes to hours and getting a legend value per 1000 hours
unitval       <- c('x 1000000 million hooks')


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
map("worldHires",resolution=1,add=T,fill=TRUE,col=colland);map.axes();#box()
#axis(1);axis(2,las=1); box()




#-Add a legend
legend(x='topright',fill=c('white',colintens),legend=legval$ALL,bg='white',title=unitval,box.lty=1)


#- Add axis and title
title(main="Fishing intensity",outer=F,cex=cl)
mtext(xl$label,side=1,outer=T,line=-3,at=0.5,font=xl$font,cex=xl$cex)
mtext(yl$label,side=2,outer=T,line=-1.5,at=0.5,font=yl$font,cex=yl$cex)                                                                                       














