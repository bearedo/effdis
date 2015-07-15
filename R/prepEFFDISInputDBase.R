### R-script to process effdis data and export them ###

## Load packages ##

library(rio)
library(spatial)
library(sp)
library(doBy)
library(rgdal)
library(RODBC)
library(RColorBrewer)
library(ggplot2)
library(vmstools)
library(gam)
library(maps)
library(mapdata)
library(COZIGAM)
library(RODBC)

setwd("/home/doug/effdis")

## source some useful functions  ##

source("/home/doug/effdis/R/utilityDB.R")
source("/home/doug/effdis/R/trend.r")


## Load data sets from ICCAT ## 

mean_weights <- import("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/effdis_2011/input/MeanWeights2011.xlsx")
fleet_ranks  <- import("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/effdis_2011/input/revisedFleetRanks.xlsx")
t1det9sp     <- import("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/effdis_2011/input/t1det_9sp.xlsx")
t2ceLL       <- import("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/effdis_2011/input/t2ce_LL_raw5x5.xlsx")
t2ce         <- read.table("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/effdis_2011/input/t2ce.csv",sep=",",header=T)
flags        <- import("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/effdis_2011/input/flags.csv")

colnames(t1det9sp) <- tolower(colnames(t1det9sp))
colnames(t2ce) <- tolower(colnames(t2ce))
t2ce$trend <- trend.r(year=t2ce$yearc,month=t2ce$timeperiodid,start.year=1950)
dimnames(t2ce)[[2]][55:56] <- c('longitude','latitude') # Have to be different.

# Get rid of all observations on land and add on string for region, Med or Atlantic #

geogWGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # Make sure proj is what we think it is.

t2ce.spdf      <- SpatialPointsDataFrame(cbind(x=an(ac(t2ce$longitude)),y=an(ac(t2ce$latitude))),data=t2ce[,c(1,3,4)]) # Create spatial points dataframe with t2ce data
t2ce.spdf@proj4string <- geogWGS84

seas <- readOGR(dsn="/home/doug/effdis/data", layer="World_Seas") # World seas and oceans
seas@proj4string <- geogWGS84

seas.polys <- as.character(sort(unique(seas@data$NAME)))
seas.polys[1:10]

wo <- grep('Atl',seas.polys) # Find all relevant polygons
wi <- grep('Med',seas.polys)
wj <- grep('Adriatic',seas.polys) # Find all relevant polygons
wk <- grep('Aegean',seas.polys)
wl <- grep('Balearic',seas.polys) # Find all relevant polygons
wm <- grep('Bay of Biscay',seas.polys)
wn <- grep('Bristol',seas.polys) # Find all relevant polygons
wp <- grep('Caribbean',seas.polys)
wq <- grep('Celtic',seas.polys) # Find all relevant polygons
wr <- grep('English Channel',seas.polys)
ws <- grep('Lawrence',seas.polys) # Find all relevant polygons
wt <- grep('Inner Seas',seas.polys)
wu <- grep('Ionian',seas.polys) # Find all relevant polygons
wv <- grep('Irish',seas.polys)
wx <- grep('North Sea',seas.polys) # Find all relevant polygons
wz <- grep('Gibra',seas.polys) # Find all relevant polygons
wa <- grep('Ligurian',seas.polys) # Find all relevant polygons
wzz <- grep('Tyrr',seas.polys) # Find all relevant polygons
wxx <- grep('Alb',seas.polys) # Find all relevant polygons
wmm <- grep('Mex',seas.polys) # Find all relevant polygons

atlantic <- seas[seas@data$NAME %in% seas.polys[c(wo,wm,wn,wp,wq,wr,ws,wt,wv,wx,wmm)],] # create object of Atlantic polygons
med <-      seas[seas@data$NAME %in% seas.polys[c(wi,wj,wk,wl,wu,wz,wa,wzz,wxx)],]

plot(atlantic) # check
plot(med)

idx.at <- over(t2ce.spdf,atlantic)
idx.med <- over(t2ce.spdf,med)

at.sea      <- rep(NA,length(t2ce.spdf[,1]))


at.sea[which(is.na(idx.at[,1])==F)] <- 'AT'
at.sea[which(is.na(idx.med[,1])==F)] <- 'MED'

t2ce$region <- at.sea

plot(t2ce$longitude[is.na(t2ce$region)],t2ce$latitude[is.na(t2ce$region)],pch='.')
plot(atlantic,add=T)
plot(med,add=T)
map('world',add=T)

t2ce$region[is.na(t2ce$region)] <- 'LAND'

#   AT   LAND    MED 
#476145  80975  10381 


## Load codes ## 

codes_effort_types <- import("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/CODES_EffortTypes.xls")
codes_species <- import("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/CODES_Species.xlsx")
codes_sampling_areas <- import("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/CODES_SamplingAreas.xls")
codes_square_types <- import("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/CODES_SquareTypes.xls")
codes_time_periods <- import("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/CODES_TimePeriods.xls")
codes_square_types<- import("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/CODES_SquareTypes.xls")

## Data preparation ##

# Add on fleet codes and flagnames

#t2ce$FleetCode <- flags$FleetCode[match(t2ce$FleetID,flags$FleetID)]
#t2ce$FlagName <- flags$FlagName[match(t2ce$FleetCode,flags$FleetCode)] 

# Create center point of each grid using Laurie's code lonLat #

df <- data.frame(quad=t2ce$QuadID,lat=t2ce$Lat,lon=t2ce$Lon,square=t2ce$SquareTypeCode) # simplify data.frame
df$square <- as.character(df$square) # function requires input to be 'character'

hl <- length(df[,1])
ndf <- data.frame(quad=rep(NA,hl),lat=rep(NA,hl),lon=rep(NA,hl),square=rep(NA,hl))
for(i in 1:hl){
  ndf[i,] <- latLon(df[i,]) # calculate the central lat long of each grid looping thro' each row
}


t2ce$longitude <- ndf$lon # add on lat and long to main database
t2ce$latitude <- ndf$lat


## Connect to postgres database on ICCAT server with RODBC and upload the data NB. you must edit etc/odbc.ini file and install the correct drivers etc.

# Connect #

chan <- odbcConnect("effdis-tuna-cc1", case="postgresql", believeNRows=FALSE)
sqlTables(chan)  # List all tables in the DB

# Load data

#sqlQuery(chan,'drop table t2ce') # upload the data. Takes ages so only need to do once.
#sqlSave(chan,t2ce,tablename='t2ce')
#sqlSave(chan,mean_weights,tablename='mean_weights') # 
#sqlSave(chan,fleet_ranks,tablename='fleet_ranks') # A worksheet
#sqlSave(chan,t1det9sp,tablename='t1det9sp') # task1 data for raising
#sqlSave(chan,flags,tablename='codes_flags_fleets') 


# Load codes #

#sqlSave(chan,codes_effort_types,tablename='codes_effort_types')
#sqlSave(chan,codes_species,tablename='codes_species')
#sqlSave(chan,codes_sampling_areas,tablename='codes_sampling_areas')
#sqlSave(chan,codes_square_types,tablename='codes_square_types')
#sqlSave(chan,codes_time_periods,tablename='codes_time_periods')

# Get the data from the database to check #

#t2ce <- sqlQuery(chan, "SELECT * FROM t2ce LIMIT 5;") # Extract first five rows
#t2ce$trend <- trend.r(year=t2ce$yearc,month=t2ce$timeperiodid,start.year=1950)


# Add comments #
# 
# sqlQuery(chan, "COMMENT ON TABLE t2ce IS 'These are ICCAT task 2 effort data supplied by Carlos Palma carlos.palma@iccat.int';");
# sqlQuery (chan,"COMMENT ON COLUMN t2ce.yearc IS 'Year';\n");
# sqlQuery(chan,"COMMENT ON COLUMN t2ce.lon IS 'see table codes_square_types ';\n");
# sqlQuery(chan,"COMMENT ON COLUMN t2ce.lat IS 'see table codes_square_types';\n");
# sqlQuery(chan,"COMMENT ON COLUMN t2ce.longitude IS 'Center longitude of grid cell calculated according to latLon function of Kell';\n");
# sqlQuery(chan,"COMMENT ON COLUMN t2ce.latitude  IS 'Center latitude of grid cell calculated according to latLon function of Kell';\n");
# sqlQuery(chan,"COMMENT ON COLUMN t2ce.timeperiodid IS '1-12 is month, 13-16 is quarter, 17 is year, 18-19 are first and second semester';\n");
# 
# sqlQuery(chan, "COMMENT ON TABLE t1det9sp IS 'These are ICCAT Task 1 landings data for raising the Task 2 data. Supplied by Carlos Palma carlos.palma@iccat.int';");
# 
# 
# 
# # Add geo point #
# 
# sqlQuery(chan,"ALTER TABLE public.t2ce ADD COLUMN the_point geometry(Point,4326);\n");
# sqlQuery(chan,"UPDATE public.t2ce SET the_point = ST_SETSRID(ST_MAKEPOINT(longitude,latitude),4326);\n"); 
# 
# # Add index for geopoint
# 
# sqlQuery (chan,"CREATE INDEX t2ce_the_point ON t2ce USING GIST (the_point);\n");
# 
# # Create separate smaller table with unique locations to add depths to. #
# 
# sqlQuery (chan, "SELECT DISTINCT longitude,latitude, the_point INTO t2ce_distinct_locations from t2ce;")
# sqlQuery(chan, "COMMENT ON TABLE t2ce_distinct_locations IS 'These are unique or distinct ICCAT task 2 effort data locations derived from table t2ce. It was made to facilitate easier matching to other potentially useful covariates such as';");
# 

 odbcClose(chan)
# 













