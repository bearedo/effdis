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
#library(COZIGAM)
library(RODBC)
library(reshape2)
library(mgcv)

setwd("/home/doug/effdis")

## source some useful functions  ##

source("/home/doug/effdis/R/utilityDB.R")
source("/home/doug/effdis/R/trend.r")

chan <- odbcConnect("effdis-tuna-cc1", case="postgresql", believeNRows=FALSE)
chan <- odbcConnect("effdis-local", case="postgresql", believeNRows=FALSE) # Local machine

## Load data sets from ICCAT ## 

mean_weights <- import("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/effdis_2011/input/MeanWeights2011.xlsx")
fleet_ranks  <- import("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/effdis_2011/input/revisedFleetRanks.xlsx")
t1det9sp     <- import("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/effdis_2011/input/t1det_9sp.xlsx")
t2ceLL       <- import("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/effdis_2011/input/t2ce_LL_raw5x5.xlsx")
t2ce         <- read.table("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/effdis_2011/input/t2ce.csv",sep=",",header=T)
flags        <- import("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/effdis_2011/input/flags.csv")

workplan <- import('/home/doug/effdis/data/EFFDIS-Workplan.xlsx')
sqlSave(chan,workplan,'workplan',rownames=FALSE)


colnames(t1det9sp) <- tolower(colnames(t1det9sp))
t1det9sp$species <- tolower(t1det9sp$species)
colnames(t2ce) <- tolower(colnames(t2ce))
t2ce$trend <- trend.r(year=t2ce$yearc,month=t2ce$timeperiodid,start.year=1950)
dimnames(t2ce)[[2]][55:56] <- c('longitude','latitude') # Have to be different.
sp9 <- data.frame(alb=t2ce$alb,bft=t2ce$bft,bet=t2ce$bet,skj=t2ce$skj,yft=t2ce$yft,swo=t2ce$swo,bum=t2ce$bum,sai=t2ce$sai,whm=t2ce$whm) 
t2ce$totsp9 <- apply(sp9,1,sum)

# Tidy up flag codes using those in t2ceLL file supplied by Carlos - all others get to be 'other'.

flgs <- as.character(sort(unique(t2ceLL$FlagName)))
mm <- match(as.character(t2ce$flagname),flgs)

t2ce$flagname <- as.character(t2ce$flagname)
t2ce$flagname[is.na(mm)] <- 'Other'

mm1 <- match(as.character(t1det9sp$flag),flgs)
t1det9sp$flag <-as.character(t1det9sp$flag)
t1det9sp$flag[is.na(mm1)] <- 'Other'

# Identify all observations on land and add on string for region, Med or Atlantic #

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
wpa  <- grep('Pacific',seas.polys)
gog  <- grep('Guin',seas.polys)

atlantic <- seas[seas@data$NAME %in% seas.polys[c(wo,wm,wn,wp,wq,wr,ws,wt,wv,wx,wmm,gog)],] # create object of Atlantic polygons
med <-      seas[seas@data$NAME %in% seas.polys[c(wi,wj,wk,wl,wu,wz,wa,wzz,wxx)],]
pacific   <- seas[seas@data$NAME %in% seas.polys[c(wpa)],]

#Shapefile for countries

world <- readOGR(dsn="/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data", layer="world") # World seas and oceans
geogWGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # Make sure proj is what we think it is.
world@proj4string <- geogWGS84

world.polys <- as.character(sort(unique(world@data$NAME)))
world.polys

usa <- world[world@data$NAME %in% world.polys[223],]
canada <- world[world@data$NAME %in% world.polys[38],]
mexico <- world[world@data$NAME %in% world.polys[140],]
columbia <- world[world@data$NAME %in% world.polys[47],]
venezuela <- world[world@data$NAME %in% world.polys[228],]
belize <- world[world@data$NAME %in% world.polys[23],]
brazil <- world[world@data$NAME %in% world.polys[30],]
guayana <- world[world@data$NAME %in% world.polys[92],]
suriname <- world[world@data$NAME %in% world.polys[202],]
frenchguiana <- world[world@data$NAME %in% world.polys[75],]

america <- world[world@data$NAME %in% world.polys[c(9,16,20,23,25,39,55,60,61,85,86,105,93,136,146,154,182,183,181,185,223,52,168,
                                                    65,38,88,157,140,47,228,23,30,92,202,75
)],]


wsahara <- world[world@data$NAME %in% world.polys[234],]
morocco <- world[world@data$NAME %in% world.polys[147],]
mauritania <- world[world@data$NAME %in% world.polys[137],]
senegal <- world[world@data$NAME %in% world.polys[189],]
gambia <- world[world@data$NAME %in% world.polys[79],]
guinea <- world[world@data$NAME %in% world.polys[90],]
sl <- world[world@data$NAME %in% world.polys[191],]
ghana <- world[world@data$NAME %in% world.polys[82],]
togo <- world[world@data$NAME %in% world.polys[211],]
nigeria <- world[world@data$NAME %in% world.polys[159],]
cam <- world[world@data$NAME %in% world.polys[37],]
gab <- world[world@data$NAME %in% world.polys[78],]
congo <- world[world@data$NAME %in% world.polys[49],]
angola <- world[world@data$NAME %in% world.polys[6],]
                    

africa <- world[world@data$NAME %in% world.polys[c(234,50,3,29,123,158,199,41,78,66,24,34,42,53,147,137,189,122,
                                                   79,74,90,91,82,174,191,82,211,159,37,78,49,6,198,238,239,197,150
)],]

plot(atlantic) # check
plot(med,add=T)
plot(africa,add=T,col='blue')
plot(america,add=T,col='yellow')
#plot(t2ce.spdf,add=T)

#idx.at <- over(t2ce.spdf,atlantic)
#idx.med <- over(t2ce.spdf,med)
#idx.pac <- over(t2ce.spdf,pacific)#
#idx.america <- over(t2ce.spdf,america)
#idx.africa  <- over(t2ce.spdf,africa)

atl <- as.numeric(row.names(idx.at[!is.na(idx.at[,1]),]))
md <- as.numeric(row.names(idx.med[!is.na(idx.med[,1]),]))
pac <- as.numeric(row.names(idx.pac[!is.na(idx.pac[,1]),]))
af <- as.numeric(row.names(idx.africa[!is.na(idx.africa[,1]),]))
am <- as.numeric(row.names(idx.america[!is.na(idx.america[,1]),]))

plot(world,xlim=c(-20,20))
points(t2ce.spdf[c(af,am),],pch='.')
points(t2ce.spdf[c(atl),],pch='.',col='red')
points(t2ce.spdf[c(pac),],pch='.',col='green')
points(t2ce.spdf[c(md),],pch='.',col='blue')


at.sea      <- rep(NA,length(t2ce.spdf[,1]))

# If the observation is on land in an african or american country it gets to be 'Atlantic'

at.sea[atl] <- 'AT'
at.sea[md] <- 'MD'
at.sea[pac] <- 'PA'
at.sea[af] <- 'AT'
at.sea[am] <- 'AT'
at.sea[is.na(at.sea)] <-'LAND'
table(at.sea)
t2ce$region <- at.sea

plot(world)
points(t2ce$longitude[t2ce$region == 'LAND'],t2ce$latitude[t2ce$region == 'LAND'],pch='x',col='cyan')
points(t2ce$longitude[t2ce$region == 'LAND'],t2ce$latitude[t2ce$region == 'LAND'],pch='x',col='cyan')


## Load codes ## 

codes_effort_types <- import("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/CODES_EffortTypes.xls")
codes_species <- import("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/CODES_Species.xlsx")
codes_sampling_areas <- import("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/CODES_SamplingAreas.xls")
codes_square_types <- import("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/CODES_SquareTypes.xls")
codes_time_periods <- import("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/CODES_TimePeriods.xls")
codes_square_types<- import("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/CODES_SquareTypes.xls")

## Data preparation ##

# Add on fleet codes and flagnames

t2ce$fleetcode <- flags$FleetCode[match(t2ce$fleetid,flags$FleetID)]
t2ce$flagname <- flags$FlagName[match(t2ce$fleetcode,flags$FleetCode)] 

# Create center point of each grid using Laurie's code lonLat #

df <- data.frame(quad=t2ce$quadid,lat=t2ce$L=lat,lon=t2ce$lon,square=t2ce$squaretypecode) # simplify data.frame
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

t2ce<- read.table('/home/doug/effdis/data/t2ce.csv',header=T,sep=',') # Read in from cloud server.



sqlQuery(chan,'drop table t2ce') # upload the data. Takes ages so only need to do once.
#t2ce<- t2ce[,-1]
#for(i in 19:55) {t2ce[,i]<-as.numeric(t2ce[,i])}
#t2ce$fleetcode <- 
sqlSave(chan,t2ce[1,],tablename='t2ce')
#sqlQuery(chan,'drop table t2ce_long_format')
#sqlSave(chan,task2.lf[1,],tablename='t2ce_long_format')
sqlSave(chan,mean_weights,tablename='mean_weights') # 
sqlSave(chan,fleet_ranks,tablename='fleet_ranks') # A worksheet
sqlSave(chan,t1det9sp,tablename='t1det9sp') # task1 data for raising
sqlSave(chan,flags,tablename='codes_flags_fleets') 
psql -d effdis -U postgres -c "\COPY public.t2ce_long_format FROM '/home/dbeare/effdis/data/task2.lf.csv' WITH delimiter ',' CSV HEADER NULL AS 'NA';"
psql -d effdis -h localhost -U postgres -c "\COPY public.t2ce FROM '/home/doug/effdis/data/t2ce.csv' WITH delimiter ',' CSV HEADER NULL AS 'NA';"


# Load codes #

sqlSave(chan,codes_effort_types,tablename='codes_effort_types')
sqlSave(chan,codes_species,tablename='codes_species')
sqlSave(chan,codes_sampling_areas,tablename='codes_sampling_areas')
sqlSave(chan,codes_square_types,tablename='codes_square_types')
sqlSave(chan,codes_time_periods,tablename='codes_time_periods')

# Get the data from the database to check #

t2ce <- sqlQuery(chan, "SELECT * FROM t2ce LIMIT 5;") # Extract first five rows
#t2ce$trend <- trend.r(year=t2ce$yearc,month=t2ce$timeperiodid,start.year=1950)


# Add comments #
# 
sqlQuery(chan, "COMMENT ON TABLE t2ce IS 'These are ICCAT task 2 effort data supplied by Carlos Palma carlos.palma@iccat.int';");
sqlQuery (chan,"COMMENT ON COLUMN t2ce.yearc IS 'Year';\n");
sqlQuery(chan,"COMMENT ON COLUMN t2ce.lon IS 'see table codes_square_types ';\n");
sqlQuery(chan,"COMMENT ON COLUMN t2ce.lat IS 'see table codes_square_types';\n");
sqlQuery(chan,"COMMENT ON COLUMN t2ce.longitude IS 'Center longitude of grid cell calculated according to latLon function of Kell';\n");
sqlQuery(chan,"COMMENT ON COLUMN t2ce.latitude  IS 'Center latitude of grid cell calculated according to latLon function of Kell';\n");
sqlQuery(chan,"COMMENT ON COLUMN t2ce.timeperiodid IS '1-12 is month, 13-16 is quarter, 17 is year, 18-19 are first and second semester';\n");

sqlQuery(chan, "COMMENT ON TABLE t1det9sp IS 'These are ICCAT Task 1 landings data for raising the Task 2 data. Supplied by Carlos Palma carlos.palma@iccat.int';");
sqlQuery(chan, "COMMENT ON TABLE t2ce_long_format IS 'These are ICCAT task 2 effort data supplied by Carlos Palma carlos.palma@iccat.int in the long format. They are the same data as in t2ce';");


# Add geo point #

sqlQuery(chan,"ALTER TABLE public.t2ce ADD COLUMN the_point geometry(Point,4326);\n");
sqlQuery(chan,"UPDATE public.t2ce SET the_point = ST_SETSRID(ST_MAKEPOINT(longitude,latitude),4326);\n"); 

sqlQuery(chan,"ALTER TABLE public.t2ce_long_format ADD COLUMN the_point geometry(Point,4326);\n");
sqlQuery(chan,"UPDATE public.t2ce_long_format SET the_point = ST_SETSRID(ST_MAKEPOINT(longitude,latitude),4326);\n"); 

# # Add index for geopoint
# 
# sqlQuery (chan,"CREATE INDEX t2ce_the_point ON t2ce USING GIST (the_point);\n");
sqlQuery (chan,"CREATE INDEX t2ce_long_format_the_point ON t2ce_long_format USING GIST (the_point);\n");

# 
# # Create separate smaller table with unique locations to add depths to. #
# 
# sqlQuery (chan, "SELECT DISTINCT longitude,latitude, the_point INTO t2ce_distinct_locations from t2ce;")
# sqlQuery(chan, "COMMENT ON TABLE t2ce_distinct_locations IS 'These are unique or distinct ICCAT task 2 effort data locations derived from table t2ce. It was made to facilitate easier matching to other potentially useful covariates such as depth';");
# 

sqlQuery (chan, "SELECT * INTO t2ce_ll from t2ce where geargrpcode ='LL';")
sqlQuery(chan, "COMMENT ON TABLE t2ce_ll IS 'These ICCAT task 2 effort data locations derived from table t2ce for longliners only';");

sqlQuery (chan, "SELECT * INTO t2ce_ps from t2ce where geargrpcode ='PS';")
sqlQuery(chan, "COMMENT ON TABLE t2ce_ps IS 'These ICCAT task 2 effort data locations derived from table t2ce for purse-seiners only';");

sqlQuery (chan, "SELECT * INTO t2ce_bb from t2ce where geargrpcode ='BB';")
sqlQuery(chan, "COMMENT ON TABLE t2ce_bb IS 'These ICCAT task 2 effort data locations derived from table t2ce for bait-boats only';");

sqlQuery (chan, "SELECT * INTO t2ce_long_format_ll from t2ce_long_format where geargrpcode ='LL';")
sqlQuery(chan, "COMMENT ON TABLE t2ce_long_format_ll IS 'These ICCAT task 2 effort data locations derived from table t2ce_long_format for longliners only';");

sqlQuery (chan, "SELECT * INTO t2ce_long_format_ps from t2ce_long_format where geargrpcode ='PS';")
sqlQuery(chan, "COMMENT ON TABLE t2ce_long_format_ps IS 'These ICCAT task 2 effort data locations derived from table t2ce_long_format for purse-seiners only';");

sqlQuery (chan, "SELECT * INTO t2ce_long_format_bb from t2ce_long_format where geargrpcode ='BB';")
sqlQuery(chan, "COMMENT ON TABLE t2ce_long_format_bb IS 'These ICCAT task 2 effort data locations derived from table t2ce_long_format for bait-boats only';");





# 


 odbcClose(chan)
# 













