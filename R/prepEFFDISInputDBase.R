### R-script to process effdis data and export them ###

## Load packages ##

library(rio)
library(RODBC)


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

## Load codes ## 

codes_effort_types <- import("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/CODES_EffortTypes.xls")
codes_species <- import("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/CODES_Species.xlsx")
codes_sampling_areas <- import("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/CODES_SamplingAreas.xls")
codes_square_types <- import("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/CODES_SquareTypes.xls")
codes_time_periods <- import("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/CODES_TimePeriods.xls")
codes_square_types<- import("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/CODES_SquareTypes.xls")

## Data preparation ##

# Add on fleet codes and flagnames

t2ce$FleetCode <- flags$FleetCode[match(t2ce$FleetID,flags$FleetID)]
t2ce$FlagName <- flags$FlagName[match(t2ce$FleetCode,flags$FleetCode)] 



# Create center point of each grid using Laurie's code lonLat #

df <- data.frame(quad=t2ce$QuadID,lat=t2ce$Lat,lon=t2ce$Lon,square=t2ce$SquareTypeCode) # simplify data.frame
df$square <- as.character(df$square) # function requires input to be 'character'

hl <- length(df[,1])
ndf <- data.frame(quad=rep(NA,hl),lat=rep(NA,hl),lon=rep(NA,hl),square=rep(NA,hl))
for(i in 1:hl){
  ndf[i,] <- latLon(df[i,]) # calculate the central lat long of each grid looping thro' each row
}


t2ce$lon <- ndf$lon # add on lat and long to main database
t2ce$lat <- ndf$lat

dimnames(t2ce)[[2]][55:56] <- c('longitude','latitude') # Have to be different.

## Connect to postgres database on ICCAT server with RODBC and upload the data NB. you must edit etc/odbc.ini file and install the correct drivers etc.

# Connect #

chan <- odbcConnect("effdis-tuna-cc1", case="postgresql", believeNRows=FALSE)
sqlTables(chan)  # List all tables in the DB

# Load data

#sqlQuery(chan,'drop table t2ce') # upload the data. Takes ages so only need to do once.
#sqlSave(chan,t2ce,tablename='t2ce')
sqlSave(chan,mean_weights,tablename='mean_weights') # 
#sqlSave(chan,fleet_ranks,tablename='fleet_ranks') # A worksheet

sqlSave(chan,t1det9sp,tablename='t1det9sp') # task1 data for raising
sqlSave(chan,flags,tablename='codes_flags_fleets') 


# Load codes #

sqlSave(chan,codes_effort_types,tablename='codes_effort_types')
sqlSave(chan,codes_species,tablename='codes_species')
sqlSave(chan,codes_sampling_areas,tablename='codes_sampling_areas')
sqlSave(chan,codes_square_types,tablename='codes_square_types')
sqlSave(chan,codes_time_periods,tablename='codes_time_periods')

# Get the data from the database to check #

t2ce <- sqlQuery(chan, "SELECT * FROM t2ce LIMIT 5;") # Extract first five rows


# Add comments #

sqlQuery(chan, "COMMENT ON TABLE t2ce IS 'These are ICCAT task 2 effort data supplied by Carlos Palma carlos.palma@iccat.int';");
sqlQuery (chan,"COMMENT ON COLUMN t2ce.yearc IS 'Year';\n");
sqlQuery(chan,"COMMENT ON COLUMN t2ce.lon IS 'see table codes_square_types ';\n");
sqlQuery(chan,"COMMENT ON COLUMN t2ce.lat IS 'see table codes_square_types';\n");
sqlQuery(chan,"COMMENT ON COLUMN t2ce.longitude IS 'Center longitude of grid cell calculated according to latLon function of Kell';\n");
sqlQuery(chan,"COMMENT ON COLUMN t2ce.latitude  IS 'Center latitude of grid cell calculated according to latLon function of Kell';\n");
sqlQuery(chan,"COMMENT ON COLUMN t2ce.timeperiodid IS '1-12 is month, 13-16 is quarter, 17 is year, 18-19 are first and second semester';\n");

# Add geo point #

sqlQuery(chan,"ALTER TABLE public.t2ce ADD COLUMN the_point geometry(Point,4326);\n");
sqlQuery(chan,"UPDATE public.t2ce SET the_point = ST_SETSRID(ST_MAKEPOINT(longitude,latitude),4326);\n"); 

# Add index for geopoint

sqlQuery (chan,"CREATE INDEX t2ce_the_point ON t2ce USING GIST (the_point);\n");
odbcClose(chan)














