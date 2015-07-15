### R-script to process effdis data and export ###

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
library(gam)
library(maps)
library(mapdata)
library(COZIGAM)

library(RODBC)
# postgres server running locally (in gedt /etc/odbc.ini)
chan <- odbcConnect("effdis-local", case="postgresql", believeNRows=FALSE)
sqlTables(chan)  #List all tables in the DB
#mydata <- sqlFetch(chan, "some_table") # Return a table as a dataframe
odbcClose(chan)


#Connect to postgres server at tuna-cc1. NB. you must edit etc/odbc.ini file.
chan <- odbcConnect("effdis-tuna-cc1", case="postgresql", believeNRows=FALSE)
sqlTables(chan)  #List all tables in the DB
t2ce <- sqlFetch(chan, "t2ce") # Return a table as a dataframe
dimnames(t2ce)[[2]][55:56] <- c('longitude','latitude')
#sqlQuery(chan,'drop table t2ce')
#sqlSave(chan,t2ce,tablename='t2ce')


#Add comments
sqlQuery(chan, "COMMENT ON TABLE t2ce IS 'These are ICCAT task 2 effort data supplied by Carlos Palma carlos.palma@iccat.int';");
sqlQuery (chan,"COMMENT ON COLUMN t2ce.yearc IS 'Year';\n");
sqlQuery(chan,"COMMENT ON COLUMN t2ce.lon IS 'see table codes_square_types ';\n");
sqlQuery(chan,"COMMENT ON COLUMN t2ce.lat IS 'see table codes_square_types';\n");
sqlQuery(chan,"COMMENT ON COLUMN t2ce.longitude IS 'Center longitude of grid cell calculated according to latLon function of Kell';\n");
sqlQuery(chan,"COMMENT ON COLUMN t2ce.latitude  IS 'Center latitude of grid cell calculated according to latLon function of Kell';\n");
sqlQuery(chan,"COMMENT ON COLUMN t2ce.timeperiodid IS '1-12 is month, 13-16 is quarter, 17 is year, 18-19 are first and second semester';\n");

#Add geo point

sqlQuery(chan,"ALTER TABLE public.t2ce ADD COLUMN the_point geometry(Point,4326);\n");
sqlQuery(chan,"UPDATE public.t2ce SET the_point = ST_SETSRID(ST_MAKEPOINT(longitude,latitude),4326);\n"); 

#Add index for geopoint

sqlQuery (chan,"CREATE INDEX t2ce_the_point ON t2ce USING GIST (the_point);\n");
odbcClose(chan)

#### Read in data for 2011 ###

# Set working directory #

setwd('/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/effdis_2011/input')

#source("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Scripts/utilityDB.R")
source("/home/doug/effdis/R/utilityDB.R")
source("/home/doug/effdis/R/trend.r")

# List files in directory #

list.files()

mean_weights <- import("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/effdis_2011/input/MeanWeights2011.xlsx")

#fleet_ranks <- import("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/effdis_2011/input/revisedFleetRanks.xlsx")

t1det9sp <- import("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/effdis_2011/input/t1det_9sp.xlsx")
colnames(t1det9sp) <- tolower(colnames(t1det9sp))

t2ceLL <- import("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/effdis_2011/input/t2ce_LL_raw5x5.xlsx")

t2ce  <- read.table("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/effdis_2011/input/t2ce.csv",sep=",",header=T)
colnames(t2ce) <- tolower(colnames(t2ce))
t2ce$trend <- trend.r(year=t2ce$yearc,month=t2ce$timeperiodid,start.year=1950)

#table.csv  <- read.table("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/effdis_2011/input/table.csv",sep=",",header=T)

flags <- import("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/effdis_2011/input/flags.csv")

#codes_flag_fleets <- import("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/CODES_Flags-Fleets.xls")


codes_effort_types <- import("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/CODES_EffortTypes.xls")
codes_effort_types <- codes_effort_types[-c(1,2),]
dimnames(codes_effort_types)[[2]]<-c('EffortTypeID','EffortTypeCode','EffortTypeName')

codes_species <- import("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/CODES_Species.xlsx")
codes_sampling_areas <- import("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/CODES_SamplingAreas.xls")
codes_square_types <- import("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/CODES_SquareTypes.xls")
codes_time_periods <- import("/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/CODES_TimePeriods.xls")


              
t2ce$FleetCode <- flags$FleetCode[match(t2ce$FleetID,flags$FleetID)]
t2ce$FlagName <- flags$FlagName[match(t2ce$FleetCode,flags$FleetCode)] 


dim(t2ce[t2ce$GearGrpCode == 'LL' & t2ce$SquareTypeCode %in% c('5x5'),])

dim(t2ce)

# First five rows of Task 2 data

head(t2ceLL)
head(t2ce)

#How many regions

table(t2ceLL$Region)

#AT    MD 
#99488  2026 

table(t2ce$Region) # no region in the database

# How many years ?

ys <- table(t2ceLL$YearC)

summary(t2ceLL$YearC)

ys <- table(t2ce$YearC)

summary(t2ce$YearC)

#Remove the missing value

t2ceLL <- t2ceLL[!is.na(t2ceLL$YearC),]


#How many Flag states ?

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

t2<-sort(table(t2ce$FleetCode)) # not available in the Access Db.

t1<-sort(table(t1det9sp$Fleet))


u2<-unique(t2ce$FleetCode) # NB. Not available in the Access Db.

u1<-unique(t1det9sp$Fleet) 


match(u1,u2) # Match fleet codes ? 

# Some countries report numbers, some weights and some both

table(t2ceLL$DSetType)
# n-    nw    -w 
# 44290 21437 35787 

table(t2ce$DSetType)

#--     n-     nw     -w 
#     24 136211  85256 346010 


table(t2ceLL$CatchUnit)
#kg    nr 
#57224 44290

table(t2ce$CatchUnit)
 #grd     kg     nr 
 #122206 270693 174602 


table(t2ceLL$CatchUnit,t2ceLL$FlagName)

# Belize Brasil China P.R. Chinese Taipei  Cuba EU.Cyprus EU.España EU.Greece EU.Italy EU.Malta EU.Portugal Japan Korea Rep. Maroc
# kg    238   8081       1067          21908    41       157      9210       262       77       44         600     0       6499    65
# nr      0    813          0              0  2247         0         0         0        0        0           0 34298        194     0
# 
# Mexico Namibia Panama Philippines South Africa St. Vincent and Grenadines Trinidad and Tobago Uruguay U.S.A. U.S.S.R. Vanuatu
# kg    442     727   2763         338          824                        874                  60     896      0       53     201
# nr      0       0      0           0            0                          0                   0       0   6550        0       0
# 
# Venezuela
# kg      1797
# nr       188

######################################################
##### Year month coverage in the Atlantic ############
######################################################



#######################################################

source("/home/doug/effdis/R/yr.month.coverage.task2.r")



yr.month.coverage.task2.r()
yr.month.coverage.task2.r(tdata=t2ce,which.flag='Belize')
yr.month.coverage.task2.r(tdata=t2ce,which.gear='LL',which.flag='China P.R.')
yr.month.coverage.task2.r(which.flag='Chinese Taipei')
yr.month.coverage.task2.r(which.flag='Japan')
yr.month.coverage.task2.r(which.flag='U.S.A.')

# Note - data coverage for Chinese Taipei has increased markely with what appear to be steps in around 1974 and 1997.

# Add trend column (useful for time-series analysis)

t2ceLL$trend <- trend.r(year=t2ceLL$YearC,month=t2ceLL$TimePeriodID,start.year=1950)
t2ce$trend <- trend.r(year=t2ce$YearC,month=t2ce$TimePeriodID,start.year=1950)


### convert spatial information to grid centroids with Laurie's code ##

# Note: apparently even though it says 1x1 in the original data they have actually been converted to 5x5.

table(t2ceLL$SquareTypeCode)

#1x1   5x5 
# 11874 89640 

table(t2ce$SquareTypeCode)

#0x10  10x20    1x1  20x20   5x10    5x5   none 
#1235     39 403016    250    753 161986    222 

df <- data.frame(quad=t2ceLL$QuadID,lat=t2ceLL$Lat5,lon=t2ceLL$Lon5,square=ac(t2ceLL$SquareTypeCode))
df1<- data.frame(quad=rep(NA,length(df[,1])),lat=rep(NA,length(df[,1])),lon=rep(NA,length(df[,1])),square=rep(NA,length(df[,1])))
for(i in 1:length(df[,1]))
{
  df1[i,] <- latLon(x=df[i,])
}
t2ceLL$lon <- df1$lon
t2ceLL$lat <- df1$lat
write.table(t2ceLL,'/home/doug/effdis/data/t2ceLL.csv',sep=',')
t2ceLL <- read.table('/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/t2ceLL.csv',sep=',')


df <- data.frame(quad=t2ce$QuadID,lat=t2ce$Lat,lon=t2ce$Lon,square=t2ce$SquareTypeCode)
df$square <- as.character(df$square)

#Checking

# 
str(df)
hl <- length(df[,1])
ndf <- data.frame(quad=rep(NA,hl),lat=rep(NA,hl),lon=rep(NA,hl),square=rep(NA,hl))
for(i in 1:hl){
ndf[i,] <- latLon(df[i,])
}

plot(ndf$lon,ndf$lat,pch='.')
map('world',add=T)
#   

t2ce$lon <- ndf$lon
t2ce$lat <- ndf$lat

dim(t2ce)

#write.table(t2ce,'/home/doug/effdis/data/t2ce.csv',sep=',')
write.table(t2ce,'/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/effdis_2011/input/t2ce.csv',sep=',')


t2ce <- read.table('/home/doug/Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/effdis_2011/input/t2ce.csv',sep=',')
dimnames(t2ce)[[2]][55:56] <- c('longitude','latitude') # Have to be different.


#####################################
#Task 1 #############################
######################################

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

xyplot(log(Qty_t)~YearC|Species,data=t1ct1)


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

# Chinese Taipei in Atlantic Task 2#

ct <- t2ceLL[t2ceLL$FlagName == 'Chinese Taipei'& t2ceLL$Region == 'AT',]

## Sampling in space ##

## By year ##

source("/home/doug/effdis/R/spatial.coverage.by.year.task2.r")

spatial.coverage.by.year.task2.r()
spatial.coverage.by.year.task2.r(which.flag='Belize')
spatial.coverage.by.year.task2.r(which.flag='China P.R.')
spatial.coverage.by.year.task2.r(which.flag='Chinese Taipei')
spatial.coverage.by.year.task2.r(which.flag='Japan')
spatial.coverage.by.year.task2.r(which.flag='U.S.A.')

#By month

source("/home/doug/effdis/R/spatial.coverage.by.month.task2.r")


spatial.coverage.by.month.task2.r()
spatial.coverage.by.month.task2.r(which.flag='Belize')
spatial.coverage.by.month.task2.r(which.flag='China P.R.')
spatial.coverage.by.month.task2.r(which.flag='Chinese Taipei')
spatial.coverage.by.month.task2.r(which.flag='Japan')
spatial.coverage.by.month.task2.r(which.flag='U.S.A.')


source("/home/doug/effdis/R/effort.by.year.task2.r")
par(mfrow=c(2,1))
effort.by.year.task2.r(which.flag='Chinese Taipei')
effort.by.year.task2.r(which.flag='Japan')


#Sum by year and month

ct1 <- aggregate(Eff1~trend+TimePeriodID,data=ct,sum)
ct1 <- orderBy(~trend+TimePeriodID,data=ct1)


plot(ct1$trend,log(ct1$Eff1),type='l',xaxt='n',ylab='Number of hooks',xlab="year")
lines(supsmu(ct1$trend,log(ct1$Eff1)          ),col='green')
xl <- seq(min(ct$YearC),max(ct$Year),by=5)
axis(1,at=seq(min(ct$trend),max(ct$trend),by=60),labels=as.character(xl))
abline(v=seq(min(ct$trend),max(ct$trend),by=60),lty=2,col='blue')


#Have a look at the relationship between n hooks and species weights caught

par(mfrow=c(3,4),mar=c(1,1,3,1))
for(i in 19:27){
plot(log(ct[,i]),log(ct$Eff1),pch='.')
title(colnames(ct)[i])
}
plot(log(ct$Total),log(ct$Eff1),pch='.')
title('Total')

# Multivariate relationships in the Task 2 data #

tdata <- t2ceLL

task2.simple <- data.frame(year=tdata$YearC,trend=tdata$trend,month=tdata$TimePeriodID,region = tdata$Region, flagname=tdata$FlagName, 
                  fleetcode=tdata$FleetCode,lon=tdata$lon,lat=tdata$lat,hooks=tdata$Eff1,dsettype=tdata$DSetType,catchunit=tdata$CatchUnit,
                  ALB=tdata$ALB,BFT=tdata$BFT,
             BET=tdata$BET,SKJ=tdata$SKJ,YFT=tdata$YFT,SWO=tdata$SWO,BUM=tdata$BUM,SAI=tdata$SAI,WHM=tdata$WHM,Total=tdata$Total)

#pairs(ct2,pch='.')

dim(task2.simple)

task2.simple[task2.simple$flagname == 'Chinese Taipei' & task2.simple$year == 1990 & task2.simple$month == 1,]

cc <- round(cor(task2.simple[,-c(4,5,6,9,10,11)]),2)

par(mfrow=c(1,1))
image(cc)

# Merge Task1 and Task2

# Simplify t1ct
# Take out longlines

task1 <- t1det9sp[t1det9sp$GearGrp == 'LL',]
task1.simple <- data.frame(year=task1$YearC,region=task1$Region,area=task1$Area,flagname=task1$Flag,fleetcode=task1$Fleet,
                    species=task1$Species,total_catch_kgs=task1$Qty_t*1000)

# Sum over region, year, species fleetand flag

task1.sum <- aggregate(total_catch_kgs~region+year+flagname+fleetcode+species,data=task1.simple,sum)

# Convert task2 to long-format

library(reshape2)

task2.lf <- melt(task2.simple[,-21],id=c('year','trend','month','region','flagname','fleetcode','lon','lat','hooks','dsettype','catchunit'))
dimnames(task2.lf)[[2]][12:13] <- c('species','measured_catch')

dim(task2.lf) #= 913626

task2.lf [task2.lf$species == 'ALB' & task2.lf$flagname == 'Chinese Taipei' & task2.lf$year == 1990 & task2.lf$month == 1,]
task2.lf [task2.lf$flagname == 'EU.Greece' & task2.lf$month ==2 & task2.lf$year == 2000,]



# Now merge task 1 and 2
#long format
task2.lf$total_catch_kgs <- 
  task1.sum$total_catch_kgs[match(paste(task2.lf$year,task2.lf$region,task2.lf$flagname,
                                        task2.lf$fleetcode,task2.lf$species),
                                        paste(task1.sum$year,task1.sum$region,task1.sum$flagname,
                                              task1.sum$fleetcode,task1.sum$species))]


task1.2.lf <- task2.lf

sum(is.na(task1.2.lf$total_catch_kgs)) # 148966

nas <- rep(0,length(task1.2.lf[,1]))
nas[!is.na(task1.2.lf$total_catch_kgs)]<- 1
table(nas)
task1.2.lf <- task1.2.lf[!is.na(task1.2.lf$total_catch_kgs),]
#task1.2.lf$trend <- trend.r(year=task1.2.lf$year,month=task1.2.lf$month,start.year=1956)
task1.2.lf <- orderBy(~trend+species+flagname,data=task1.2.lf)
task1.2.lf$bin <- ifelse(task1.2.lf$measured_catch==0,0,1)

task2.sum <- aggregate(cbind(measured_catch,hooks)~year+species+region+flagname+fleetcode+catchunit,
                        data=task2.lf,sum,na.rm=T)

head(task2.sum)
dim(task2.sum) # = 6822
task2.sum <- orderBy(~year+species+flagname+catchunit,data=task2.sum)

task1.2 <-merge(task2.sum,task1.sum,all.x=T)

write.table(task1.2,'/home/doug/effdis/data/task1.2.csv',sep=',')

ctaipei <- task1.2.lf[task1.2.lf$flagname == 'Chinese Taipei' & task1.2.lf$region == 'AT',]
#ctaipei <- task1.2[task1.2$flagname == 'Chinese Taipei' & task1.2$region == 'AT',]

ctaipei <- ctaipei[ctaipei$catchunit== 'kg',]
par(mar=c(4,4,4,4))
plot(log(ctaipei$measured_catch),log(ctaipei$total_catch_kgs),xlab='task2',ylab='task1',ylim=c(0,24),xlim=c(0,24))
plot(ctaipei$measured_catch,ctaipei$total_catch_kgs,pch='.')
abline(0,1)

xyplot(total_catch_kgs ~ measured_catch|catchunit,data=ctaipei)
xyplot(log(total_catch_kgs) ~ log(measured_catch)|flagname,groups=catchunit,data=task1.2)

xyplot(log(total_catch_kgs) ~ log(measured_catch)|flagname,groups=catchunit,data=task1.2[task1.2$region == 'AT' ,]
       ,xlab='task2',ylab='task1',auto.key = T)

```{r relationship between task 1 and task 2 data, include=TRUE,echo=FALSE,message = FALSE, error=FALSE, warnings=FALSE}
task1.2 <- read.table("/home/doug/effdis/data/task1.2.csv",header=T)
library(lattice)
xyplot(log(total_catch_kgs) ~ log(measured_catch)|flagname,groups=catchunit,data=task1.2[task1.2$region == 'AT' ,]
       ,xlab='task2',ylab='task1',auto.key = T)
```
## Investigating the relationship between Task 1 and Task 2 data (Sum of products) for fleet combinations
task1.2 <- read.table("/home/doug/effdis/data/task1.2.csv",header=T,sep=',')

## Developing the most appropriate multi-variate models (e.g. GLMs, GAMs) for interpolation


xyplot(log(total_catch_kgs) ~ log(measured_catch)|flagname,groups=catchunit,data=task1.2[task1.2$region == 'AT' ,])
xyplot(log(total_catch_kgs) ~ log(measured_catch)|flagname,groups=catchunit,data=task1.2[task1.2$region == 'AT' ,])

xyplot(log(hooks) ~ log(measured_catch)|flagname,groups=catchunit,data=task1.2[task1.2$region == 'AT' ,])
xyplot(log(hooks) ~ log(measured_catch)|flagname,groups=catchunit,data=task1.2[task1.2$region == 'AT' ,])
xyplot(log(hooks) ~ log(measured_catch)|flagname,groups=catchunit,data=task1.2[task1.2$region == 'AT' ,])



xyplot(log(hooks) ~ log(measured_catch)|flagname,groups=species,data=task2.lf[task2.lf$region == 'AT' ,])
xyplot(log(hooks) ~ log(measured_catch)|flagname,groups=species,data=task2.lf[task2.lf$region == 'AT' ,])
xyplot(log(hooks) ~ log(measured_catch)|flagname,groups=species,data=task2.lf[task2.lf$region == 'AT' ,])





xyplot(log(hooks) ~ log(total_catch_kgs)|flagname,groups=species,data=task2.sum[task2.sum$region == 'AT' & task2.sum$dsettype == '-w',])




###Experiment with GAMs#####

library(gam)

## Taipei ##

write.table(task1.2.lf,"/home/doug/effdis/data/task1.2.lf.csv",sep=',')

task1.2.lf.tai <- task1.2.lf[task1.2.lf$flagname == 'Chinese Taipei' & task1.2.lf$catchunit == 'kg',]
summary(task1.2.lf.tai)
task1.2.lf.tai$cpue <- task1.2.lf.tai$measured_catch/task1.2.lf.tai$hooks

min.lat <- min(task1.2.lf.tai$lat)
max.lat <- max(task1.2.lf.tai$lat)
min.lon <- min(task1.2.lf.tai$lon)
max.lon <- max(task1.2.lf.tai$lon)
grid.res <- 5
t1 <- min(task1.2.lf.tai$trend)
t2 <- max(task1.2.lf.tai$trend)
lonnie <- seq(min.lon,max.lon,by=grid.res)
lattie <- seq(min.lat,max.lat,by=grid.res)
lo <- length(lonnie)
la <- length(lattie)

#z0<-gam(hooks~offset(measured_catch)+1,family=quasipoisson,data=task2.lf.tai)
#z1<-gam(hooks~offset(measured_catch)+lo(trend)+lo(month)+lo(lon)+lo(lat),family=quasipoisson,data=task2.lf.tai)
#summary(z1)

alb <- task1.2.lf.tai[task1.2.lf.tai$species == 'SWO',]

#Binomial model for probability of catch
b0 <- gam(bin~1,family=quasibinomial(link="logit"),data=alb)
b1 <- gam(bin~lo(trend)+lo(month)+lo(lon)+lo(lat),family=quasibinomial(link="logit"),data=alb)
#b2 <- gam(bin~lo(trend)+lo(month)+lo(lon)+lo(lat)*species,family=quasibinomial(link="logit"),data=alb)

grd <-expand.grid(lon=lonnie,lat=lattie)
grd$month <- 6
grd$trend <- 648
grd$species <- as.factor('ALB')
prob <- predict(b1,grd,type="response")
grd$prob <- as.vector(prob)
image(lonnie,lattie,matrix(grd$prob,length(lonnie),length(lattie)))
contour(lonnie,lattie,matrix(grd$prob,length(lonnie),length(lattie)),add=T)


#Gamma model to be used together with above. Given you caught something then how much?
zz0 <- gam(measured_catch~1,family=Gamma(link="log"),data=alb[alb$bin==1,])
zz1 <- gam(measured_catch~lo(trend)+lo(month)+lo(lon)+lo(lat),family=Gamma(link="log"),data=alb[alb$bin==1,])
#zz2 <- gam(measured_catch~lo(trend)+lo(month)+lo(lon)+lo(lat)*species,family=Gamma(link="log"),data=alb[alb$bin==1,])
#zz3 <- gam(measured_catch~lo(trend,month,lon,lat)*species,family=Gamma(link="log"),data=task2.lf.tai[task2.lf.tai$bin==1,])

plot(alb$trend,log(alb$measured_catch),pch='.')
lines(alb$trend[alb$bin==1],log(fitted(zz1)),col='red')


pcatch <- predict(zz1,grd,type="response")
grd$pcatch <- as.vector(pcatch)
image(lonnie,lattie,matrix(grd$pcatch,length(lonnie),length(lattie)))
contour(lonnie,lattie,matrix(grd$pcatch,length(lonnie),length(lattie)),add=T)


#Poisson model for nhooks
h0 <- gam(hooks~1,family=quasipoisson(link="log"),data=alb[alb$species == 'ALB',])
h1 <- gam(hooks~lo(trend)+lo(month)+lo(lon)+lo(lat),family=quasipoisson(link="log"),data=alb[alb$species == 'ALB',])

anova(h0,h1,test='Chi')

hks <- predict(h1,grd,type="response")
grd$hks<- as.vector(hks)
image(lonnie,lattie,matrix(grd$hks,lo,la))
contour(lonnie,lattie,matrix(grd$hks,lo,la),add=T)

grd$catch <- grd$prob*grd$pcatch
image(lonnie,lattie,matrix(grd$catch,lo,la))
contour(lonnie,lattie,matrix(grd$catch,lo,la),add=T)

grd$cpue <- grd$catch/grd$hks

image(lonnie,lattie,matrix(grd$cpue,lo,la))
contour(lonnie,lattie,matrix(grd$cpue,la,la),add=T)

# Task 1 ALB catches by Taipei December 2009

task1.sum[task1.sum$flagname == "Chinese Taipei" & task1.sum$species == 'ALB' & task1.sum$year == 2009,]
task2.sum[task2.sum$flagname == "Chinese Taipei" & task2.sum$species == 'ALB' & task2.sum$year == 2009,]

# Get rid of observations on land etc. #

grd.spdf      <- SpatialPointsDataFrame(cbind(x=an(ac(grd$lon)),y=an(ac(grd$lat))),data=grd)
data.spdf     <- SpatialPointsDataFrame(cbind(x=an(ac(alb$lon)),y=an(ac(alb$lat))),data=alb)

geogWGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # Make sure proj is what we think it is.

grd.spdf@proj4string <- geogWGS84
data.spdf@proj4string <- geogWGS84

plot(grd.spdf,pch='.')
plot(data.spdf,add=T,col='red')
map('worldHires',add=T,fill=T,col='green')

#seas <- readOGR(dsn="/home/doug/effdis/data", layer="World_Seas") # World seas and oceans

geogWGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # Make sure proj is what we think it is.
seas@proj4string <- geogWGS84

class(seas)

seas.polys <- as.character(sort(unique(seas@data$NAME)))
seas.polys[1:10]

wo <- grep('Atl',seas.polys)
wi <- grep('Med',seas.polys)

seas.polys[wi]

atlantic <- seas[seas@data$NAME %in% seas.polys[wo],]
med <-      seas[seas@data$NAME %in% seas.polys[wi],]

plot(atlantic)
plot(med)

idx <- over(grd.spdf,atlantic)
at.sea      <- rep(NA,length(grd[,1]))


at.sea[which(is.na(idx[,1])==TRUE)] <- 'land'
at.sea[which(is.na(idx[,1])==FALSE)] <- 'sea'

grd$at.sea <- at.sea

grd$cpue[grd$at.sea == 'land'] <- NA
grd$prob[grd$at.sea == 'land'] <- NA
grd$catch[grd$at.sea == 'land'] <- NA
grd$hks[grd$at.sea == 'land'] <- NA
grd$pcatch[grd$at.sea == 'land'] <- NA

write.table(grd,file='/home/doug/effdis/R/grd')
grd <- read.table(file='/home/doug/effdis/R/grd')

par(mfrow=c(2,2))

image(lonnie,lattie,matrix(grd$hks,lo,la),col=topo.colors(100),xlab='',ylab='')
contour(lonnie,lattie,matrix(grd$hks,lo,la),col=topo.colors(100),add=T)
map('worldHires',add=T,fill=T);title('Number of hooks')


image(lonnie,lattie,matrix(grd$prob,lo,la),col=topo.colors(100),xlab='',ylab='')
contour(lonnie,lattie,matrix(grd$prob,lo,la),col=topo.colors(100),add=T)
map('worldHires',add=T,fill=T);title('Probability of catch (P)')


image(lonnie,lattie,matrix(grd$pcatch,lo,la),col=topo.colors(100),xlab='',ylab='')
contour(lonnie,lattie,matrix(grd$catch,lo,la),col=topo.colors(100),add=T)
map('worldHires',add=T,fill=T);title('Catch without zeros (C1)')


image(lonnie,lattie,matrix(grd$catch,lo,la),col=topo.colors(100),xlab='',ylab='')
contour(lonnie,lattie,matrix(grd$catch,lo,la),col=topo.colors(100),add=T)
map('worldHires',add=T,fill=T);title('Catch - P x C1')



image(lonnie,lattie,matrix(grd$catch,lo,la),col=topo.colors(100),xlab='',ylab='')
contour(lonnie,lattie,matrix(grd$catch,lo,la),col=topo.colors(100),add=T)
map('worldHires',add=T,fill=T)




sum(grd$catch,na.rm=T)


9541000/sum(grd$catch,na.rm=T)

# Albacore task1 in 2009
task1.sum[task1.sum$region == 'AT' & task1.sum$flagname == "Chinese Taipei" & task1.sum$species == 'SWO' & task1.sum$year == 2009,]
# Albacore task2 in 2009
task2.sum[task2.sum$region == 'AT' & task2.sum$flagname == "Chinese Taipei" & task2.sum$species == 'SWO' & task2.sum$year == 2009,]
# Albacore task2 December 2009
t2 <- task2.lf[task2.lf$flagname == "Chinese Taipei" & task2.lf$species == 'SWO' & task2.lf$year == 2009 & task2.lf$trend == 648,]
sum(t2$measured_catch)/1000
sum(grd$catch,na.rm=T)/1000


summary(h2)

z2 <- glm(hooks~1,family=quasipoisson,data=ct3)
z3 <- glm(hooks~measured_catch_kgs,family=quasipoisson,data=ct3)
z4 <- glm(hooks~measured_catch_kgs+total_catch_kgs,family=quasipoisson,data=ct3)

z2 <- glm.nb(hooks~1,data=ct3)
z3 <- glm.nb(hooks~measured_catch_kgs,data=ct3)
z4 <- glm.nb(hooks~measured_catch_kgs+total_catch_kgs,data=ct3)
anova(z2,z3,z4,test="Chi")






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


ocean.polys <- as.character(sort(unique(oceans@data$NAME)))
ocean.polys[1:10]

wo <- grep('ATLANTIC',ocean.polys)
w1 <- grep('MED',ocean.polys)

ocean.polys[w1]

atlantic <- oceans[oceans@data$NAME %in% ocean.polys[c(97,117)],]
med <- oceans[oceans@data$NAME %in% ocean.polys[c(92,93)],]

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













