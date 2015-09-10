convert2long.format.t2.r<-function(input=t2ce)
{ 
  
# Convert standard task2 input data to long format using output of get.effdis.data.r and prepare.effdis.data.r
  
tdata <- ps

#Just use relevant columns

task2.simple <- data.frame(year=tdata$year,trend=tdata$trend,month=tdata$month,region = tdata$region, 
                           flagname=tdata$flagname,fleetcode=tdata$fleetcode,geargrpcode=tdata$geargrpcode,
                           longitude=tdata$longitude,latitude=tdata$latitude,
                           eff1=tdata$eff1,eff1type=tdata$eff1type,dsettype=tdata$dsettype,
                           catchunit=tdata$catchunit,
                           alb=tdata$alb,bft=tdata$bft,
                           bet=tdata$bet,skj=tdata$skj,yft=tdata$yft,swo=tdata$swo,bum=tdata$bum,sai=tdata$sai,whm=tdata$whm,tot9sp=tdata$totsp9)

print(dim(task2.simple))

# remember to install reshape

task2.lf <- melt(task2.simple[,-23],id=c('year','trend','month','region','flagname','fleetcode','geargrpcode','longitude','latitude','eff1','eff1type','dsettype','catchunit'))
dimnames(task2.lf)[[2]][14:15] <- c('species','measured_catch')

print(dim(task2.lf))

task2.lf

}

ps <- get.effdis.t2.data.r(which.gear='LL',which.flag='All',which.effort='NO.HOOKS')
#ps<-prepare.effdis.data.r(input=ps)
#pslf <- convert2long.format.t2.r(input =ps)
