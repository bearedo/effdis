get.effdis.t2.data.r <- function(what.dsn="effdis-tuna-cc1",which.gear='LL',which.region='AT',which.flag='All',
                                 which.effort='NO.HOOKS',which.dsettype='-w')
{

  #which.gear <- "LL"
  #which.region <- "AT"
  #which.flag <- "EU.Portugal"
  #which.effort <- "NO.HOOKS"
  #which.dsettype <- 'nw'
  
  # Function to extract task 2 data from effdis cloud server ()
  
  chan <- odbcConnect("effdis-tuna-cc1", case="postgresql", believeNRows=FALSE) # Connect to database
  
if(which.flag=='All') # If which.flag is 'All' extract all data
  {
  query <- paste0("SELECT yearc AS year, trend, timeperiodid AS month, flagname, fleetcode,region, 
geargrpcode,longitude,latitude, catchunit, dsettype, eff1, eff1type,
alb,bft,bet,skj,yft,swo,bum,sai,whm,totsp9
FROM t2ce
WHERE region =","'",which.region,"'","AND timeperiodid < 13 AND eff1type=","'",which.effort,"'",
                  "AND geargrpcode=","'",which.gear,"'","AND dsettype=","'",which.dsettype,"'")
  }

else{
query <- paste0("SELECT yearc AS year, trend, timeperiodid AS month, flagname, fleetcode,region, 
geargrpcode,longitude,latitude, catchunit, dsettype, eff1, eff1type,
alb,bft,bet,skj,yft,swo,bum,sai,whm,totsp9

FROM t2ce
WHERE region =","'",which.region,"'","AND timeperiodid < 13 AND eff1type=","'",which.effort,"'",
"AND geargrpcode=","'",which.gear,"'","AND flagname=","'",which.flag,"'","AND dsettype=","'",which.dsettype,"'")
}

  out <- sqlQuery(chan,query)
  
  out
  
}


#ps <- get.effdis.t2.data.r(which.gear='LL',which.flag='EU.Portugal')
#ps <- prepare.effdis.data.r(input=ps)
#ps <- add.covariates.r()
  
  
