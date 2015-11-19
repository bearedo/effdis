get.effdis.t2.data <-
function(which.dsn="effdis-tuna-cc1",which.gear='LL',which.region='AT',which.flag='All',
                                 which.dsettype='-w')
{

  #which.gear <- "LL"
  #which.region <- "AT"
  #which.flag <- "EU.Portugal"
  #which.effort <- "NO.HOOKS"
  #which.dsettype <- 'nw'
  
  # Function to extract task 2 data from effdis cloud server ()
  
  chan <- odbcConnect(which.dsn, case="postgresql", believeNRows=FALSE) # Connect to database
  
if(which.flag=='All') # If which.flag is 'All' extract all data
  {
  query <- paste0("SELECT yearc AS year, trend, timeperiodid AS month, flagname, fleetcode,region, squaretypecode,
geargrpcode,longitude,latitude, catchunit, dsettype, eff1, eff1type,
alb,bft,bet,skj,yft,swo,bum,sai,whm,totsp9
FROM t2ce_new
WHERE region =","'",which.region,"'","AND timeperiodid < 13 AND geargrpcode=","'",which.gear,"'",
                  "AND dsettype=","'",which.dsettype,"'")
  }

else{
query <- paste0("SELECT yearc AS year, trend, timeperiodid AS month, flagname, fleetcode,region,squaretypecode,
geargrpcode,longitude,latitude, catchunit, dsettype, eff1, eff1type,
alb,bft,bet,skj,yft,swo,bum,sai,whm,totsp9

FROM t2ce_new
WHERE region =","'",which.region,"'","AND timeperiodid < 13 AND geargrpcode=","'",which.gear,"'",
                "AND flagname=","'",which.flag,"'","AND dsettype=","'",which.dsettype,"'")
}

  out <- sqlQuery(chan,query)
  
  odbcClose(chan)
  
  out
}
