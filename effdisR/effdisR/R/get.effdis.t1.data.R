get.effdis.t1.data <-
function(which.dsn="effdis-tuna-cc1",which.gear='LL',which.region='AT',which.flag='All')
{

  #which.dsn<-"effdis-tuna-cc1"
  
  #which.gear <- "LL"
  #which.region <- "AT"
  #which.flag <- "EU.Portugal"
 # which.effort <- "NO.HOOKS"
 #which.datatype <- 'C'
  
  # Function to extract task 2 data from effdis cloud server ()
  
  chan <- odbcConnect(which.dsn, case="postgresql", believeNRows=FALSE) # Connect to database
  
if(which.flag=='All') # If which.flag is 'All' extract all data
  {
  query <- paste0("SELECT *
FROM t1det_2019
WHERE region =","'",which.region,"'","AND geargrp=","'",which.gear,"'")
  }

else{
query <- paste0("SELECT * 

FROM t1det_2019
WHERE region =","'",which.region,"'","AND geargrp=","'",which.gear,"'",
                "AND flag=","'",which.flag,"'")
}

  out <- sqlQuery(chan,query)
  
  odbcClose(chan)
  
  out
}
