get.effdis.t1.data <- function(which.dsn="effdis-tuna-cc1",which.gear='LL',which.region='AT',which.flag='All',
                                 which.datatype='C')
{

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
FROM t1det9sp_new
WHERE region =","'",which.region,"'","AND geargrp=","'",which.gear,"'",
                  "AND datatype=","'",which.datatype,"'")
  }

else{
query <- paste0("SELECT * 

FROM t1det9sp_new
WHERE region =","'",which.region,"'","AND geargrp=","'",which.gear,"'",
                "AND flag=","'",which.flag,"'","AND datatype=","'",which.datatype,"'")
}

  out <- sqlQuery(chan,query)
  
  odbcClose(chan)
  
  out
}


#ps <- get.effdis.t2.data.r(which.gear='LL',which.flag='EU.Portugal')
#ps <- prepare.effdis.data.r(input=ps)
#ps <- add.covariates.r()
  
  
