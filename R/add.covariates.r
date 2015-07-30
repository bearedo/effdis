
add.covariates.r <- function(input = t2ce_lf_ll,what.dsn = 'effdis-tuna-cc1')
{
  
  # Function adds depth, sst, chlorophyll a, ph and primary production estimates to the data which I thought might be handy.
  
  #   depth_m - ocean depths from the SRTM30 Plus bathymetry dataset downloaded from MARSPEC (http://www.marspec.org/Modern_Data.html)
  #   m_ann_sst - mean annual sst (2009-2013) from UNEP-WCMC (http://data.unep-wcmc.org/datasets/36)
  #   m_ann_chla - mean annual chlorophyll-a concentration (2009-2013) from UNEP-WCMC (http://data.unep-wcmc.org/datasets/37)
  #   aq_primpro - Aquamaps HCAF v4 primary productivity from SeaWiFS data from GMED (http://gmed.auckland.ac.nz/data.html#)
  #   bo_ph - pH data from WOD 2009 from GMED (http://gmed.auckland.ac.nz/data.html#)
  
  chan <- odbcConnect(what.dsn, case="postgresql", believeNRows=FALSE)                                                                                                                
  t2ce_distinct_locations_covariates <- sqlQuery(chan, "SELECT *, ST_AsText(the_geom_4326) AS the_point from t2ce_distinct_locations_covariates;") 
  
  # m <- dbDriver("PostgreSQL")
  # con <- dbConnect(m, dbname="effdis", host = "134.213.29.249", user = "postgres")
  # q <- "SELECT *, ST_AsText(the_geom_4326) AS geom from t2ce_distinct_locations_covariates;"
  # rs <-  dbSendQuery(con,q)
  # df <-  fetch(rs,n=-1)
  
  tdlc     <- SpatialPointsDataFrame(cbind(x=an(ac(t2ce_distinct_locations_covariates$longitude)),y=an(ac(t2ce_distinct_locations_covariates$lat))),data=t2ce_distinct_locations_covariates)
  geogWGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # Make sure proj is what we think it is.
  tdlc@proj4string <- geogWGS84
  
  # Put covariates on
  
  input$depth_m <- tdlc@data$depth_m[match(paste(input$longitude,input$latitude), paste(tdlc@data$longitude,tdlc@data$latitude))]
  input$m_ann_sst <- tdlc@data$m_ann_sst[match(paste(input$longitude,input$latitude), paste(tdlc@data$longitude,tdlc@data$latitude))]
  input$aq_prim_pro <- tdlc@data$aq_prim_pro[match(paste(input$longitude,input$latitude), paste(tdlc@data$longitude,tdlc@data$latitude))]
  input$m_ann_chla <- tdlc@data$m_ann_chla[match(paste(input$longitude,input$latitude), paste(tdlc@data$longitude,tdlc@data$latitude))]
  
  out <- list(output=input)
  out}