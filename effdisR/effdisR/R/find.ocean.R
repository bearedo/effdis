find.ocean <-
function(input=grd)
{
  
  # Function takes a data frame of locations (must be called latitude and longitude) and adds a vector telling you 
  # whether it is in Atlantic, Pacific or Med. The definitions of the 'Atlantic' etc are very general.
  #input <- grd
  #seas <- readOGR(dsn="/home/doug/effdis/data", layer="World_Seas") # World seas and oceans
  #data("seas")
  geogWGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # Make sure proj is what we think it is.
  seas@proj4string <- geogWGS84
  seas.polys <- as.character(sort(unique(seas@data$NAME)))
  
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
  
  # Create multi-polygons for selection.
  atlantic <- seas[seas@data$NAME %in% seas.polys[c(wo,wp,ws,wv,wmm,gog)],] # create object of Atlantic polygons
  med <-      seas[seas@data$NAME %in% seas.polys[c(wi,wj,wk,wl,wu,wz,wa,wzz,wxx)],]
  pacific   <- seas[seas@data$NAME %in% seas.polys[c(wpa)],]
  
  # Make SpatialPointsDataFrame
  
  input.spdf      <- SpatialPointsDataFrame(cbind(x=as.numeric(as.character(input$longitude)),y=as.numeric(as.character(input$latitude))),data=input)
  geogWGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # Make sure proj is what we think it is.
  input.spdf@proj4string <- geogWGS84
  
  # Work out which ocean the data are in
  idx.atl <- over(input.spdf,atlantic)
  idx.med <- over(input.spdf,med)
  idx.pac <- over(input.spdf,pacific)
  
  #print('found all points in polygons')
  
  # Add vector denoting which ocean
  
  which.ocean      <- rep(NA,length(input[,1]))
  which.ocean[which(!is.na(idx.atl[,1]))] <- 'atl'
  which.ocean[which(!is.na(idx.med[,1]))] <- 'med'
  which.ocean[which(!is.na(idx.pac[,1]))] <- 'pac'
  which.ocean[is.na(which.ocean)]         <-'land'
  
  
  input$which.ocean <- which.ocean
  
  input
  
}
