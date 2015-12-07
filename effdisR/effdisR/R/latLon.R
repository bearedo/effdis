latLon<-function(x){
  
  x$lat<-x$lat + switch(x$square,
                        "1x1"    = 0.5,
                        "5x5"    = 2.5,
                        "10x20"  = 5.0,
                        "10x10"  = 5.0,
                        "5x10"   = 2.5,
                        "20x20"  = 10.0,
                        "LatLon" = 0.0,
                        "none"   = 0.0)
  
  x$lon<-x$lon + switch(x$square,
                        "1x1"    =  0.5,
                        "5x5"    =  2.5,
                        "10x20"  = 10.0,
                        "10x10"  =  5.0,
                        "5x10"   =  5.0,
                        "20x20"  = 10.0,
                        "LatLon" = 0.0,
                        "none"   =  0.0)
  
  x[x$quad %in% 3:4, "lon"]<--x[x$quad %in% 3:4, "lon"]
  x[x$quad %in% 2:3, "lat"]<--x[x$quad %in% 2:3, "lat"]
  
  return(x)}