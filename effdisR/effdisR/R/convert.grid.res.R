
convert.grid.res <- function(input=ll1)
{

#Function to convert/simplify 1x1 grid to 5x5
#input <- ll1
res1 <- input[input$squaretypecode == '1x1',]
res5 <- input[input$squaretypecode == '5x5',]

res10   <- input[input$squaretypecode == '10x10',]

res10$longitude   <- res10$longitude - 2.5
res10$latitude    <- res10$latitude - 2.5
res10$squaretypecode <- '5x5'

#res1020 <- ll1[ll1$squaretypecode == '10x20',]
#res1020$longitude   <- res1020$longitude + 2.5
#res1020$latitude    <- res1020$latitude + 2.5
#res1020$squaretypecode <- '5x5'

res0510 <- input[input$squaretypecode == '5x10',]
res0510$longitude <- res0510$longitude + 2.5
res0510$squaretypecode <- '5x5' 

# res2020 <- ll1[ll1$squaretypecode == '20x20',]
# res2020$longitude   <- res2020$longitude + 2.5
# res2020$latitude    <- res2020$latitude + 2.5
# res2020$squaretypecode <- '5x5'


coords5  <- SpatialPointsDataFrame(cbind(x=as.numeric(as.character(res5$longitude)),y=as.numeric(as.character(res5$latitude))),data=res5[,c(4,5)])
coords1  <- SpatialPointsDataFrame(cbind(x=as.numeric(as.character(res1$longitude)),y=as.numeric(as.character(res1$latitude))),data=res1[,c(4,5)])

geogWGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # Make sure proj is what we think it is.

coords1@proj4string <- geogWGS84
coords5@proj4string <- geogWGS84
#plot(coords)

#- Define grid cell area
resx        <- 5
resy        <- 5

#-Obtain outer region of all the data.

bbox        <- bbox(coords5)

spatBound<- list(xrange = c(bbox["x",][1]-5,bbox["x",][2]+5),
                 yrange = c(bbox["y",][1]-5,bbox["y",][2]+5))


grd         <- createGrid(spatBound$x,spatBound$y,resx,resy,type="SpatialGridDataFrame",exactBorder=T)

grd@proj4string <- geogWGS84

#- Reset values
grd@data[] <- 0

idx                           <- over(as(coords1,"SpatialPoints"),as(grd,"SpatialGrid"))

coordGrd  <- coordinates(grd)
coordGrd[,1] <- coordGrd[,1]+2.5
coordGrd[,2] <- coordGrd[,2]+2.5

# points(coordGrd,col="blue")
# points(res5$longitude,res5$latitude,col="red")

#Convert longitudes and latitudes

res1$gridID                 <- idx
res1$longitude <- coordGrd[,1][res1$gridID]
res1$latitude  <- coordGrd[,2][res1$gridID]

# Get rid ofgridID
res1<-res1[,-25]
# Change squaretypecode
res1$squaretypecode <- ifelse(res1$squaretypecode=='1x1','5x5',res1$squaretypecode)
# 
# points(res1$longitude,res1$latitude,col="green")

out<-rbind(res1,res5,res10,res0510)
out

}