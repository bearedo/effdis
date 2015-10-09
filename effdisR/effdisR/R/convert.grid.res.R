
convert.grid.res <- function(input=ll1)
{

#Function to convert/simplify 1x1 grid to 5x5
#input <- ll1
res1 <- input[input$squaretypecode == '1x1',]
res5 <- input[input$squaretypecode != '1x1',]

input.spdf  <- SpatialPointsDataFrame(cbind(x=as.numeric(as.character(input$longitude)),y=as.numeric(as.character(input$latitude))),data=input[,c(9,10)])
coords1      <- SpatialPointsDataFrame(cbind(x=as.numeric(as.character(res1$longitude)),y=as.numeric(as.character(res1$latitude))),data=res1[,c(4,5)])

geogWGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # Make sure proj is what we think it is.

coords1@proj4string <- geogWGS84
input.spdf@proj4string <- geogWGS84
#plot(coords)

#- Define grid cell area
resx        <- 5
resy        <- 5

#-Obtain outer region of all the data.

bbox        <- bbox(input.spdf)
spatBound   <- list(xrange = c(floor(range(bbox["x",])[1]),ceiling(range(bbox["x",])[2])),
                    yrange = c(floor(range(bbox["y",])[1]),ceiling(range(bbox["y",])[2])))
grd         <- createGrid(spatBound$x,spatBound$y,resx,resy,type="SpatialGridDataFrame",exactBorder=T)

grd@proj4string <- geogWGS84

#- Reset values
grd@data[] <- 0

idx                           <- over(as(coords1,"SpatialPoints"),as(grd,"SpatialGrid"))

coordGrd  <- coordinates(grd)

#Convert longitudes and latitudes

res1$gridID                 <- idx
res1$longitude <- coordGrd[,1][res1$gridID]
res1$latitude  <- coordGrd[,2][res1$gridID]

# Get rid ofgridID
res1<-res1[,-25]
# Change squaretypecode
res1$squaretypecode <- ifelse(res1$squaretypecode=='1x1','5x5',res1$squaretypecode)


out<-rbind(res1,res5)
out

}