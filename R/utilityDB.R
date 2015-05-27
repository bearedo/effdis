ac    <-function(x) as.character(x)
a2df  <-function(x) data.frame(expand.grid(dimnames(x)),val=c(x))

reFac<-function(x){

  for (i in names(x))
      if (is(x[,i],"factor"))
         x[,i]<-factor(x[,i])

   return(x)}

latLon<-function(x){

  x$lat<-x$lat + switch(x$square,
                           "1x1"    = 0.5,
                           "5x5"    = 2.5,
                           "10x20"  = 5.0,
                           "10x10"  = 5.0,
                           "5x10"   = 2.5,
                           "20x20"  = 10.0,
                           "none"   = 0.0)
   
  x$lon<-x$lon + switch(x$square,
                           "1x1"    =  0.5,
                           "5x5"    =  2.5,
                           "10x20"  = 10.0,
                           "10x10"  =  5.0,
                           "5x10"   =  5.0,
                           "20x20"  = 10.0,
                           "none"   =  0.0)

   x[x$quad %in% 3:4, "lon"]<--x[x$quad %in% 3:4, "lon"]
   x[x$quad %in% 2:3, "lat"]<--x[x$quad %in% 2:3, "lat"]

   return(x)}


month<-function(x){
  x <-pmax(0,x)

  fn<-function(x) switch(ac(x),
                         "13"= 2,
                         "14"= 5,
                         "15"= 8,
                         "16"=11,
                         "17"= 6,
                         "18"= 3,
                         "19"= 9,
                         x)

  sapply(x,fn)}

stdz  <-function(x) ((x-mean(x))/var(x))
minMax<-function(x) (x-min(x))/diff(range(x))
#t1    <-ddply(ctE,c("month","year"),transform,y=stdz(cpue))
#t2    <-ddply(ctE,c("month","year"),transform,y=minMax(cpue))

