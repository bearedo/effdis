plot.mods <-
function(input=bft.aa,cmod=bft,which.year=1995,which.month=1,grid.res=5,which.value = 'prob',which.gear='PS',plot.samples.only=TRUE)
{
 
  #cmod <- alb.ps;which.year=1990;which.month=1;grid.res=1;which.value='catch';input <- model.data

  
   #Function to plot model output
  min.lat <- min(cmod$pmod.data$latitude)
  max.lat <- max(cmod$pmod.data$latitude)
  min.lon <- min(cmod$pmod.data$longitude)
  max.lon <- max(cmod$pmod.data$longitude)
  
  which.species <- as.character(input$species[1])
  
  lonnie <- seq(min.lon,max.lon,by=grid.res)
  lattie <- seq(min.lat,max.lat,by=grid.res)
  lo <- length(lonnie)
  la <- length(lattie)

   if(which.value == 'prob'){
     if(plot.samples.only == TRUE)
       {
    input[,which.value][input$observation == F] <-NA
     }
     
    image(lonnie,lattie,matrix(input[,which.value][input$year==which.year & input$month == which.month],lo,la),col=topo.colors(100))
    contour(lonnie,lattie,matrix(input[,which.value][input$year==which.year & input$month == which.month],lo,la),add=T)
    map('worldHires',add=T,fill=T);
    title(paste(toupper(which.species),'-', which.value ,month.abb[which.month],which.year,which.gear))
    w0 <- (1:length(cmod$pmod.data[,1]))[cmod$pmod.data$year == which.year & cmod$pmod.data$month == which.month]
    points(cmod$pmod.data$longitude[w0],cmod$pmod.data$latitude[w0],pch='.')
   }
  
  else
  {
    if(plot.samples.only == TRUE)
      {
    input[,which.value][input$observation == F] <-NA
    }
    image(lonnie,lattie,matrix(log(input[,which.value])[input$year==which.year & input$month == which.month],lo,la),col=topo.colors(100))
    contour(lonnie,lattie,matrix(log(input[,which.value])[input$year==which.year & input$month == which.month],lo,la),add=T)
    map('worldHires',add=T,fill=T);
    title(paste(toupper(which.species),'-', which.value ,month.abb[which.month],which.year,which.gear))
    w0 <- (1:length(cmod$pmod.data$longitude))[cmod$pmod.data$year== which.year & cmod$pmod.data$month == which.month]
    points(cmod$pmod.data$longitude[w0],cmod$pmod.data$latitude[w0],pch='.')
    }
    
}
