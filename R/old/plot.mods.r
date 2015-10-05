
plot.mods <- function(input=bft.aa,cmod=bft,what.year=1995,what.month=1,grid.res=5,what.value = 'prob',what.gear='PS',plot.samples.only=TRUE)
{
 
  #cmod <- alb.ps;what.year=1990;what.month=1;grid.res=1;what.value='catch';input <- model.data
  
  
   #Function to plot model output
  min.lat <- min(cmod$pmod.data$latitude)
  max.lat <- max(cmod$pmod.data$latitude)
  min.lon <- min(cmod$pmod.data$longitude)
  max.lon <- max(cmod$pmod.data$longitude)
  
  what.species <- as.character(input$species[1])
  
  lonnie <- seq(min.lon,max.lon,by=grid.res)
  lattie <- seq(min.lat,max.lat,by=grid.res)
  lo <- length(lonnie)
  la <- length(lattie)

   if(what.value == 'prob'){
     if(plot.samples.only == TRUE)
       {
    input[,what.value][input$observation == F] <-NA
     }
     
    image(lonnie,lattie,matrix(input[,what.value][input$year==what.year & input$month == what.month],lo,la),col=topo.colors(100))
    contour(lonnie,lattie,matrix(input[,what.value][input$year==what.year & input$month == what.month],lo,la),add=T)
    map('worldHires',add=T,fill=T);
    title(paste(toupper(what.species),'-', what.value ,month.abb[what.month],what.year,what.gear))
    w0 <- (1:length(cmod$pmod.data[,1]))[cmod$pmod.data$year == what.year & cmod$pmod.data$month == what.month]
    points(cmod$pmod.data$longitude[w0],cmod$pmod.data$latitude[w0],pch='.')
   }
  
  else
  {
    if(plot.samples.only == TRUE)
      {
    input[,what.value][input$observation == F] <-NA
    }
    image(lonnie,lattie,matrix(log(input[,what.value])[input$year==what.year & input$month == what.month],lo,la),col=topo.colors(100))
    contour(lonnie,lattie,matrix(log(input[,what.value])[input$year==what.year & input$month == what.month],lo,la),add=T)
    map('worldHires',add=T,fill=T);
    title(paste(toupper(what.species),'-', what.value ,month.abb[what.month],what.year,what.gear))
    w0 <- (1:length(cmod$pmod.data$longitude))[cmod$pmod.data$year== what.year & cmod$pmod.data$month == what.month]
    points(cmod$pmod.data$longitude[w0],cmod$pmod.data$latitude[w0],pch='.')
    
    
  }
    
    
  
  
}
