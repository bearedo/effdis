aggt2data <-
function(input=lllf,which.flag="Japan",which.effort='NO.HOOKS',start.year=1950,end.year=2015)

  {
  
   #input <- lllf; which.flag='Korea Rep.'; which.effort='NO.HOOKS'; start.year=1990; end.year=2015
  
  input <- input[input$year >= start.year & input$year <= end.year & input$eff1type %in% which.effort,]
  
  input <- input[input$species == 'bet',] # Doesn't matter which as the Xn data are all repeated in the long format
  
  #input[input$trend == 517 & input$longitude == -12.5 & input$latitude == -12.5 & input$flagname == "Japan",]
  #input[input$trend == 517 & input$longitude == -12.5 & input$latitude == -2.5 & input$flagname == "Korea Rep.",]
  
  
  
  if(which.flag=='All')
  {
    input <- input; print('Modeling all data')
  }
  
  else
  {
    input <- input[input$flagname == which.flag,]
    #print(paste('Modeling',which.flag))
  }
  
  
  n0 <- input[input$dsettype == "n-",]
  nw <- input[input$dsettype == "nw",]
  w0 <- input[input$dsettype == "-w",]
  
  input1 <- rbind(n0,nw,w0)
  
  #input1[input1$trend == 517 & input1$longitude == -17.5 & input1$latitude == -2.5 & input1$flagname == "Korea Rep.",]
  
  
  effort <- aggregate(list(eff1=input1$eff1), 
                      by=list(year=input1$year,month=input1$month,trend=input1$trend,month=input1$month,
                              longitude=input1$longitude,latitude=input1$latitude),sum,na.rm=T)
  
  #input2[input2$trend == 517 & input2$longitude == -17.5 & input2$latitude == -2.5,]
  
  effort$flagname <- which.flag
  
  effort <- orderBy(~trend,data=effort)
  
effort
}
