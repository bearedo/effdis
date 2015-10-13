aggt2data <-
function(input=lllf, which.flag ='Japan',which.effort='NO.HOOKS')

  {
  
  #input <- lllf;what.flag='Japan';what.effort='NO.HOOKS'
  
  if(which.flag=='All')
  {
    
    input_kgs <- input[input$catchunit == 'kg' & input$eff1type == which.effort,]
    allt2 <- aggregate(list(eff=input_kgs$eff1,measured_catch=input_kgs$measured_catch), 
                       by=list(year=input_kgs$year,month=input_kgs$month,longitude=input_kgs$longitude,latitude=input_kgs$latitude,
                               species=input_kgs$species,flagname=input_kgs$flagname),sum,na.rm=T)
    
    
  }
  
  else{
  input_kgs <- input[input$catchunit == 'kg' & input$eff1type == which.effort & lllf$flagname == which.flag,]

allt2 <- aggregate(list(eff=input_kgs$eff1,measured_catch=input_kgs$measured_catch), 
                   by=list(year=input_kgs$year,month=input_kgs$month,longitude=input_kgs$longitude,latitude=input_kgs$latitude,species=input_kgs$species,
                       flagname=input_kgs$flagname),sum,na.rm=T)
  }
  
  
allt2
}
