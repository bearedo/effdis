aggt2data <-
function(input=lllf, what.flag ='Japan',what.effort='NO.HOOKS')

  {
  
  #input <- lllf;what.flag='Japan';what.effort='NO.HOOKS'
  
  input_kgs <- input[input$catchunit == 'kg' & input$eff1type == what.effort & lllf$flagname == what.flag,]

allt2 <- aggregate(list(eff=input_kgs$eff1,measured_catch=input_kgs$measured_catch), 
                   by=list(year=input_kgs$year,month=input_kgs$month,longitude=input_kgs$longitude,latitude=input_kgs$latitude,species=input_kgs$species,
                       flagname=input_kgs$flagname),sum,na.rm=T)

allt2
}
