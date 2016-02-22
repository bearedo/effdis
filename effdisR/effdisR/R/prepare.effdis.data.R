prepare.effdis.data <-
function(input=data)
  {
  #input <-ps
  #Prepare and format data extracted using get.effdis.data.r#
  # Remove strange data values
  input <- input[input$eff1type != '--',] 
  input <- input[input$catchunit != '--',] 
  input <- input[input$eff1type != '--',]
  
 input$catchunit <- as.character(input$catchunit)
  input$dsettype  <- as.character(input$dsettype)
  input$flagname  <- as.character(input$flagname)
  input$fleetcode  <- as.character(input$fleetcode)
  input$geargrpcode  <- as.character(input$geargrpcode)
  input$squaretypecode <- as.character(input$squaretypecode)
  input$region  <- as.character(input$region)
  input$eff1type  <- as.character(input$eff1type)
  
  #Add covariates (depth,sst) which may be useful for modeling
  #input <- add.covariates.r(input = input, what.dsn = 'effdis-tuna-cc1')
  input
}
