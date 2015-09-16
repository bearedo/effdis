kgs.from.nos.r <- function(input=pslf)
{
  # Use linear model from model.nos.kgs.r to estimate kgs from numbers
  
  #input <- pslf
  
  ### Split data into kg and nr ###
  
  t2ce_lf_kg <- input[input$dsettype %in% c('-w','nw'),]
  t2ce_lf_kg <- t2ce_lf_kg[t2ce_lf_kg$catchunit  != "--",]
  
  t2ce_lf_nr <- input[input$dsettype == 'n-',] # Data where ONLY numbers have been supplied
  #Change name to co-incide with best model
  t2ce_lf_nr$measured_catch_nr <- t2ce_lf_nr$measured_catch
  
  dd <- dim(t2ce_lf_nr)
  if (dd[1]>0)

    {
  # Convert measured catch to kgs with bm #
  
  t2ce_lf_nr$lnr<- log(t2ce_lf_nr$measured_catch_nr)
  
  aa <- exp(predict(bm,t2ce_lf_nr,type='response'))
  
  ww <- (1:length(t2ce_lf_nr[,1]))[t2ce_lf_nr$measured_catch == 0]
  aa[ww] <- 0
  
  t2ce_lf_nr$measured_catch <- aa
  
  # Recombine with t2ce_lf_kg #
  
  t2ce_lf_nr$dsettype <- '-w'
  t2ce_lf_nr$catchunit <- 'kg'
  t2ce_lf_nr <- t2ce_lf_nr[,-c(16,17)]
  

  t2ce_lf <- rbind(t2ce_lf_kg,t2ce_lf_nr)
  
  t2ce_lf
  
  }
  
  else{print ('All data are in kgs')}
  
}