


fit2stageGAMtoCatch.r <- function (input = pslf,which.species ='bft',start.year=1950, end.year=2015,which.flag='Japan')

   # Function to fit GAMs to Effdis catch data. 
   # b1 is the probability of catching a fish as a function of location and time.
   # g1 models the positive component of the catch as a function of location and time.
  {
  
  # which.species <- 'alb'
  # which.flag <- 'Japan'
  # start.year <- 1990
  # end.year   <- 2010
   
   input <- lllf[lllf$species == which.species,]
   input <- input[input$year >= start.year & input$year <= end.year,]
   
   if(which.flag=='All')
   {input <- input; print('Modeling all data')}
   
   else{input <- input[input$flagname == which.flag,]
   print(paste('Modeling',which.flag))}
   
   bin <- ifelse(input$measured_catch==0,0,1) # Binary variable
   input$bin <- bin
   
  ## Take out the kgs. [NB. The flags which report only numbers have been converted.]
  
   input <- input[input$catchunit == 'kg',]
    
   input$lmeasured_catch <- log(input$measured_catch+1)
  
  ## Bernouilli model for probability of catch as a function of x,y,month,t from quasibinomial family
   
  bbs<-"cr"
  
  b1 <- gam(bin~te(longitude,latitude,k=12,bs=bbs)+te(trend,k=6,bs=bbs)+te(month,k=3,bs=bbs),family=quasibinomial(link="logit"),method="REML",data=input)
  
  print(summary(b1))
  
  #Gamma model for the positive component of task 2 catch data
  
  input1 <- aggregate(list(measured_catch=input$measured_catch), 
                    by=list(trend=input$trend,month=input$month,longitude=input$longitude,latitude=input$latitude),sum,na.rm=T)
  
  bin <- ifelse(input1$measured_catch==0,0,1)
  input1$bin <- bin
  input2 <- input1[input1$bin == 1,]
  
  g1 <- gam(measured_catch~te(longitude,latitude,k=6,bs=bbs)+te(trend,k=6,bs=bbs)+te(month,k=3,bs=bbs),family=Gamma(link="log"),method="REML",data=input2)
  
  print(summary(g1))
  
  gc(reset=T)
  
  mods <- list(pmod = b1, pmod.data=input, gmod = g1, gmod.data = input2)
  
  mods
}

