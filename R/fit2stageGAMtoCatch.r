fit2stageGAMtoCatch.r <- function (input = pslf,which.species ='bft',start.year=1950, end.year=2015)
{
  
   which.species <- 'bft'
   input <- xxx[xxx$species == which.species,]
   start.year <- 2005
   end.year   <- 2010
   input <- input[input$year >= start.year & input$year <= end.year,]
   
   bin <- ifelse(input$measured_catch==0,0,1)
   input$bin <- bin
   
  
  ## Take out the kgs. [NB. The flags which report only numbers have been converted.]
  
   input <- input[input$catchunit == 'kg',]
    
   input$lmeasured_catch <- log(input$measured_catch+1)
  
  ## Binomial (Bernoulli) model for probability of catch
  
  bs<-"cr"
  
  b1 <- gam(bin~te(longitude,latitude,k=12,bs=bs)+te(trend,k=6,bs=bs)+te(month,k=3,bs=bs),family=quasibinomial(link="logit"),method="REML",data=input)
  

  print(summary(b1))
  
  #Gamma model for task 2 catch 
  
  dat1 <- aggregate(list(measured_catch=input$measured_catch), 
                    by=list(trend=input$trend,month=input$month,longitude=input$longitude,latitude=input$latitude),sum,na.rm=T)
  
  
  
  g1 <- gam(measured_catch~te(longitude,latitude,k=12,bs=bs)+te(trend,k=6,bs=bs)+te(month,k=3,bs=bs),family=Gamma(link="log"),method="REML",data=dat1[dat1$bin==1,])
  
  print(summary(g1))
  gc()
  
}