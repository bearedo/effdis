fit2stageGAMtoCatch.r <- function (input = pslf)
{
  
   input <- xxx
  
  ## Take out the kgs. [NB. The flags which report only numbers have been converted.]
  
   input <- input[input$catchunit == 'kg',]
    
  ## sum catches by species ##
  
  dat1 <- aggregate(list(measured_catch=input$measured_catch), 
                    by=list(trend=input$trend,month=input$month,longitude=input$longitude,latitude=input$latitude),sum)
  
  #dat2 <- add.covariates.r(input=dat1)$output
  #dat2 <- as.data.frame(dat2)
  
  #dat2$depth_m <- dat2$depth_m * -1
  #dat2$ldepth_m <- log(dat2$depth_m)
  
  #dat <- dat[!is.na(dat$ldepth_m),]
  
  bin <- ifelse(dat1$measured_catch==0,0,1)
  dat1$bin <- bin
  dat1$lmeasured_catch <- log(dat1$measured_catch+1)
  
  #Binomial model for probability of catch
  
  bs<-"cr"
  
  b1 <- gam(bin~te(longitude,latitude,k=12,bs=bs)+te(trend,k=6,bs=bs)+te(month,k=3,bs=bs),family=quasibinomial(link="logit"),method="REML",data=dat1)
  
  print(summary(b1))
  
  #Gamma model for task 2 catch 
  
  g1 <- gam(measured_catch~te(longitude,latitude,k=12,bs=bs)+te(trend,k=6,bs=bs)+te(month,k=3,bs=bs),family=Gamma(link="log"),method="REML",data=dat1[dat1$bin==1,])
  
  print(summary(g1))
  gc()
  
  
  
  
  
  
  
  
  
  
}