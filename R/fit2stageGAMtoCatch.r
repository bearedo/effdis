


fit2stageGAMtoCatch <- function (input = pslf,which.species ='bft',start.year=1950, end.year=2015,which.flag='Japan',kk=6)

   # Function to fit GAMs to Effdis catch data. 
   # b1 is the probability of catching a fish as a function of location and time.
   # g1 models the positive component of the catch as a function of location and time.
  {
  
   #input <- lllf;which.species <- 'alb';which.flag <- 'Chinese Taipei';start.year <- 1995; end.year   <- 2010
   
   input <- input[input$species == which.species,]
  input <- input[input$year >= start.year & input$year <= end.year,]
   
   if(which.flag=='All')
   {
     input <- input; print('Modeling all data')
     }
   
   else{
     input <- input[input$flagname == which.flag,]
   #print(paste('Modeling',which.flag))
   }
   
   bin <- ifelse(input$measured_catch==0,0,1) # Binary variable
   input$bin <- bin
   
   tbin <- table(input$bin)
   
   if(sum(bin) >= 10)
   {
  ## Take out only the kgs. [NB. The flags which report only numbers have been converted.]
  
   input <- input[input$catchunit == 'kg',]
    
   input$lmeasured_catch <- log(input$measured_catch+1)
  
  ## Bernouilli model for probability of catch as a function of x,y,month,t from quasibinomial family
   
  bbs<-"cr"
  
  dat0 <- input # input data is dat0
  
  ss = cc = matrix(NA,nr=length(dat0[,1]), nc=6)
  
  for (i in 1:6)
  { cc[,i] <- cos(2*pi*i*dat0$trend/12)                                                              
    ss[,i] <- sin(2*pi*i*dat0$trend/12) } # set up the regressors
  
  ss <- ss[,-6]
  dat1 <- cbind(dat0,ss,cc)
  dd <- dim(dat0)
  dimnames(dat1)[[2]][(dd[2]+1):(dim(dat1)[2])] <- c(paste('sin',1:5,sep=''),paste('cos',1:6,sep=''))
  
  # Fit all possible harmonics and penalise them out with REML and select =T #
  
  input <- dat1
    
  b1 <- gam(bin~te(longitude,latitude,k=6,bs=bbs)+te(trend,k=6,bs=bbs)+sin1+cos1+sin2+cos2+sin3+cos3+sin4+cos4+sin5+cos5+cos6,data=input,
            family=quasibinomial(link="logit"),method="REML",select=TRUE)
  
  #Gamma model for the positive component of task 2 catch data
  
  input1 <- aggregate(list(measured_catch=input$measured_catch), 
                    by=list(trend=input$trend,month=input$month,longitude=input$longitude,latitude=input$latitude),sum,na.rm=T)
  
  bin <- ifelse(input1$measured_catch==0,0,1)
  input1$bin <- bin
  input2 <- input1[input1$bin == 1,]
  
  dat0 <- input2 # input data is dat0
  
  ss = cc = matrix(NA,nr=length(dat0[,1]), nc=6)
  
  for (i in 1:6)
  { cc[,i] <- cos(2*pi*i*dat0$trend/12)                                                              
  ss[,i] <- sin(2*pi*i*dat0$trend/12) } # set up the regressors
  
  ss <- ss[,-6]
  dat1 <- cbind(dat0,ss,cc)
  dd <- dim(dat0)
  dimnames(dat1)[[2]][(dd[2]+1):(dim(dat1)[2])] <- c(paste('sin',1:5,sep=''),paste('cos',1:6,sep=''))
  
  
  input2 <- dat1
  
  g1 <- gam(measured_catch~te(longitude,latitude,k=kk,bs=bbs)+te(trend,k=kk,bs=bbs)+sin1+cos1+sin2+cos2+sin3+cos3+sin4+cos4+sin5+cos5+cos6,family=Gamma(link="log"),
            method="REML",select=TRUE,data=input2)
  
  #print(summary(g1))
  
  gc(reset=T)
  
  mods <- list(pmod = b1, pmod.data=input, gmod = g1, gmod.data = input2)
  
  mods
   }
  
  else{ print ('Insufficient data to support model')}
  
  
}

