
fit2stageGAMtoCatch <-
function (input = pslf,which.species ='bft',start.year=1950, end.year=2015,which.flag='Japan',kk=6)
{
   # Function to fit GAMs to Effdis catch data. 
   # b1 is the probability of catching a fish as a function of location and time.
   # g1 models the positive component of the catch as a function of location and time.
  
  
   #input <- lllf;which.species <- 'alb';which.flag <- 'Japan';start.year <- 1970; end.year   <- 2010
   
  ninput <- input[input$species == which.species & input$year >= start.year & input$year <= end.year,]
  
  
   if(which.flag=='All')
   {
     print('Modeling all data')
     }
   
   else{
     ninput <- ninput[ninput$flagname == which.flag,]
   print(paste('Modeling',which.flag))
   }
   
   bin <- ifelse(ninput$measured_catch==0,0,1) # Binary variable
   ninput$bin <- bin
   
   tbin <- table(ninput$bin)
   
   if(sum(bin) >= 50)
   {
  ## Take out only the kgs. [NB. The flags which report only numbers have been converted.]
  
   ninput <- ninput[ninput$catchunit == 'kg',]
    
   ninput$lmeasured_catch <- log(ninput$measured_catch+1)
  
  ## Bernouilli model for probability of catch as a function of x,y,month,t from quasibinomial family
   
  bbs<-"cr"
  
  #Set up harmonics for seasonality
  
  dat0 <- ninput # input data is dat0
  
  ss = cc = matrix(NA,nrow=length(dat0[,1]), ncol=6)
  
  for (i in 1:6)
  { cc[,i] <- cos(2*pi*i*dat0$trend/12)                                                              
    ss[,i] <- sin(2*pi*i*dat0$trend/12) } # set up the regressors
  
  ss <- ss[,-6]
  dat1 <- cbind(dat0,ss,cc)
  dd <- dim(dat0)
  dimnames(dat1)[[2]][(dd[2]+1):(dim(dat1)[2])] <- c(paste('sin',1:5,sep=''),paste('cos',1:6,sep=''))
  
  # Fit all possible harmonics and penalise them out with REML and select =T #
  
ninput <- dat1
  
#registerDoMC(core=2)
#if(which.gam == 'gam')
#{
  
#   #print(dat1[1,])
#   Fit the gam using Hastie and Tibshirani's original code
#   print('Fitting Hastie and Tibshirani GAM to binary data')
#   library(gam)
# 
#   b1 <- gam(bin~lo(longitude,latitude)+lo(trend)+sin1+cos1+sin2+cos2+sin3+cos3+sin4+cos4,family=binomial(link="logit"),data=ninput)
#   b1.scope <-list('trend'=~1+trend,'sin1'=~1+sin1,'sin2'=~1+sin2,'cos1'=~1+cos1,'cos2'=~1+cos2,'sin3'=~1+sin3,'cos3'=~1+cos3,
#                   'sin4'=~1+sin4,'cos4'=~1+cos4)
#   #save(ninput,file='ninput')
#   #load('ninput')
#   b1 <- step.gam(object=b1,scope=b1.scope,direction = "both",parallel=TRUE)
# #}


# else
#     {
#     #Fit the GAM using Simon Wood's Generalized Cross-validation
     print('Fitting MGCV to binary data')
     registerDoMC(core=2)
   b1 <- gam(bin~te(longitude,latitude,k=kk,bs=bbs)+te(trend,k=kk,bs=bbs)+sin1+cos1+sin2+cos2+sin3+cos3+sin4+cos4+sin5+cos5+cos6,data=ninput,
             family=quasibinomial(link="logit"),method="REML",select=TRUE)
#   }
#   
 
  
  ################################################################
  #Gamma model for the positive component of task 2 catch data
  ################################################################
  
  input1 <- aggregate(list(measured_catch=ninput$measured_catch), 
                    by=list(trend=ninput$trend,month=ninput$month,longitude=ninput$longitude,
                            latitude=ninput$latitude),sum,na.rm=T)
  
  bin <- ifelse(input1$measured_catch==0,0,1)
  input1$bin <- bin
  input2 <- input1[input1$bin == 1,]
  
  #Set up harmonics for seasonality
  
  dat0 <- input2 # input data is dat0
  
  ss = cc = matrix(NA,nrow=length(dat0[,1]), ncol=6)
  
  for (i in 1:6)
  { cc[,i] <- cos(2*pi*i*dat0$trend/12)                                                              
  ss[,i] <- sin(2*pi*i*dat0$trend/12) } # set up the regressors
  
  ss <- ss[,-6]
  dat1 <- cbind(dat0,ss,cc)
  dd <- dim(dat0)
  dimnames(dat1)[[2]][(dd[2]+1):(dim(dat1)[2])] <- c(paste('sin',1:5,sep=''),paste('cos',1:6,sep=''))
  
  
  input2 <- dat1
  
#   #if(which.gam == 'gam')
#   #{
#     #Fit the gam using Hastie and Tibshirani's original code
#     print('fitting GAM to positive component')
#     library(gam)
#     registerDoMC(cores=2)
#     
#     g1 <- gam(measured_catch~lo(longitude,latitude)+lo(trend)+sin1+cos1+sin2+cos2+sin3+cos3+sin4+cos4,data=input2,family=Gamma(link="log"))
#     g1.scope <-list('trend'=~1+trend,'sin1'=~1+sin1,'sin2'=~1+sin2,'cos1'=~1+cos1,'cos2'=~1+cos2,'sin3'=~1+sin3,'cos3'=~1+cos3,
#                     'sin4'=~1+sin4,'cos4'=~1+cos4)
#    #g1 <- step.gam(object=g1,g1.scope,direction = "both",parallel=T)
#     
#   #}
  
  
#else
#    {
    
    #Fit the GAM using Simon Wood's Generalized Cross-validation 
    print('Fitting MGCV to positive component')
    registerDoMC(core=2)
  
 g1 <- gam(measured_catch~te(longitude,latitude,k=kk,bs=bbs)+te(trend,k=kk,bs=bbs)+sin1+cos1+sin2+cos2+sin3+cos3+sin4+cos4+sin5+cos5+cos6,family=Gamma(link="log"),
            method="REML",select=TRUE,data=input2)
  #}

  #print(summary(g1))
  
  gc(reset=T)
  
  mods <- list(pmod = b1, pmod.data=ninput, gmod = g1, gmod.data = input2)
  
  mods
   }
  
  else
  { 
      print ('Insufficient data to support model') 
    }
  
}
