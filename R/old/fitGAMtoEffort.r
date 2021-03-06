
fitGAMtoEffort <- function(input = lllf, which.flag='Japan',which.effort='NO.HOOKS',start.year=1950,end.year=2010,kk=6)
{
  
#input <- pslf; which.flag='EU.España'; which.effort='FISH.HOUR'; start.year=1970; end.year=2010

#input <- input[input$species == 'alb',] # Doesn't matter which as the Xn data are all repeated in the long format
#input <- input[input$year >= start.year & input$year <= end.year & input$eff1type == which.effort,]

if(which.flag=='All')
{
  input <- input; print('Modeling all data')
}

else{input <- input[input$flagname == which.flag,]
#print(paste('Modeling',which.flag))
}


n0 <- input[input$dsettype == "n-",]
nw <- input[input$dsettype == "nw",]
mm <- duplicated(nw[,c(1:11)])
nw <- nw[mm==TRUE,]
w0 <- input[input$dsettype == "-w",]

input1 <- rbind(n0,nw,w0)

input2 <- aggregate(list(eff1=input1$eff1), 
                  by=list(trend=input1$trend,month=input1$month,longitude=input1$longitude,latitude=input1$latitude),sum)

# Set up harmonics

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


#input3 <- aggregate(list(eff1=input$eff1),by=list(year=input$year),sum)

#print(input3)

bbs <- 'cr'
h1 <- gam(eff1~te(longitude,latitude,k=kk,bs=bbs)+te(trend,k=kk,bs=bbs)+sin1+cos1+sin2+cos2+sin3+cos3+sin4+cos4+sin5+cos5+cos6,family=quasipoisson(link="log"),method="REML",data=input2)
#print(summary(h1))

out<- list(emod=h1,emod.data=input2)

out
}