
fitGAMtoEffort.r <- function(input = lllf, which.flag='Japan',which.effort='NO.HOOKS',start.year=1950,end.year=2010)
{
  
#input <- lllf; which.flag='Japan'; which.effort='NO.HOOKS'; start.year=1995; end.year=2010

input <- lllf[lllf$species == 'alb',] # Doesn't matter which as the Xn data are all repeated in the long format
input <- input[input$year >= start.year & input$year <= end.year,]

if(which.flag=='All')
{input <- input; print('Modeling all data')}

else{input <- input[input$flagname == which.flag,]
print(paste('Modeling',which.flag))}


n0 <- input[input$dsettype == "n-",]
nw <- input[input$dsettype == "nw",]
mm <- duplicated(nw[,c(1:11)])
nw <- nw[mm==TRUE,]
w0 <- input[input$dsettype == "-w",]

input1 <- rbind(n0,nw,w0)

input2 <- aggregate(list(eff1=input1$eff1), 
                  by=list(trend=input1$trend,month=input1$month,longitude=input1$longitude,latitude=input1$latitude),sum)

bbs <- 'cr'
h1 <- gam(eff1~te(longitude,latitude,k=6,bs=bbs)+te(trend,k=6,bs=bbs)+te(month,k=3,bs=bbs),family=quasipoisson(link="log"),method="REML",data=input2)
print(summary(h1))

out<- list(emod=h1,emod.data=input2)

out
}