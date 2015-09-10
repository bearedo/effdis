
fitGAMtoEffort.r <- function()
{
  
  
  
  
n0 <- tdata[tdata$dsettype == 'n-',]
nw <- tdata[tdata$dsettype == 'nw',]
mm <- duplicated(nw[,-9])
nw <- nw[mm==TRUE,]
w0 <- tdata[tdata$dsettype == '-w',]

tdata1 <- rbind(n0,nw,w0)
}