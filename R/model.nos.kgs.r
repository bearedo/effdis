

model.nos.kgs.r <- function(input = pslf,which.gear='LL',which.effort='NO.HOOKS')
{
  #Function finds the data in the task2 db which is available for both weights (kgs) and numbers. It then models kgs as a function of nos plus some other relevant covariates.
  ps <- get.effdis.t2.data.r(which.gear=which.gear,which.flag='All',which.effort=which.effort,which.dsettype = 'nw') # Get data for which there are nos and weights
  print(dim(ps))
  ps<-prepare.effdis.data.r(input=ps)
  pslf <- convert2long.format.t2.r(input =ps)
  
  
  input <- pslf


#Sum catches (kgs and nos) over catchunit

pp0 <- aggregate(list(measured_catch=input$measured_catch), 
                 by=list(trend=input$trend,month=input$month,
                         flagname=input$flagname,catchunit=input$catchunit,
                         species=input$species),sum,na.rm=T)

# Split into two

pp0_nr <- pp0[pp0$catchunit == 'nr',]
dimnames(pp0_nr)[[2]][6] <- 'measured_catch_nr'
pp0_kg <- pp0[pp0$catchunit == 'kg',]
dimnames(pp0_kg)[[2]][6] <- 'measured_catch_kg'

pp0_kg_nr <- merge(pp0_kg[,-4],pp0_nr[,-4])


# Plot the relationship between nrs and kgs for different species, flag combinations

#library(lattice)
#xyplot(log(measured_catch_kg)~log(measured_catch_nr),groups=flagname,data=pp0_kg_nr[pp0_kg_nr$species=='bft',])

#xyplot(log(measured_catch_kg)~log(measured_catch_nr)|species,auto.key=TRUE,groups=flagname,data=pp0_kg_nr)

#head(pp0_kg_nr)

#Log transform 
pp0_kg_nr$lnr <- log(pp0_kg_nr$measured_catch_nr)
pp0_kg_nr$lkg <- log(pp0_kg_nr$measured_catch_kg)


# Model kg data as a function of nr, flag and species

m1 <- lm(measured_catch_kg~measured_catch_nr,data=pp0_kg_nr,na.action='na.omit')
m2 <- lm(measured_catch_kg~measured_catch_nr+trend,data=pp0_kg_nr,na.action='na.omit')
m3 <- lm(measured_catch_kg~measured_catch_nr+trend+species,data=pp0_kg_nr,na.action='na.omit')
m4 <- lm(measured_catch_kg~measured_catch_nr+trend+species+flagname,data=pp0_kg_nr,na.action='na.omit')

#anova(m1,m2,m3,m4)

best.model <- step(m4,direction='both')


print(summary(best.model))


best.model

}












