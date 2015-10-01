model.nos.kgs <-
function(input=pslf,which.dsn='effdis-local',which.gear='LL')
{
  #Function finds the data in the task2 db (long-format) which is available for both weights (kgs) and numbers. 
  #It then models kgs as a function of nos plus some other relevant covariates.

  #input <- pslf
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



# Model kg data as a function of nr and species

pp0_kg_nr$lnr[pp0_kg_nr$lnr == '-Inf'] <- NA
pp0_kg_nr$lkg[pp0_kg_nr$lkg == '-Inf'] <- NA

m1 <- lm(lkg~lnr,data=pp0_kg_nr,na.action='na.omit')
m2 <- lm(lkg~lnr+trend,data=pp0_kg_nr,na.action='na.omit')
m3 <- lm(lkg~lnr+trend+species,data=pp0_kg_nr,na.action='na.omit')

#anova(m1,m2,m3,m4)

best.model <- step(m3,direction='both')


print(summary(best.model))


best.model

}
