
# Load libraries

library(rio)
library(spatial)
library(sp)
library(doBy)
library(rgdal)
library(RODBC)
library(RColorBrewer)
library(ggplot2)
library(vmstools)
library(gam)
library(maps)
library(mapdata)
#library(COZIGAM)
library(RODBC)
library(reshape2)
library(mgcv)




# Purse-seine example #

psn <- get.effdis.t2.data.r(which.gear='PS',which.flag='All',which.effort='D.FISH',which.dsettype = 'n-')
psnw <- get.effdis.t2.data.r(which.gear='PS',which.flag='All',which.effort='D.FISH',which.dsettype = 'nw')
psw <- get.effdis.t2.data.r(which.gear='PS',which.flag='All',which.effort='D.FISH',which.dsettype = '-w')

ps1 <- rbind(psn,psnw,psw)

ps1<-prepare.effdis.data.r(input=ps1)
ps1<-find.ocean.r(ps1)$output
ps1 <- ps1[ps1$which.ocean == 'atl',]

pslf <- convert2long.format.t2.r(input =ps1)
bm <- model.nos.kgs.r(input=t2ce)

xxx <- kgs.from.nos.r(pslf)


# Longline example #

setwd('/home/doug/effdis/R')
source('fitGAMtoEffort.r')
source('fit2stageGAMtoCatch.r')
source('get.effdis.t2.data.r')
source('plot.mods.r')
source('prepare.effdis.data.r')
source('model.nos.kgs.r')
source('kgs.from.nos.r')
source('predict.effdis.t2.data.r')
source('find.ocean.r')

# Get data for each dsettype

lln  <- get.effdis.t2.data.r(which.dsn='effdis-local',which.gear='LL',which.flag='All',which.dsettype = 'n-')
llnw <- get.effdis.t2.data.r(which.dsn='effdis-local',which.gear='LL',which.flag='All',which.dsettype = 'nw')
llw  <- get.effdis.t2.data.r(which.dsn='effdis-local',which.gear='LL',which.flag='All',which.dsettype = '-w')

ll1 <- rbind(lln,llnw,llw)
ll1<-find.ocean.r(ll1)
ll1 <- ll1[ll1$which.ocean == 'atl',]
ll1<-prepare.effdis.data.r(input=ll1)


library(reshape2)
lllf <- convert2long.format.t2.r(input =ll1)
bm <- model.nos.kgs.r(input=lllf,which.gear='LL')

lllf <- kgs.from.nos.r(lllf) # for those fleets that supply only number

table(lllf$eff1type)

# D.AT SEA   D.FISH NO.BOATS NO.HOOKS   -none-  NO.SETS NO.TRIPS SUC.D.FI SUC.SETS 
# 252     6561       27  2139498    67959     1593      810       36      504 

lllf <- lllf[lllf$eff1type=='NO.HOOKS',]

round(tapply(lllf$measured_catch,list(lllf$flagname,lllf$species),sum))
# 
#                                   alb       bft       bet     skj       yft       swo      bum      sai      whm
# Belize                        1071895         0    234674     221   3703162    607120     6972    86433        0
# Brasil                       29528868     36689  23280684  304638  23561886  44227091  4165724  2695229  2539062
# China P.R.                    1680863    651431  71963332       0  11669700   4907148   578185    75977    92565
# Chinese Taipei             1112048944   2268735 427433167 4050376 168825153  63878088 21988107  4491598 16863009
# Cuba                          2318702    140124  17544588  194544  46411223  12586091  1526797        0        0
# EU.España                           0       583         0       0         0 330590030        0        0        0
# EU.Portugal                    401905      5671   1025803   85984    980496   7590550   186347   253279    46087
# Japan                       427690181 108263219 885670651  391070 530186651 153341434 63410090 30414446 22801198
# Korea Rep.                   22400908     52906  87711722   53955  58933158   7521993  1855169   488125   619906
# Maroc                               0         0    173004       0         0   1569999        0        0        0
# Mexico                            161    168022     52345   58300  12145359    357940   680208  1054383   175191
# Namibia                       3904680         0   1069953    2001    152481   4476566        0        0        0
# Other                       234209669   2365851  84171421      20  79942111   4041531    25248     1129     3737
# Panama                         454275      7750    472800       0   3408783    151500        0        0        0
# Philippines                    365909         0  13197249       0   1657759    268181    10851        0     2986
# South Africa                  3164360         0   2075170   88103   1570356   2257357     6201       70     1800
# St. Vincent and Grenadines    3259883         0   1671089  587912  13679789    148932        0   353133    43775
# Trinidad and Tobago            101989         0    216221      75   3528001    319819   114491    96471    64113
# Uruguay                        999233      2800   4061848       0    429170  13341174        0        0        0
# U.S.A.                        9377762   4084505  29461113  313144  88348762 164941702  2164056   632043   796426
# U.S.S.R.                            0         0   9325000       0   3826000    430000   112000    52000        0
# Vanuatu                       3384988         0    274000       0   2265401    117402     3418        0      260
# Venezuela                     2894234         0   7682840    7200  14322296    214114   720908   741495   647481

alb <- fit2stageGAMtoCatch.r(input=lllf,which.flag='All',which.species='alb',start.year=1990,end.year=2010)
bft <- fit2stageGAMtoCatch.r(input=lllf,which.flag='All',which.species='bft',start.year=1990,end.year=2010)
bet <- fit2stageGAMtoCatch.r(input=lllf,which.flag='All',which.species='bet',start.year=1990,end.year=2010)
skj <- fit2stageGAMtoCatch.r(input=lllf,which.flag='All',which.species='skj',start.year=1990,end.year=2010)
yft <- fit2stageGAMtoCatch.r(input=lllf,which.flag='All',which.species='yft',start.year=1990,end.year=2010)
swo <- fit2stageGAMtoCatch.r(input=lllf,which.flag='All',which.species='swo',start.year=1990,end.year=2010)
bum <- fit2stageGAMtoCatch.r(input=lllf,which.flag='All',which.species='bum',start.year=1990,end.year=2010)
sai <- fit2stageGAMtoCatch.r(input=lllf,which.flag='All',which.species='sai',start.year=1990,end.year=2010)
whm <- fit2stageGAMtoCatch.r(input=lllf,which.flag='All',which.species='whm',start.year=1990,end.year=2010)



# Do we just assume here that once we've modeled NO.HOOKS as a function of time we can use that sensibly ?

emod <- fitGAMtoEffort.r(input=lllf,which.flag='All',which.effort='NO.HOOKS',start.year=1970,end.year=2010)

# Create grids and predict over them 

aa <- predict.effdis.t2.data.r(cmod=swo, emod=emod,grid.res=2,start.year=1970,end.year=2010)


plot.mods.r(what.year = 1985,what.month=6,what.value = 'prob',grid.res=2)
plot.mods.r(what.year = 1985,what.month=6,what.value = 'measured_catch',grid.res=2)
plot.mods.r(what.year = 1985,what.month=6,what.value = 'cpue',grid.res=2)


plot(aa$month,aa$prob)
boxplot(catch~month,data=aa)
plot(aa$trend,aa$catch/1000,pch='.')
abline(v=seq(1,756,by=12),lty=2,col='blue')












