
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

# Get data for each dsettype
lln <- get.effdis.t2.data.r(which.dsn='effdis-local',which.gear='LL',which.flag='All',which.effort='NO.HOOKS',which.dsettype = 'n-')
llnw <- get.effdis.t2.data.r(which.dsn='effdis-local',which.gear='LL',which.flag='All',which.effort='NO.HOOKS',which.dsettype = 'nw')
llw <- get.effdis.t2.data.r(which.dsn='effdis-local',which.gear='LL',which.flag='All',which.effort='NO.HOOKS',which.dsettype = '-w')

ll1 <- rbind(lln,llnw,llw)
ll1<-find.ocean.r(ll1)$output
ll1 <- ll1[ll1$which.ocean == 'atl',]
ll1<-prepare.effdis.data.r(input=ll1)


library(reshape2)
lllf <- convert2long.format.t2.r(input =ll1)
bm <- model.nos.kgs.r(which.gear='LL',which.effort='NO.HOOKS')

xxx <- kgs.from.nos.r(lllf) # for those fleets that supply on
