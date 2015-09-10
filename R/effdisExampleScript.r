
# Purse-seine example #

psn <- get.effdis.t2.data.r(which.gear='PS',which.flag='All',which.effort='D.FISH',which.dsettype = 'n-')
psnw <- get.effdis.t2.data.r(which.gear='PS',which.flag='All',which.effort='D.FISH',which.dsettype = 'nw')
psw <- get.effdis.t2.data.r(which.gear='PS',which.flag='All',which.effort='D.FISH',which.dsettype = '-w')

ps1 <- rbind(psn,psnw,psw)

ps1<-prepare.effdis.data.r(input=ps1)
pslf <- convert2long.format.t2.r(input =ps1)
bm <- model.nos.kgs.r(input=t2ce)

xxx <- kgs.from.nos.r(pslf)


# Longline example #


lln <- get.effdis.t2.data.r(which.gear='LL',which.flag='All',which.effort='NO.HOOKS',which.dsettype = 'n-')
llnw <- get.effdis.t2.data.r(which.gear='LL',which.flag='All',which.effort='NO.HOOKS',which.dsettype = 'nw')
llw <- get.effdis.t2.data.r(which.gear='LL',which.flag='All',which.effort='NO.HOOKS',which.dsettype = '-w')

ll1 <- rbind(lln,llnw,llw)

ll1<-prepare.effdis.data.r(input=ll1)
lllf <- convert2long.format.t2.r(input =ll1)
bm <- model.nos.kgs.r(input=t2ce)

xxx <- kgs.from.nos.r(lllf)
