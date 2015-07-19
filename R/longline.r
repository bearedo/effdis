### Code to estimate long-line effort in the Atlantic and Med ##

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
library(COZIGAM)
library(reshape2)

## Read in task 2 longline data from ICCAT DB ##

chan <- odbcConnect("effdis-tuna-cc1", case="postgresql", believeNRows=FALSE)
sqlTables(chan)  #List all tables in the DB
#t2ce_lf_ll <- sqlQuery(chan, "SELECT * from t2ce_long_format_ll;") # Return a table as a dataframe. Note unless you have a good connection this will take a while.

t2ce_lf_ll <- task2.lf[task2.lf$geargrpcode == 'LL',]
t2ce_lf_ll <- orderBy(~flagname+trend,data=t2ce_lf_ll)

for(i in c(4:7,12:14)) {t2ce_lf_ll[,i] <- ac(t2ce_lf_ll[,i])}


dim(t2ce_lf_ll) # 2,458,134

head(t2ce_lf_ll)

# Get the depths when Jason has done them # 


# EDA # 

table(t2ce_lf_ll$eff1type)

# D.AT SEA    D.FISH FISH.HOUR HOURS.SEA   KM.SETS LINE.DAYS  NO.BOATS  NO.HOOKS  NO.LINES    -none-   NO.NETS  NO.POLES   NO.SETS  NO.TRAPS  NO.TRIPS 
# 0       999     42093         0         0         0         0       126   2326977         0     82845         0         0      2682         0      1827 
# N.POLE-D  SUC.D.FI  SUC.SETS    TRAP D 
# 0        36       549         0 

# Only use NO.HOOKS

t2ce_lf_ll <- t2ce_lf_ll[t2ce_lf_ll$eff1type == 'NO.HOOKS',]


table(t2ce_lf_ll$catchunit,t2ce_lf_ll$flagname)
# 
# 
# Belize Brasil China P.R. Chinese Taipei   Cuba EU.Cyprus EU.España EU.Greece EU.Italy EU.Malta EU.Portugal  Japan Korea Rep.  Maroc Mexico Namibia  Other Panama
# --      0    351         27            684      9         0        54      1575      486      810           0   3465        585     45     45     189    981     54
# kg   5670  74709      10593         202158    369      1863     95517     25218    10962     9198       15291      0      73935    585   4662    7560  76770   4869
# nr      0   9477        828         138825  20691         0     91872      5022       18      162           0 314343      17316      0    702       0    576      0
# 
# Philippines South Africa St. Vincent and Grenadines Trinidad and Tobago Uruguay U.S.A. U.S.S.R. Vanuatu Venezuela
# --           9          207                         54                   0     315  21753        0      45      2466
# kg        4509        13383                       9972                 756    9279      0     1494   85860    162270
# nr           0            0                          0                   0    7812 687609        0   85860      4203
# 


t2ce_lf_ll <- t2ce_lf_ll[t2ce_lf_ll$eff1type != '--',]
t2ce_lf_ll <- t2ce_lf_ll[t2ce_lf_ll$catchunit != '--',]



tt0<- table(t2ce_lf_ll$flagname,t2ce_lf_ll$catchunit,t2ce_lf_ll$year)
tt1<- melt(tt0)

t_kg <- tt1[tt1[,2]=='kg',]
t_nr <- tt1[tt1[,2]=='nr',]
tt2 <- data.frame(t_kg,t_nr[,4])

only.nrs <- (1:length(tt2[,1]))[tt2$value ==0 & tt2$t_nr...4. > 0] # Combinations who only reported numbers. Mostly Japan and USA.

# Do data with kgs only first #


three.d.effort.by.year.r(what.year='2006',what.flag='Belize',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='Brasil',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='China P.R.',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='Chinese Taipei',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='Cuba',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='EU.Cyprus',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='EU.España',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='EU.Greece',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='EU.Italy',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='EU.Malta',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='EU.Portugal',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='Japan',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='Korea Rep',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='Maroc',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='Mexico',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='Namibia',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='Other',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='Panama',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='Philippines',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='South Africa',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='St. Vincent and Grenadines',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='Trinidad and Tobago',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='Uruguay',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='U.S.A',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='U.S.S.R',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='Vanuatu',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='Venezuala',scaling.f=100000)


three.d.catch.by.year.r(tdata=t2ce_lf_ll,what.year='1984',what.flag='EU.Italy',what.species='alb',scaling.f=1,catchunit = "kg")

yrs <- as.character(1950:2010)
us <- ac(sort(unique(task2.lf$species)))
flgs <- ac(sort(unique(task2.lf$flagname)))

for(i in us){
  for(j in yrs){
    for(k in flgs){
      print(c(i,j,k))
three.d.catch.by.year.r(tdata = t2ce_lf_ll,what.year=j,what.flag=k,what.species=i,scaling.f=10,catchunit='kg')
    }}}



for(i in us){
  for(j in yrs){
    for(k in flgs){
      print(c(i,j,k))
      three.d.catch.by.year.r(tdata = t2ce_lf_ll,what.year=j,what.flag=k,what.species=i,scaling.f=10,catchunit='nr')
    }}}




