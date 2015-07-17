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

## Read in task 2 longline data from ICCAT DB ##

chan <- odbcConnect("effdis-tuna-cc1", case="postgresql", believeNRows=FALSE)
sqlTables(chan)  #List all tables in the DB
#t2ce_lf_ll <- sqlQuery(chan, "SELECT * from t2ce_long_format_ll;") # Return a table as a dataframe. Note unless you have a good connection this will take a while.

t2ce_lf_ll <- task2.lf[task2.lf$geargrpcode == 'LL',]
t2ce_lf_ll <- orderBy(~flagname+trend,data=t2ce_lf_ll)

for(i in c(4:7,12:15)) {t2ce_lf_ll[,i] <- ac(t2ce_lf_ll[,i])}


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


tt<- table(t2ce_lf_ll$flagname,t2ce_lf_ll$catchunit,t2ce_lf_ll$year)



