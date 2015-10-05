pkgname <- "effdisR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('effdisR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("add.covariates")
### * add.covariates

flush(stderr()); flush(stdout())

### Name: add.covariates
### Title: Function to add depth, sst, chlorophyll a and primary production
###   to a data frame
### Aliases: add.covariates
### Keywords: ~kwd1 ~kwd2

### ** Examples

require(RODBC)
#lln  <- get.effdis.t2.data(which.dsn='effdis-tuna-cc1',which.gear='LL',which.flag='Brasil',which.dsettype = 'n-') # Get data from cloud server
#require(reshape2)
#llnlf <- convert.to.long.format.t2(input = lln) # Convert to long format
#llnlf <- add.covariates(input=llnlf) # Add on covariates



## The function is currently defined as
function (input = t2ce_lf_ll, what.dsn = "effdis-tuna-cc1") 
{
    chan <- odbcConnect(what.dsn, case = "postgresql", believeNRows = FALSE)
    t2ce_distinct_locations_covariates <- sqlQuery(chan, "SELECT *, ST_AsText(the_geom_4326) AS the_point from t2ce_distinct_locations_covariates;")
    tdlc <- SpatialPointsDataFrame(cbind(x = an(ac(t2ce_distinct_locations_covariates$longitude)), 
        y = an(ac(t2ce_distinct_locations_covariates$lat))), 
        data = t2ce_distinct_locations_covariates)
    geogWGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    tdlc@proj4string <- geogWGS84
    input$depth_m <- tdlc@data$depth_m[match(paste(input$longitude, 
        input$latitude), paste(tdlc@data$longitude, tdlc@data$latitude))]
    input$m_ann_sst <- tdlc@data$m_ann_sst[match(paste(input$longitude, 
        input$latitude), paste(tdlc@data$longitude, tdlc@data$latitude))]
    input$aq_prim_pro <- tdlc@data$aq_prim_pro[match(paste(input$longitude, 
        input$latitude), paste(tdlc@data$longitude, tdlc@data$latitude))]
    input$m_ann_chla <- tdlc@data$m_ann_chla[match(paste(input$longitude, 
        input$latitude), paste(tdlc@data$longitude, tdlc@data$latitude))]
    out <- input
    out
  }



cleanEx()
nameEx("aggt2data")
### * aggt2data

flush(stderr()); flush(stdout())

### Name: aggt2data
### Title: Function to aggregate Task II data as an alternative to modeling
### Aliases: aggt2data
### Keywords: ~kwd1 ~kwd2

### ** Examples


#totals <- agg2data(input=lllf,what.flag="Chinese Taipei", what.effort = "NO.HOOKS") # Total catches by location, year, month and species for selected flag country


## The function is currently defined as
function (input = lllf, what.flag = "Japan", what.effort = "NO.HOOKS") 
{
    input_kgs <- input[input$catchunit == "kg" & input$eff1type == 
        what.effort & lllf$flagname == what.flag, ]
    allt2 <- aggregate(list(eff = input_kgs$eff1, measured_catch = input_kgs$measured_catch), 
        by = list(year = input_kgs$year, month = input_kgs$month, 
            longitude = input_kgs$longitude, latitude = input_kgs$latitude, 
            species = input_kgs$species, flagname = input_kgs$flagname), 
        sum, na.rm = T)
    allt2
  }



cleanEx()
nameEx("convert2long.format.t2")
### * convert2long.format.t2

flush(stderr()); flush(stdout())

### Name: convert2long.format.t2
### Title: Function to convert short format to long format which is useful
###   for regression modeling
### Aliases: convert2long.format.t2
### Keywords: ~kwd1 ~kwd2

### ** Examples



#lllf <- convert2long.format.t2(input =ll1) # Convert longline data in the short format to long format.

## The function is currently defined as
function (input = t2ce) 
{
    tdata <- input
    task2.simple <- data.frame(year = tdata$year, trend = tdata$trend, 
        month = tdata$month, region = tdata$region, flagname = tdata$flagname, 
        fleetcode = tdata$fleetcode, geargrpcode = tdata$geargrpcode, 
        longitude = tdata$longitude, latitude = tdata$latitude, 
        eff1 = tdata$eff1, eff1type = tdata$eff1type, dsettype = tdata$dsettype, 
        catchunit = tdata$catchunit, alb = tdata$alb, bft = tdata$bft, 
        bet = tdata$bet, skj = tdata$skj, yft = tdata$yft, swo = tdata$swo, 
        bum = tdata$bum, sai = tdata$sai, whm = tdata$whm, tot9sp = tdata$totsp9)
    task2.lf <- melt(task2.simple[, -23], id = c("year", "trend", 
        "month", "region", "flagname", "fleetcode", "geargrpcode", 
        "longitude", "latitude", "eff1", "eff1type", "dsettype", 
        "catchunit"))
    dimnames(task2.lf)[[2]][14:15] <- c("species", "measured_catch")
    task2.lf
  }



cleanEx()
nameEx("effdisR-package")
### * effdisR-package

flush(stderr()); flush(stdout())

### Name: effdisR-package
### Title: Analysis, visualisation and effort estimate using the fishing
###   effort databases maintained by ICCAT (International Commission for
###   the Conservation of Atlantic Tunas)
### Aliases: effdisR-package effdisR
### Keywords: package

### ** Examples

 simple examples of the most important functions 



cleanEx()
nameEx("find.ocean")
### * find.ocean

flush(stderr()); flush(stdout())

### Name: find.ocean
### Title: Function to determine which Ocean an observation is in
### Aliases: find.ocean
### Keywords: ~kwd1 ~kwd2

### ** Examples


#llnw <- get.effdis.t2.data(which.dsn='effdis-tuna-cc1',which.gear='LL',which.flag='All',which.dsettype = 'nw') # Get data from PostgreSQL
#llnw <- find.ocean(input=llnw) # Add which.ocean variable


## The function is currently defined as
function (input = grd) 
{
    seas <- readOGR(dsn = "/home/doug/effdis/data", layer = "World_Seas")
    geogWGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    seas@proj4string <- geogWGS84
    seas.polys <- as.character(sort(unique(seas@data$NAME)))
    wo <- grep("Atl", seas.polys)
    wi <- grep("Med", seas.polys)
    wj <- grep("Adriatic", seas.polys)
    wk <- grep("Aegean", seas.polys)
    wl <- grep("Balearic", seas.polys)
    wm <- grep("Bay of Biscay", seas.polys)
    wn <- grep("Bristol", seas.polys)
    wp <- grep("Caribbean", seas.polys)
    wq <- grep("Celtic", seas.polys)
    wr <- grep("English Channel", seas.polys)
    ws <- grep("Lawrence", seas.polys)
    wt <- grep("Inner Seas", seas.polys)
    wu <- grep("Ionian", seas.polys)
    wv <- grep("Irish", seas.polys)
    wx <- grep("North Sea", seas.polys)
    wz <- grep("Gibra", seas.polys)
    wa <- grep("Ligurian", seas.polys)
    wzz <- grep("Tyrr", seas.polys)
    wxx <- grep("Alb", seas.polys)
    wmm <- grep("Mex", seas.polys)
    wpa <- grep("Pacific", seas.polys)
    gog <- grep("Guin", seas.polys)
    atlantic <- seas[seas@data$NAME %in% seas.polys[c(wo, wp, 
        ws, wv, wmm, gog)], ]
    med <- seas[seas@data$NAME %in% seas.polys[c(wi, wj, wk, 
        wl, wu, wz, wa, wzz, wxx)], ]
    pacific <- seas[seas@data$NAME %in% seas.polys[c(wpa)], ]
    input.spdf <- SpatialPointsDataFrame(cbind(x = an(ac(input$longitude)), 
        y = an(ac(input$latitude))), data = input)
    geogWGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    input.spdf@proj4string <- geogWGS84
    idx.atl <- over(input.spdf, atlantic)
    idx.med <- over(input.spdf, med)
    idx.pac <- over(input.spdf, pacific)
    which.ocean <- rep(NA, length(input[, 1]))
    which.ocean[which(!is.na(idx.atl[, 1]))] <- "atl"
    which.ocean[which(!is.na(idx.med[, 1]))] <- "med"
    which.ocean[which(!is.na(idx.pac[, 1]))] <- "pac"
    which.ocean[is.na(which.ocean)] <- "land"
    input$which.ocean <- which.ocean
    input
  }



cleanEx()
nameEx("fit2stageGAMtoCatch")
### * fit2stageGAMtoCatch

flush(stderr()); flush(stdout())

### Name: fit2stageGAMtoCatch
### Title: Function to fit generalised additive models to the EFFDIS catch
###   data in 2 stages. The first is a Bernoulli model the second is a
###   Gamma model fitted to the positive part of the data
### Aliases: fit2stageGAMtoCatch
### Keywords: ~kwd1 ~kwd2

### ** Examples


#alb <- fit2stageGAMtoCatch(input=lllf,which.flag='Japan',which.species='alb',start.year=1995,end.year=2010)


## The function is currently defined as
function (input = pslf, which.species = "bft", start.year = 1950, 
    end.year = 2015, which.flag = "Japan", kk = 6) 
{
    input <- input[input$species == which.species, ]
    input <- input[input$year >= start.year & input$year <= end.year, 
        ]
    if (which.flag == "All") {
        input <- input
        print("Modeling all data")
    }
    else {
        input <- input[input$flagname == which.flag, ]
    }
    bin <- ifelse(input$measured_catch == 0, 0, 1)
    input$bin <- bin
    tbin <- table(input$bin)
    if (sum(bin) >= 10) {
        input <- input[input$catchunit == "kg", ]
        input$lmeasured_catch <- log(input$measured_catch + 1)
        bbs <- "cr"
        dat0 <- input
        ss = cc = matrix(NA, nr = length(dat0[, 1]), nc = 6)
        for (i in 1:6) {
            cc[, i] <- cos(2 * pi * i * dat0$trend/12)
            ss[, i] <- sin(2 * pi * i * dat0$trend/12)
        }
        ss <- ss[, -6]
        dat1 <- cbind(dat0, ss, cc)
        dd <- dim(dat0)
        dimnames(dat1)[[2]][(dd[2] + 1):(dim(dat1)[2])] <- c(paste("sin", 
            1:5, sep = ""), paste("cos", 1:6, sep = ""))
        input <- dat1
        b1 <- gam(bin ~ te(longitude, latitude, k = 6, bs = bbs) + 
            te(trend, k = 6, bs = bbs) + sin1 + cos1 + sin2 + 
            cos2 + sin3 + cos3 + sin4 + cos4 + sin5 + cos5 + 
            cos6, data = input, family = quasibinomial(link = "logit"), 
            method = "REML", select = TRUE)
        input1 <- aggregate(list(measured_catch = input$measured_catch), 
            by = list(trend = input$trend, month = input$month, 
                longitude = input$longitude, latitude = input$latitude), 
            sum, na.rm = T)
        bin <- ifelse(input1$measured_catch == 0, 0, 1)
        input1$bin <- bin
        input2 <- input1[input1$bin == 1, ]
        dat0 <- input2
        ss = cc = matrix(NA, nr = length(dat0[, 1]), nc = 6)
        for (i in 1:6) {
            cc[, i] <- cos(2 * pi * i * dat0$trend/12)
            ss[, i] <- sin(2 * pi * i * dat0$trend/12)
        }
        ss <- ss[, -6]
        dat1 <- cbind(dat0, ss, cc)
        dd <- dim(dat0)
        dimnames(dat1)[[2]][(dd[2] + 1):(dim(dat1)[2])] <- c(paste("sin", 
            1:5, sep = ""), paste("cos", 1:6, sep = ""))
        input2 <- dat1
        g1 <- gam(measured_catch ~ te(longitude, latitude, k = kk, 
            bs = bbs) + te(trend, k = kk, bs = bbs) + sin1 + 
            cos1 + sin2 + cos2 + sin3 + cos3 + sin4 + cos4 + 
            sin5 + cos5 + cos6, family = Gamma(link = "log"), 
            method = "REML", select = TRUE, data = input2)
        gc(reset = T)
        mods <- list(pmod = b1, pmod.data = input, gmod = g1, 
            gmod.data = input2)
        mods
    }
    else {
        print("Insufficient data to support model")
    }
  }



cleanEx()
nameEx("fitGAMtoEffort")
### * fitGAMtoEffort

flush(stderr()); flush(stdout())

### Name: fitGAMtoEffort
### Title: Function to fit generalised additive model from the quasipoisson
###   family to the effort data
### Aliases: fitGAMtoEffort
### Keywords: ~kwd1 ~kwd2

### ** Examples


emod <- fitGAMtoEffort(input=lllf,which.flag='Japan',which.effort='NO.HOOKS',start.year=1970,end.year=2010,which.gam='gam')



## The function is currently defined as
function (input = lllf, which.flag = "Japan", which.effort = "NO.HOOKS", 
    start.year = 1950, end.year = 2010, kk = 6) 
{
    if (which.flag == "All") {
        input <- input
        print("Modeling all data")
    }
    else {
        input <- input[input$flagname == which.flag, ]
    }
    n0 <- input[input$dsettype == "n-", ]
    nw <- input[input$dsettype == "nw", ]
    mm <- duplicated(nw[, c(1:11)])
    nw <- nw[mm == TRUE, ]
    w0 <- input[input$dsettype == "-w", ]
    input1 <- rbind(n0, nw, w0)
    input2 <- aggregate(list(eff1 = input1$eff1), by = list(trend = input1$trend, 
        month = input1$month, longitude = input1$longitude, latitude = input1$latitude), 
        sum)
    dat0 <- input2
    ss = cc = matrix(NA, nr = length(dat0[, 1]), nc = 6)
    for (i in 1:6) {
        cc[, i] <- cos(2 * pi * i * dat0$trend/12)
        ss[, i] <- sin(2 * pi * i * dat0$trend/12)
    }
    ss <- ss[, -6]
    dat1 <- cbind(dat0, ss, cc)
    dd <- dim(dat0)
    dimnames(dat1)[[2]][(dd[2] + 1):(dim(dat1)[2])] <- c(paste("sin", 
        1:5, sep = ""), paste("cos", 1:6, sep = ""))
    input2 <- dat1
    bbs <- "cr"
    h1 <- gam(eff1 ~ te(longitude, latitude, k = kk, bs = bbs) + 
        te(trend, k = kk, bs = bbs) + sin1 + cos1 + sin2 + cos2 + 
        sin3 + cos3 + sin4 + cos4 + sin5 + cos5 + cos6, family = quasipoisson(link = "log"), 
        method = "REML", data = input2)
    out <- list(emod = h1, emod.data = input2)
    out
  }



cleanEx()
nameEx("get.effdis.t1.data")
### * get.effdis.t1.data

flush(stderr()); flush(stdout())

### Name: get.effdis.t1.data
### Title: Function to get the Task I data from the PostgreSQL database on
###   the ICCAT cloud server
### Aliases: get.effdis.t1.data
### Keywords: ~kwd1 ~kwd2

### ** Examples


#ll.t1 <- get.effdis.t1.data.r(which.dsn='effdis-tuna-cc1',which.gear = 'LL',which.region='AT',which.flag='Japan')





cleanEx()
nameEx("get.effdis.t2.data")
### * get.effdis.t2.data

flush(stderr()); flush(stdout())

### Name: get.effdis.t2.data
### Title: Function to get the Task II data from the postgreSQL database on
###   the ICCAT cloud server
### Aliases: get.effdis.t2.data
### Keywords: ~kwd1 ~kwd2

### ** Examples

#llnw <- get.effdis.t2.data(which.dsn='effdis-tuna-cc1',which.gear='LL',which.flag='All',which.dsettype = 'nw')



cleanEx()
nameEx("kgs.from.nos")
### * kgs.from.nos

flush(stderr()); flush(stdout())

### Name: kgs.from.nos
### Title: Function to estimate weight caught for countries that only
###   record numbers
### Aliases: kgs.from.nos
### Keywords: ~kwd1 ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (input = pslf) 
{
    t2ce_lf_kg <- input[input$dsettype %in% c("-w", "nw"), ]
    t2ce_lf_kg <- t2ce_lf_kg[t2ce_lf_kg$catchunit != "--", ]
    t2ce_lf_nr <- input[input$dsettype == "n-", ]
    t2ce_lf_nr$measured_catch_nr <- t2ce_lf_nr$measured_catch
    dd <- dim(t2ce_lf_nr)
    if (dd[1] > 0) {
        t2ce_lf_nr$lnr <- log(t2ce_lf_nr$measured_catch_nr)
        aa <- exp(predict(bm, t2ce_lf_nr, type = "response"))
        ww <- (1:length(t2ce_lf_nr[, 1]))[t2ce_lf_nr$measured_catch == 
            0]
        aa[ww] <- 0
        t2ce_lf_nr$measured_catch <- aa
        t2ce_lf_nr$dsettype <- "-w"
        t2ce_lf_nr$catchunit <- "kg"
        t2ce_lf_nr <- t2ce_lf_nr[, -c(16, 17)]
        t2ce_lf <- rbind(t2ce_lf_kg, t2ce_lf_nr)
        t2ce_lf
    }
    else {
        print("All data are in kgs")
    }
  }



cleanEx()
nameEx("model.nos.kgs")
### * model.nos.kgs

flush(stderr()); flush(stdout())

### Name: model.nos.kgs
### Title: Function to fit linear model (kgs as a function of numbers) for
###   those flags that send both
### Aliases: model.nos.kgs
### Keywords: ~kwd1 ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (input = pslf, which.dsn = "effdis-local", which.gear = "LL") 
{
    pp0 <- aggregate(list(measured_catch = input$measured_catch), 
        by = list(trend = input$trend, month = input$month, flagname = input$flagname, 
            catchunit = input$catchunit, species = input$species), 
        sum, na.rm = T)
    pp0_nr <- pp0[pp0$catchunit == "nr", ]
    dimnames(pp0_nr)[[2]][6] <- "measured_catch_nr"
    pp0_kg <- pp0[pp0$catchunit == "kg", ]
    dimnames(pp0_kg)[[2]][6] <- "measured_catch_kg"
    pp0_kg_nr <- merge(pp0_kg[, -4], pp0_nr[, -4])
    library(lattice)
    xyplot(log(measured_catch_kg) ~ log(measured_catch_nr), groups = flagname, 
        data = pp0_kg_nr[pp0_kg_nr$species == "bft", ])
    xyplot(log(measured_catch_kg) ~ log(measured_catch_nr) | 
        species, auto.key = TRUE, groups = flagname, data = pp0_kg_nr)
    pp0_kg_nr$lnr <- log(pp0_kg_nr$measured_catch_nr)
    pp0_kg_nr$lkg <- log(pp0_kg_nr$measured_catch_kg)
    pp0_kg_nr$lnr[pp0_kg_nr$lnr == "-Inf"] <- NA
    pp0_kg_nr$lkg[pp0_kg_nr$lkg == "-Inf"] <- NA
    m1 <- lm(lkg ~ lnr, data = pp0_kg_nr, na.action = "na.omit")
    m2 <- lm(lkg ~ lnr + trend, data = pp0_kg_nr, na.action = "na.omit")
    m3 <- lm(lkg ~ lnr + trend + species, data = pp0_kg_nr, na.action = "na.omit")
    best.model <- step(m3, direction = "both")
    print(summary(best.model))
    best.model
  }



cleanEx()
nameEx("plot.mods")
### * plot.mods

flush(stderr()); flush(stdout())

### Name: plot.mods
### Title: Function to plot output of fitted GAMs
### Aliases: plot.mods
### Keywords: ~kwd1 ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (input = bft.aa, cmod = bft, which.year = 1995, which.month = 1, 
    grid.res = 5, which.value = "prob", which.gear = "PS", plot.samples.only = TRUE) 
{
    min.lat <- min(cmod$pmod.data$latitude)
    max.lat <- max(cmod$pmod.data$latitude)
    min.lon <- min(cmod$pmod.data$longitude)
    max.lon <- max(cmod$pmod.data$longitude)
    which.species <- as.character(input$species[1])
    lonnie <- seq(min.lon, max.lon, by = grid.res)
    lattie <- seq(min.lat, max.lat, by = grid.res)
    lo <- length(lonnie)
    la <- length(lattie)
    if (which.value == "prob") {
        if (plot.samples.only == TRUE) {
            input[, which.value][input$observation == F] <- NA
        }
        image(lonnie, lattie, matrix(input[, which.value][input$year == 
            which.year & input$month == which.month], lo, la), 
            col = topo.colors(100))
        contour(lonnie, lattie, matrix(input[, which.value][input$year == 
            which.year & input$month == which.month], lo, la), 
            add = T)
        map("worldHires", add = T, fill = T)
        title(paste(toupper(which.species), "-", which.value, month.abb[which.month], 
            which.year, which.gear))
        w0 <- (1:length(cmod$pmod.data[, 1]))[cmod$pmod.data$year == 
            which.year & cmod$pmod.data$month == which.month]
        points(cmod$pmod.data$longitude[w0], cmod$pmod.data$latitude[w0], 
            pch = ".")
    }
    else {
        if (plot.samples.only == TRUE) {
            input[, which.value][input$observation == F] <- NA
        }
        image(lonnie, lattie, matrix(log(input[, which.value])[input$year == 
            which.year & input$month == which.month], lo, la), 
            col = topo.colors(100))
        contour(lonnie, lattie, matrix(log(input[, which.value])[input$year == 
            which.year & input$month == which.month], lo, la), 
            add = T)
        map("worldHires", add = T, fill = T)
        title(paste(toupper(which.species), "-", which.value, month.abb[which.month], 
            which.year, which.gear))
        w0 <- (1:length(cmod$pmod.data$longitude))[cmod$pmod.data$year == 
            which.year & cmod$pmod.data$month == which.month]
        points(cmod$pmod.data$longitude[w0], cmod$pmod.data$latitude[w0], 
            pch = ".")
    }
  }



cleanEx()
nameEx("predict.effdis.t2.data")
### * predict.effdis.t2.data

flush(stderr()); flush(stdout())

### Name: predict.effdis.t2.data
### Title: Function to create comprehensive space-time grid and predict
###   data from the GAMs
### Aliases: predict.effdis.t2.data
### Keywords: ~kwd1 ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (cmod = mods, effmod = emod, grid.res = 5, start.year = 1995, 
    end.year = 2010, which.flag = "All", which.gear = "LL") 
{
    pmod.data <- cmod$pmod.data
    min.lat <- min(pmod.data$latitude)
    max.lat <- max(pmod.data$latitude)
    min.lon <- min(pmod.data$longitude)
    max.lon <- max(pmod.data$longitude)
    t1 <- min(pmod.data$trend)
    t2 <- max(pmod.data$trend)
    lonnie <- seq(min.lon, max.lon, by = grid.res)
    lattie <- seq(min.lat, max.lat, by = grid.res)
    lo <- length(lonnie)
    la <- length(lattie)
    grd <- data.frame(expand.grid(longitude = lonnie, latitude = lattie))
    grd <- find.ocean.r(input = grd[, c(1, 2)])
    lyrs <- length(start.year:end.year)
    lloc <- lo * la
    ltrnd <- lyrs * 12
    ngrd <- data.frame(longitude = rep(grd$longitude, ltrnd), 
        latitude = rep(grd$latitude, ltrnd), which.ocean = rep(grd$which.ocean, 
            ltrnd), year = rep(start.year:end.year, rep((lo * 
            la * 12), lyrs)), month = rep(rep(1:12, rep(lo * 
            la, 12)), lyrs))
    ngrd$trend <- trend.r(ngrd$year, ngrd$month, start.year = 1950)
    ngrd$flagname <- which.flag
    ngrd$geargrp <- which.gear
    dat0 <- ngrd
    ss = cc = matrix(NA, nr = length(dat0[, 1]), nc = 6)
    for (i in 1:6) {
        cc[, i] <- cos(2 * pi * i * dat0$trend/12)
        ss[, i] <- sin(2 * pi * i * dat0$trend/12)
    }
    ss <- ss[, -6]
    dat1 <- cbind(dat0, ss, cc)
    dd <- dim(dat0)
    dimnames(dat1)[[2]][(dd[2] + 1):(dim(dat1)[2])] <- c(paste("sin", 
        1:5, sep = ""), paste("cos", 1:6, sep = ""))
    ngrd <- dat1
    prob <- predict(cmod$pmod, ngrd, type = "response")
    measured_catch <- predict(cmod$gmod, ngrd, type = "response")
    eff <- predict(effmod$emod, ngrd, type = "response")
    prob[ngrd$which.ocean %in% c("land", "med", "pac")] <- NA
    measured_catch[ngrd$which.ocean %in% c("land", "med", "pac")] <- NA
    eff[ngrd$which.ocean %in% c("land", "med", "pac")] <- NA
    ngrd$prob <- round(as.vector(prob), 3)
    ngrd$measured_catch <- round(as.vector(measured_catch), 3)
    ngrd$eff <- round(as.vector(eff), 3)
    which.species <- as.character(cmod$pmod.data$species[1])
    print(which.species)
    ngrd$species <- which.species
    ngrd$catch <- round(ngrd$prob * ngrd$measured_catch, 3)
    ngrd$cpue <- round(ngrd$catch/ngrd$eff, 3)
    mm.dat <- paste(pmod.data$longitude, pmod.data$latitude, 
        pmod.data$trend, pmod.data$month)
    mm.grd <- paste(ngrd$longitude, ngrd$latitude, ngrd$trend, 
        ngrd$month)
    mm <- match(mm.grd, mm.dat)
    mm <- ifelse(is.na(mm), F, T)
    ngrd$observation <- mm
    model.data <- ngrd
    filename <- paste("model-data-", which.species, "-", which.flag, 
        "-", which.gear, "-", start.year, "-", end.year, ".csv", 
        sep = "")
    write.table(model.data[model.data$observation == TRUE, ], 
        file = filename, sep = ",", row.names = F, col.names = F)
    model.data
  }



cleanEx()
nameEx("prepare.effdis.data")
### * prepare.effdis.data

flush(stderr()); flush(stdout())

### Name: prepare.effdis.data
### Title: Clean up the EFFDIS data
### Aliases: prepare.effdis.data
### Keywords: ~kwd1 ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (input = data) 
{
    input <- input[input$eff1type != "--", ]
    input <- input[input$catchunit != "--", ]
    input <- input[input$eff1type != "--", ]
    input$catchunit <- as.character(input$catchunit)
    input$dsettype <- as.character(input$dsettype)
    input$flagname <- as.character(input$flagname)
    input$fleetcode <- as.character(input$fleetcode)
    input$geargrpcode <- as.character(input$geargrpcode)
    input$region <- as.character(input$region)
    input$eff1type <- as.character(input$eff1type)
    input
  }



cleanEx()
nameEx("spatial.coverage.by.year.task2")
### * spatial.coverage.by.year.task2

flush(stderr()); flush(stdout())

### Name: spatial.coverage.by.year.task2
### Title: Function to plot the spatial distribution of EFFDIS data each
###   year
### Aliases: spatial.coverage.by.year.task2
### Keywords: ~kwd1 ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (tdata = t2ce, start.year = 1950, end.year = 2010, which.region = "AT", 
    which.gear = "LL", which.flag = "EU.Portugal") 
{
    fdata <- tdata[tdata$flagname == which.flag & tdata$geargrpcode == 
        which.gear & tdata$region == which.region, ]
    cl <- 1.5
    ys <- start.year:end.year
    ly <- length(ys)
    for (i in min(ys, na.rm = T):max(ys, na.rm = T)) {
        dat <- fdata[fdata$year == i, ]
        if (length(dat[, 1]) == 0) {
            plot(fdata$longitude, fdata$latitude, type = "n", 
                xaxt = "n", yaxt = "n", ylim = range(tdata$latitude, 
                  na.rm = T), xlim = range(tdata$longitude, na.rm = T))
            map("world", col = "green", fill = T, add = T)
            title(i, cex.main = cl)
        }
        else {
            plot(dat$longitude, dat$latitude, type = "n", xaxt = "n", 
                yaxt = "n", ylim = range(tdata$latitude, na.rm = T), 
                xlim = range(tdata$longitude, na.rm = T))
            points(dat$longitude, dat$latitude, pch = ".", col = "red")
            map("world", add = T, col = "green", fill = T)
            title(i, cex.main = cl)
        }
        mtext(side = 3, outer = T, paste(which.flag, which.gear, 
            sep = " - "))
    }
  }



cleanEx()
nameEx("three.d.catch.by.year")
### * three.d.catch.by.year

flush(stderr()); flush(stdout())

### Name: three.d.catch.by.year
### Title: Function to plot spatial distribution of EFFDIS catch data
### Aliases: three.d.catch.by.year
### Keywords: ~kwd1 ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (tdata = task2.lf, what.gear = "LL", what.year = 2005, 
    gridx = 5, gridy = 5, what.species = "alb", what.flag = "All", 
    catchunit = "kg", scaling.f = 1e+06) 
{
    if (what.flag == "All") {
        tdata1 <- tdata[tdata$geargrpcode == what.gear & tdata$month < 
            13 & tdata$year == what.year & tdata$species == what.species & 
            tdata$catchunit == catchunit, ]
    }
    else {
        tdata1 <- tdata[tdata$geargrpcode == what.gear & tdata$month < 
            13 & tdata$year == what.year & tdata$species == what.species & 
            tdata$flagname == what.flag & tdata$catchunit == 
            catchunit, ]
    }
    dd <- dim(tdata1)
    ulocs <- length(unique(tdata1$longitude))
    if (dd[1] < 1 | ulocs < 6) {
        print("No Data")
    }
    else {
        coords <- SpatialPointsDataFrame(cbind(x = an(ac(tdata1$longitude)), 
            y = an(ac(tdata1$latitude))), data = tdata1[, c(4, 
            5)])
        geogWGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
        coords@proj4string <- geogWGS84
        resx <- gridx
        resy <- gridy
        cl <- 1.1
        ca <- 1
        fonts <- 2
        xl <- list(label = "Longitude", font = fonts, cex = cl)
        yl <- list(label = "Latitude", font = fonts, cex = cl)
        zl <- list(font = fonts, cex = cl)
        colintens <- brewer.pal(6, "YlOrRd")
        colland <- brewer.pal(9, "PiYG")[8]
        colgrey <- brewer.pal(9, "Greys")
        figtype <- "tiff"
        parmar <- rep(2, 4)
        paroma <- (c(6, 6, 2, 2) + 0.1)
        reso <- 1
        bbox <- bbox(coords)
        spatBound <- list(xrange = c(floor(range(bbox["x", ])[1]), 
            ceiling(range(bbox["x", ])[2])), yrange = c(floor(range(bbox["y", 
            ])[1]), ceiling(range(bbox["y", ])[2])))
        grd <- createGrid(spatBound$x, spatBound$y, resx, resy, 
            type = "SpatialGridDataFrame", exactBorder = T)
        grd@proj4string <- geogWGS84
        grd@data[] <- 0
        idx <- over(as(coords, "SpatialPoints"), as(grd, "SpatialGrid"))
        tdata1$gridID <- idx
        grd@data[names(table(idx)), 1] <- aggregate(tdata1$measured_catch, 
            by = list(tdata1$gridID), FUN = sum, na.rm = T)$x
        rr <- range(grd@data[an(names(table(idx))), 1])
        cutbreaksval <- list(ALL = c(-1, 0, 10, 25, 50, 100, 
            150, 200))
        legval <- list(ALL = c("0", "1 <= 10", "10 <= 25", "25 <= 50", 
            "50 <= 100", "100 <= 200", "200 <= 400"))
        valdiv <- scaling.f
        unitval <- paste("x", valdiv, catchunit)
        plot(1, 1, col = "white", xlim = spatBound$xrange, ylim = spatBound$yrange, 
            xlab = "", ylab = "", xaxt = "n", yaxt = "n", las = 1, 
            cex.lab = xl$cex, font = xl$font, asp = 1/lonLatRatio(mean(spatBound$xrange), 
                mean(spatBound$yrange)))
        coordGrd <- coordinates(grd)[an(names(table(idx))), ]
        grdPols <- lonLat2SpatialPolygons(lst = lapply(as.list(1:nrow(coordGrd)), 
            function(x) {
                data.frame(SI_LONG = c(coordGrd[x, "s1"] - resx/2, 
                  rep(coordGrd[x, "s1"] + resx/2, 2), coordGrd[x, 
                    "s1"] - resx/2), SI_LATI = c(rep(coordGrd[x, 
                  "s2"] - resy/2, 2), rep(coordGrd[x, "s2"] + 
                  resy/2, 2)))
            }))
        cols <- c("white", colintens)[cut(grd@data[an(names(table(idx))), 
            1]/valdiv, breaks = cutbreaksval$ALL)]
        plot(grdPols, col = cols, add = T, border = "transparent")
        map("world", resolution = 1, add = T, fill = TRUE, col = colland)
        map.axes()
        legend(x = "topright", fill = c("white", colintens), 
            legend = legval$ALL, bg = "white", title = unitval, 
            box.lty = 1)
        title(main = paste(what.flag, what.year, what.gear, what.species, 
            catchunit), outer = F, cex = cl)
        grdPolsDF <- as(grdPols, "SpatialPolygonsDataFrame")
        grdPolsDF@data <- data.frame(value = grd@data[an(names(table(idx))), 
            1], color = cols)
        proj4string(grdPolsDF) <- CRS("+proj=longlat +ellps=WGS84")
    }
  }



cleanEx()
nameEx("three.d.effort.by.year")
### * three.d.effort.by.year

flush(stderr()); flush(stdout())

### Name: three.d.effort.by.year
### Title: Function to plot spatial distribution of EFFDIS catch data
### Aliases: three.d.effort.by.year
### Keywords: ~kwd1 ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (tdata = task2.lf, what.gear = "LL", what.year = 2005, 
    gridx = 5, gridy = 5, effort.type = "NO.HOOKS", what.flag = "All", 
    scaling.f = 1e+06) 
{
    n0 <- tdata[tdata$dsettype == "n-", ]
    nw <- tdata[tdata$dsettype == "nw", ]
    mm <- duplicated(nw[, -9])
    nw <- nw[mm == TRUE, ]
    w0 <- tdata[tdata$dsettype == "-w", ]
    tdata1 <- rbind(n0, nw, w0)
    tdata1$flagname <- as.character(tdata1$flagname)
    if (what.flag == "All") {
        tdata2 <- tdata1[tdata1$month < 13 & tdata1$year == what.year & 
            tdata1$eff1type == effort.type & tdata1$geargrpcode == 
            what.gear, ]
    }
    if (what.flag != "All") {
        tdata2 <- tdata1[tdata1$month < 13 & tdata1$year == what.year & 
            tdata1$eff1type == effort.type & tdata1$flagname == 
            what.flag & tdata1$geargrpcode == what.gear, ]
    }
    dd <- dim(tdata2)
    ulocs <- length(unique(tdata2$longitude))
    if (dd[1] < 1 | ulocs < 6) {
    }
    else {
        coords <- SpatialPointsDataFrame(cbind(x = an(ac(tdata2$longitude)), 
            y = an(ac(tdata2$latitude))), data = tdata2[, c(4, 
            5)])
        geogWGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
        coords@proj4string <- geogWGS84
        resx <- gridx
        resy <- gridy
        cl <- 0.8
        ca <- 0.8
        fonts <- 2
        xl <- list(label = "Longitude", font = fonts, cex = cl)
        yl <- list(label = "Latitude", font = fonts, cex = cl)
        zl <- list(font = fonts, cex = cl)
        colintens <- brewer.pal(6, "YlOrRd")
        colland <- brewer.pal(9, "PiYG")[8]
        colgrey <- brewer.pal(9, "Greys")
        figtype <- "tiff"
        parmar <- rep(2, 4)
        paroma <- (c(6, 6, 2, 2) + 0.1)
        reso <- 1
        bbox <- bbox(coords)
        spatBound <- list(xrange = c(floor(range(bbox["x", ])[1]), 
            ceiling(range(bbox["x", ])[2])), yrange = c(floor(range(bbox["y", 
            ])[1]), ceiling(range(bbox["y", ])[2])))
        grd <- createGrid(spatBound$x, spatBound$y, resx, resy, 
            type = "SpatialGridDataFrame", exactBorder = T)
        grd@proj4string <- geogWGS84
        grd@data[] <- 0
        idx <- over(as(coords, "SpatialPoints"), as(grd, "SpatialGrid"))
        tdata2$gridID <- idx
        grd@data[names(table(idx)), 1] <- aggregate(tdata2$eff1, 
            by = list(tdata2$gridID), FUN = sum, na.rm = T)$x
        rr <- range(grd@data[an(names(table(idx))), 1])
        cutbreaksval <- list(ALL = c(-1, 0, 10, 25, 50, 100, 
            150, 200))
        legval <- list(ALL = c("0", "1 <= 10", "10 <= 25", "25 <= 50", 
            "50 <= 100", "100 <= 200", "200 <= 400"))
        valdiv <- scaling.f
        unitval <- paste("x", valdiv, "effort units")
        plot(1, 1, col = "white", xlim = spatBound$xrange, ylim = spatBound$yrange, 
            xlab = "", ylab = "", xaxt = "n", yaxt = "n", las = 1, 
            cex.lab = xl$cex, font = xl$font, asp = 1/lonLatRatio(mean(spatBound$xrange), 
                mean(spatBound$yrange)))
        coordGrd <- coordinates(grd)[an(names(table(idx))), ]
        grdPols <- lonLat2SpatialPolygons(lst = lapply(as.list(1:nrow(coordGrd)), 
            function(x) {
                data.frame(SI_LONG = c(coordGrd[x, "s1"] - resx/2, 
                  rep(coordGrd[x, "s1"] + resx/2, 2), coordGrd[x, 
                    "s1"] - resx/2), SI_LATI = c(rep(coordGrd[x, 
                  "s2"] - resy/2, 2), rep(coordGrd[x, "s2"] + 
                  resy/2, 2)))
            }))
        cols <- c("white", colintens)[cut(grd@data[an(names(table(idx))), 
            1]/valdiv, breaks = cutbreaksval$ALL)]
        plot(grdPols, col = cols, add = T, border = "transparent")
        map("world", resolution = 1, add = T, fill = TRUE, col = colland)
        map.axes()
        legend(x = "topright", fill = c("white", colintens), 
            legend = legval$ALL, bg = "white", title = unitval, 
            box.lty = 1)
        title(main = paste(what.flag, what.year, what.gear, effort.type), 
            outer = F, cex = cl)
        grdPolsDF <- as(grdPols, "SpatialPolygonsDataFrame")
        grdPolsDF@data <- data.frame(value = grd@data[an(names(table(idx))), 
            1], color = cols)
        proj4string(grdPolsDF) <- CRS("+proj=longlat +ellps=WGS84")
    }
  }



cleanEx()
nameEx("trend")
### * trend

flush(stderr()); flush(stdout())

### Name: trend
### Title: Function to calculate absolute time from month and year
### Aliases: trend
### Keywords: ~kwd1 ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (year, month, start.year = 1958) 
{
    nyear = year - start.year
    trend = month + nyear * 12
    trend
  }



cleanEx()
nameEx("yr.month.coverage.task2")
### * yr.month.coverage.task2

flush(stderr()); flush(stdout())

### Name: yr.month.coverage.task2
### Title: Function to plot data distribution as a function of year and
###   month
### Aliases: yr.month.coverage.task2
### Keywords: ~kwd1 ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (tdata = t2ce, start.year = 1950, end.year = 2010, which.gear = "LL", 
    which.region = "AT", which.flag = "EU.Portugal") 
{
    n0 <- tdata[tdata$dsettype == "n-", ]
    nw <- tdata[tdata$dsettype == "nw", ]
    mm <- duplicated(nw[, -9])
    nw <- nw[mm == TRUE, ]
    w0 <- tdata[tdata$dsettype == "-w", ]
    tdata1 <- rbind(n0, nw, w0)
    tdata1 <- tdata[tdata$month < 13, ]
    fdata <- tdata1[tdata1$flagname == which.flag & tdata1$geargrpcode == 
        which.gear & tdata1$region == which.region, ]
    dd <- dim(fdata)
    if (dd[1] == 0) {
    }
    else {
        fmat <- matrix(NA, length(1950:2015), 12)
        dimnames(fmat) <- list(c(1950:2015), 1:12)
        ymc <- table(fdata$year, fdata$month)
        dimnames(ymc)[[1]] <- sort(unique(fdata$year))
        dimnames(ymc)[[2]] <- month.abb
        mm <- match(dimnames(ymc)[[1]], dimnames(fmat)[[1]])
        fmat[mm, ] <- ymc
        image(1950:2015, 1:12, fmat, xaxt = "n", yaxt = "n", 
            xlab = "", ylab = "", col = terrain.colors(100), 
            xlim = c(start.year, end.year), ylim = range(fdata$month, 
                na.rm = T))
        contour(1950:2015, 1:12, fmat, add = T)
        axis(side = 1, at = start.year:end.year, label = as.character(start.year:end.year))
        ms <- range(fdata$month, na.rm = T)
        axis(side = 2, at = ms[1]:ms[2], label = month.abb[ms[1]:ms[2]])
        title(paste(which.flag, which.gear, sep = " - "))
    }
  }



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
