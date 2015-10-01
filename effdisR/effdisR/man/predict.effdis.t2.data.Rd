\name{predict.effdis.t2.data}
\alias{predict.effdis.t2.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to create comprehensive space-time grid and predict data from the GAMs
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
predict.effdis.t2.data(cmod = mods, effmod = emod, grid.res = 5, start.year = 1995, end.year = 2010, which.flag = "All", which.gear = "LL")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cmod}{
%%     ~~Describe \code{cmod} here~~
}
  \item{effmod}{
%%     ~~Describe \code{effmod} here~~
}
  \item{grid.res}{
%%     ~~Describe \code{grid.res} here~~
}
  \item{start.year}{
%%     ~~Describe \code{start.year} here~~
}
  \item{end.year}{
%%     ~~Describe \code{end.year} here~~
}
  \item{which.flag}{
%%     ~~Describe \code{which.flag} here~~
}
  \item{which.gear}{
%%     ~~Describe \code{which.gear} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
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
    prob[ngrd$which.ocean \%in\% c("land", "med", "pac")] <- NA
    measured_catch[ngrd$which.ocean \%in\% c("land", "med", "pac")] <- NA
    eff[ngrd$which.ocean \%in\% c("land", "med", "pac")] <- NA
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
