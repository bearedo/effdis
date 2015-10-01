\name{three.d.effort.by.year}
\alias{three.d.effort.by.year}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
three.d.effort.by.year(tdata = task2.lf, what.gear = "LL", what.year = 2005, gridx = 5, gridy = 5, effort.type = "NO.HOOKS", what.flag = "All", scaling.f = 1e+06)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tdata}{
%%     ~~Describe \code{tdata} here~~
}
  \item{what.gear}{
%%     ~~Describe \code{what.gear} here~~
}
  \item{what.year}{
%%     ~~Describe \code{what.year} here~~
}
  \item{gridx}{
%%     ~~Describe \code{gridx} here~~
}
  \item{gridy}{
%%     ~~Describe \code{gridy} here~~
}
  \item{effort.type}{
%%     ~~Describe \code{effort.type} here~~
}
  \item{what.flag}{
%%     ~~Describe \code{what.flag} here~~
}
  \item{scaling.f}{
%%     ~~Describe \code{scaling.f} here~~
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
