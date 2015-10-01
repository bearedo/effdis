\name{find.ocean}
\alias{find.ocean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to determine which Ocean an observation is in
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
find.ocean(input = grd)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
%%     ~~Describe \code{input} here~~
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
    atlantic <- seas[seas@data$NAME \%in\% seas.polys[c(wo, wp, 
        ws, wv, wmm, gog)], ]
    med <- seas[seas@data$NAME \%in\% seas.polys[c(wi, wj, wk, 
        wl, wu, wz, wa, wzz, wxx)], ]
    pacific <- seas[seas@data$NAME \%in\% seas.polys[c(wpa)], ]
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
