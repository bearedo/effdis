\name{spatial.coverage.by.year.task2}
\alias{spatial.coverage.by.year.task2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function to plot the spatial distribution of EFFDIS data each year
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
spatial.coverage.by.year.task2(tdata = t2ce, start.year = 1950, end.year = 2010, which.region = "AT", which.gear = "LL", which.flag = "EU.Portugal")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tdata}{
%%     ~~Describe \code{tdata} here~~
}
  \item{start.year}{
%%     ~~Describe \code{start.year} here~~
}
  \item{end.year}{
%%     ~~Describe \code{end.year} here~~
}
  \item{which.region}{
%%     ~~Describe \code{which.region} here~~
}
  \item{which.gear}{
%%     ~~Describe \code{which.gear} here~~
}
  \item{which.flag}{
%%     ~~Describe \code{which.flag} here~~
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
