\name{yr.month.coverage.task2}
\alias{yr.month.coverage.task2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to plot data distribution as a function of year and month
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
yr.month.coverage.task2(tdata = t2ce, start.year = 1950, end.year = 2010, which.gear = "LL", which.region = "AT", which.flag = "EU.Portugal")
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
  \item{which.gear}{
%%     ~~Describe \code{which.gear} here~~
}
  \item{which.region}{
%%     ~~Describe \code{which.region} here~~
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
