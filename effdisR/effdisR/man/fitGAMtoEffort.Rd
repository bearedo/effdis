\name{fitGAMtoEffort}
\alias{fitGAMtoEffort}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fitGAMtoEffort(input = lllf, which.flag = "Japan", which.effort = "NO.HOOKS", start.year = 1950, end.year = 2010, kk = 6)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
%%     ~~Describe \code{input} here~~
}
  \item{which.flag}{
%%     ~~Describe \code{which.flag} here~~
}
  \item{which.effort}{
%%     ~~Describe \code{which.effort} here~~
}
  \item{start.year}{
%%     ~~Describe \code{start.year} here~~
}
  \item{end.year}{
%%     ~~Describe \code{end.year} here~~
}
  \item{kk}{
%%     ~~Describe \code{kk} here~~
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
