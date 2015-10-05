\name{fitGAMtoEffort}
\alias{fitGAMtoEffort}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to fit generalised additive model from the quasipoisson family to the effort data
}
\description{
The function extracts a relevant subset of the Task II data and models the effort data selected (e.g. NO.HOOKS) as a function of location (longitude and latitude) and time (trend and season). Seasonality is modeled using harmonics. 
}
\usage{
fitGAMtoEffort(input = lllf, which.flag = "Japan", which.effort = "NO.HOOKS", start.year = 1950, end.year = 2010, kk = 6)
}

\arguments{
  \item{input}{
  Task II data in the long format
}
  \item{which.flag}{
  Select which flag state, e.g. Japan
}
  \item{which.effort}{
Select effort type, e.g. NO.HOOKS
}
  \item{start.year}{
Select from which year you want to start
}
  \item{end.year}{
  Select from which year you want to end
}
  \item{kk}{
Select smoothing parameter. Default is 6. 
}
}
\details{
The harmonic regression is set up according to methods described in Introductory Time Series with R, 
http://www.springer.com/us/book/9780387886978
%%  ~~ If necessary, more details than the description above ~~
}
\value{

\item{emod}{Model object from the quasipoisson GAM}
 \item{pmod.data }{Data used in the quasipoisson GAM}
}
\references{
https://cran.r-project.org/web/packages/mgcv/index.html
}
\author{
Doug Beare
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
