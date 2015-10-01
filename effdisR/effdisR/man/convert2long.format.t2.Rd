\name{convert2long.format.t2}
\alias{convert2long.format.t2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
convert2long.format.t2(input = t2ce)
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
