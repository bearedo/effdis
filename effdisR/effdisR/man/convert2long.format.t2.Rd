\name{convert2long.format.t2}
\alias{convert2long.format.t2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to convert short format to long format which is useful for regression modeling
}
\description{
Takes the task II data in the form t2ce, selects the most useful columns and converts them to the long-format. Instead of nine columns for each species there is now a single column for 'species'
}
\usage{
convert2long.format.t2(input = t2ce)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
Data frame of task II data in the short format, ie. t2ce from the PostgreSQL database
}
}
\details{
This is based on melt from reshape 2 and is simply handy for regression modeling and often it's easier to write SQL queries with data in the long-format
}
\value{
Returns a new data frame in the long format with column names as per input but all the species columns are named,'species' and the catches are named, 'measured_catch'
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
