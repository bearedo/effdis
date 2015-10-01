\name{get.effdis.t1.data}
\alias{get.effdis.t1.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get.effdis.t1.data(which.dsn = "effdis-tuna-cc1", which.gear = "LL", which.region = "AT", which.flag = "All", which.datatype = "C")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{which.dsn}{
%%     ~~Describe \code{which.dsn} here~~
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
  \item{which.datatype}{
%%     ~~Describe \code{which.datatype} here~~
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
function (which.dsn = "effdis-tuna-cc1", which.gear = "LL", which.region = "AT", 
    which.flag = "All", which.datatype = "C") 
{
    chan <- odbcConnect(which.dsn, case = "postgresql", believeNRows = FALSE)
    if (which.flag == "All") {
        query <- paste0("SELECT *\nFROM t1det9sp\nWHERE region =", 
            "'", which.region, "'", "AND geargrp=", "'", which.gear, 
            "'", "AND datatype=", "'", which.datatype, "'")
    }
    else {
        query <- paste0("SELECT * \n\nFROM t1det9sp\nWHERE region =", 
            "'", which.region, "'", "AND geargrp=", "'", which.gear, 
            "'", "AND flag=", "'", which.flag, "'", "AND datatype=", 
            "'", which.datatype, "'")
    }
    out <- sqlQuery(chan, query)
    odbcClose(chan)
    out
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
