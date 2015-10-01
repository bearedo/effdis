\name{get.effdis.t2.data}
\alias{get.effdis.t2.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to get the Task II data from the postgreSQL database on the ICCAT cloud server
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get.effdis.t2.data(which.dsn = "effdis-tuna-cc1", which.gear = "LL", which.region = "AT", which.flag = "All", which.dsettype = "-w")
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
  \item{which.dsettype}{
%%     ~~Describe \code{which.dsettype} here~~
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
    which.flag = "All", which.dsettype = "-w") 
{
    chan <- odbcConnect(which.dsn, case = "postgresql", believeNRows = FALSE)
    if (which.flag == "All") {
        query <- paste0("SELECT yearc AS year, trend, timeperiodid AS month, flagname, fleetcode,region, squaretypecode,\ngeargrpcode,longitude,latitude, catchunit, dsettype, eff1, eff1type,\nalb,bft,bet,skj,yft,swo,bum,sai,whm,totsp9\nFROM t2ce\nWHERE region =", 
            "'", which.region, "'", "AND timeperiodid < 13 AND geargrpcode=", 
            "'", which.gear, "'", "AND dsettype=", "'", which.dsettype, 
            "'")
    }
    else {
        query <- paste0("SELECT yearc AS year, trend, timeperiodid AS month, flagname, fleetcode,region,squaretypecode,\ngeargrpcode,longitude,latitude, catchunit, dsettype, eff1, eff1type,\nalb,bft,bet,skj,yft,swo,bum,sai,whm,totsp9\n\nFROM t2ce\nWHERE region =", 
            "'", which.region, "'", "AND timeperiodid < 13 AND geargrpcode=", 
            "'", which.gear, "'", "AND flagname=", "'", which.flag, 
            "'", "AND dsettype=", "'", which.dsettype, "'")
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
