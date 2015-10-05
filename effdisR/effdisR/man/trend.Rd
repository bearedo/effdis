\name{trend}
\alias{trend}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to calculate absolute time from month and year
}
\description{
Counts the number of timeperiods from a start year by month. E.g. there are 1 to 25 periods from January 1958 to February 1960. 
}
\usage{
trend(year, month, start.year = 1958)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{year}{
Vector of years
}
  \item{month}{
Vector of months
}
  \item{start.year}{
Year from when to start counting.
}
}
\details{
This function is used to create the trend vector in the GAM/time-series modeling
}
\value{
Vector of time steps
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





}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
