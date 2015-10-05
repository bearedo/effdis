\name{spatial.coverage.by.year.task2}
\alias{spatial.coverage.by.year.task2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function to plot the spatial distribution of EFFDIS Task II data each year
}
\description{
Plots Task II data on map
}
\usage{
spatial.coverage.by.year.task2(tdata = t2ce, start.year = 1950, end.year = 2010, which.region = "AT", which.gear = "LL", which.flag = "EU.Portugal")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tdata}{
EFFDIS Task II data in the short format
}
  \item{start.year}{
Select start year to plot
}
  \item{end.year}{
Select end year to plot
}
  \item{which.region}{
Select region to plot, e.g. 'AT' = Atlantic
}
  \item{which.gear}{
Select gear to plot, e.g. 'PS' = Purse seine
}
  \item{which.flag}{
Select flag nation to plot, e.g. China.P.R.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Output is a plot
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

#spatial.coverage.by.year.task2(tdata=t2ce,start.year=1960, end.year=1970, which.region = 'AT', which.gear='LL',which.flag='EU.Portugal')

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
