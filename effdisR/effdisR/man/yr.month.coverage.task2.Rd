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
Task II data in the short format
}
  \item{start.year}{
Select start year from which to plot
}
  \item{end.year}{
Select end year
}
  \item{which.gear}{
Select gear type to plot
}
  \item{which.region}{
Select region to plot,e.g. 'AT' = Atlantic
}
  \item{which.flag}{
Select flag nation to plot
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
#yr.month.coverage.task2(tdata=ll,which.gear='LL',
#              start.year=1960,end.year=2010,which.flag='Japan')
#yr.month.coverage.task2(tdata=ll,which.gear='LL',
#              start.year=1960,end.year=2010,which.flag='Chinese Taipei')
#yr.month.coverage.task2(tdata=ll,which.gear='LL',
 #             start.year=1960,end.year=2010,which.flag='Brasil')
#yr.month.coverage.task2(tdata=ll,which.gear='LL',
 #             start.year=1960,end.year=2010,which.flag='U.S.A.')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
