\name{three.d.effort.by.year}
\alias{three.d.effort.by.year}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to plot spatial distribution of EFFDIS Task II effort data for a selected combination of year, gear, effort type and species
}
\description{
Takes the Task II data in the long format, builds a grid and sums the effort values within those grid cells for a whole year and plots the output on a map

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
three.d.effort.by.year(tdata = task2.lf, what.gear = "LL", what.year = 2005, gridx = 5, gridy = 5, effort.type = "NO.HOOKS", what.flag = "All", scaling.f = 1e+06)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tdata}{
Task II data in the long format
}
  \item{what.gear}{
Select gear to plot
}
  \item{what.year}{
Select year to plot
}
  \item{gridx}{
Grid resolution, longitude
}
  \item{gridy}{
Grid resolution, latitude
}
  \item{effort.type}{
Effort type to plot, e.g. NO.HOOKS
}
  \item{what.flag}{
Flag nation to plot
}
  \item{scaling.f}{
Scale for the legend and color table
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The function produces a plot
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
#three.d.effort.by.year()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
