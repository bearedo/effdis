\name{three.d.catch.by.year}
\alias{three.d.catch.by.year}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to plot spatial distribution of EFFDIS Task II catch data for a selected combination of year, gear, flag nation and species
}
\description{
Takes the Task II data in the long format, builds a grid and sums the catches within each grid cell for a whole year and plots the output on a map.
}
\usage{
three.d.catch.by.year(tdata = task2.lf, which.gear = "LL", which.year = 2005, gridx = 5, gridy = 5, which.species = "alb", which.flag = "All", catchunit = "kg", scaling.f = 1e+06)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tdata}{
Task II data in the long format
}
  \item{which.gear}{
Select gear to plot
}
  \item{which.year}{
Select year to plot
}
  \item{gridx}{
Grid resolution, longitude
}
  \item{gridy}{
Grid resolution, latitude
}
  \item{which.species}{
Select species to plot, e.g. alb
}
  \item{which.flag}{
Select flag nation to plot
}
  \item{catchunit}{
Select catchunit to plot
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


#three.d.catch.by.year(tdata=lllf,scaling.f=100)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
