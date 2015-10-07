\name{plot.mods}
\alias{plot.mods}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to plot output of fitted GAMs
}
\description{
The function uses the predicted numbers from predict.effdis.t2.data and plots them on a map. The user can select to plot at locations for which there are data, plot.samples.only = T or interpolate over the entire area for which data have ever been available.
}
\usage{
plot.mods(input = bft.aa, cmod = bft, which.year = 1995, which.month = 1, grid.res = 5, which.value = "prob", which.gear = "PS", plot.samples.only = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
Predicted object output by predict.effdis.t2.data.
}
  \item{cmod}{
GAM model object from fit2stageGAMtoCatch.
}
  \item{which.year}{
Select year to plot
}
  \item{which.month}{
Select month to plot
}
  \item{grid.res}{
Select grid resolution.
}
  \item{which.value}{
Select value to plot, ie. probability, measured_catch or cpue.
}
  \item{which.gear}{
Select gear to plot
}
  \item{plot.samples.only}{
Select fully interpolated data or only locations where data were actually recorded.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A plot is returned
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
#plot.mods(input=j_bft.ll.pred,cmod=j_bft_ll,which.year = 1995,which.month=1,which.value = 'prob',grid.res=5,which.gear='LL')


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
