\name{predict.effdis.t2.data}
\alias{predict.effdis.t2.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to create comprehensive space-time grid and predict data from the GAMs fitted to Task II data in previous steps.
}
\description{
This function first creates grid over which the models are predicted. It then takes the parameters from all 3 regression models fitted: Bernouilli (probability of catch), Gamma (positive component of catch) and Quasipoisson(the fishing effort) and predicts over the grid. Grid resolution can be set and there is an  option to produce predictions over the entire grid or only at points in space and time for which actual data are availabe.
}
\usage{
predict.effdis.t2.data(cmod = mods, effmod = emod, grid.res = 5, start.year = 1995, end.year = 2010, which.flag = "All", which.gear = "LL")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cmod}{
Model object from fit2stageGAMtoCatch
}
  \item{effmod}{
Model object from fitGAMtoEffort
}
  \item{grid.res}{
Select grid resolution in degrees of longitude and latitude
}
  \item{start.year}{
Select start year
}
  \item{end.year}{
Select end year
}
  \item{which.flag}{
Select flag nation
}
  \item{which.gear}{
Select gear
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The grid of permutations of the covariates together with predictions. It also writes out a text file.
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

# alb.aa <- predict.effdis.t2.data(cmod=alb, effmod=emod,grid.res=5,start.year=1970,end.year=2010,which.flag='Japan')

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
