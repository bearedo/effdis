\name{fitGAMtoEffort}
\alias{fitGAMtoEffort}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to fit generalised additive model from the quasipoisson family to the effort data
}
\description{
The function extracts a relevant subset of the Task II data and models the effort data selected (e.g. NO.HOOKS) as a function of location (longitude and latitude) and time (trend and season). Seasonality is modeled using harmonics. 
}
\usage{
fitGAMtoEffort(input = lllf, which.flag = "Japan", which.effort = "NO.HOOKS", start.year = 1950, end.year = 2010, kk = 6)
}

\arguments{
  \item{input}{
  Task II data in the long format
}
  \item{which.flag}{
  Select which flag state, e.g. Japan
}
  \item{which.effort}{
Select effort type, e.g. NO.HOOKS
}
  \item{start.year}{
Select from which year you want to start
}
  \item{end.year}{
  Select from which year you want to end
}
  \item{kk}{
Select smoothing parameter. Default is 6. 
}
}
\details{
The harmonic regression is set up according to methods described in Introductory Time Series with R, 
http://www.springer.com/us/book/9780387886978
%%  ~~ If necessary, more details than the description above ~~
}
\value{

\item{emod}{Model object from the quasipoisson GAM}
 \item{pmod.data }{Data used in the quasipoisson GAM}
}
\references{
https://cran.r-project.org/web/packages/mgcv/index.html
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

#emod <- fitGAMtoEffort(input=lllf,which.flag='Japan',which.effort='NO.HOOKS',start.year=1970,end.year=2010)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
