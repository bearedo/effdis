\name{model.nos.kgs}
\alias{model.nos.kgs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to fit linear model (kgs as a function of numbers) for those flags that send both
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
model.nos.kgs(input = pslf, which.gear = "LL")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
Task II data in the long format
}
 
  \item{which.gear}{
Select gear, e.g. LL = longline
}
}
\details{
The function models weight as a linear function of numbers, species and trend. It uses step to find the most adequate model.
}
\value{
A linear model object.
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

#bm <- model.nos.kgs(input=lllf,which.gear='LL')

#lllf <- kgs.from.nos(lllf) # for those fleets that supply only number

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
