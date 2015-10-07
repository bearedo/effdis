\name{model.nos.kgs}
\alias{model.nos.kgs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to fit linear model (kgs as a function of numbers) for those flags that send both numbers of fish and their total weight
}
\description{
There is some variation in how different flag nations send in their data to the ICCAT secretariat. Some send data as numbers of fish caught, some as total weight of fish caught and others send both. This function takes out countries that send both numbers and weights and uses those relationships to impute weights for flag nations that send in numbers only.
}
\usage{
model.nos.kgs(input = pslf, which.gear = "LL")
}
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
