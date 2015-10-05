\name{kgs.from.nos}
\alias{kgs.from.nos}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to estimate weight caught for countries that only record numbers
}
\description{
The function first takes out the data where only numbers have been recorded. It then uses the linear model estimates from the function model.nos.kgs which are estimated using those data for which both numbers and weights are recorded to predict weights for situations where numbers only exist. It then puts the data back together giving a data frame of catches in weights only
}
\usage{
kgs.from.nos(input = pslf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
  Task II data in the long format
}
}
\details{
Obviously there are caveats which should be taken into consideration. We are using data from countries which send catches recorded in both numbers and weights to make assumptions about countries that send in only numbers. 
}
\value{
Output is a data.frame containing catch data in weights inferred from flag nations that send in catches in both weights and numbers.
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
