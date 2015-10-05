\name{prepare.effdis.data}
\alias{prepare.effdis.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Clean up the EFFDIS data
}
\description{
Gets rid of 'odd' information in Task II data, e.g. '--' and converts factors to characters.
}
\usage{
prepare.effdis.data(input = data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
Data frame of Task II data.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Data frame of Task II data with '--' removed and factor columns converted to character.
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
##lllf<-prepare.effdis.data(input=lllf)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
