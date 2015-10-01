\name{prepare.effdis.data}
\alias{prepare.effdis.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Clean up the EFFDIS data
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
prepare.effdis.data(input = data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
%%     ~~Describe \code{input} here~~
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (input = data) 
{
    input <- input[input$eff1type != "--", ]
    input <- input[input$catchunit != "--", ]
    input <- input[input$eff1type != "--", ]
    input$catchunit <- as.character(input$catchunit)
    input$dsettype <- as.character(input$dsettype)
    input$flagname <- as.character(input$flagname)
    input$fleetcode <- as.character(input$fleetcode)
    input$geargrpcode <- as.character(input$geargrpcode)
    input$region <- as.character(input$region)
    input$eff1type <- as.character(input$eff1type)
    input
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
