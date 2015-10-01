\name{aggt2data}
\alias{aggt2data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to aggregate Task II data as an alternative to modeling}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
aggt2data(input = lllf, what.flag = "Japan", what.effort = "NO.HOOKS")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
%%     ~~Describe \code{input} here~~
}
  \item{what.flag}{
%%     ~~Describe \code{what.flag} here~~
}
  \item{what.effort}{
%%     ~~Describe \code{what.effort} here~~
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
function (input = lllf, what.flag = "Japan", what.effort = "NO.HOOKS") 
{
    input_kgs <- input[input$catchunit == "kg" & input$eff1type == 
        what.effort & lllf$flagname == what.flag, ]
    allt2 <- aggregate(list(eff = input_kgs$eff1, measured_catch = input_kgs$measured_catch), 
        by = list(year = input_kgs$year, month = input_kgs$month, 
            longitude = input_kgs$longitude, latitude = input_kgs$latitude, 
            species = input_kgs$species, flagname = input_kgs$flagname), 
        sum, na.rm = T)
    allt2
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
