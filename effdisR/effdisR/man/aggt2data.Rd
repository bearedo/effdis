\name{aggt2data}
\alias{aggt2data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to aggregate Task II data as an alternative to modeling}
\description{
The effdisR library allows user to model (statistically) the catch and effort data as functions of location and time. This function, however, allows the user to simply calculate the effort metrics directly from the raw data. To that end this function simply sums the Task II data over variables of location and time and the result can then be merged with Task I in exacly the same way as the  model output works
}
\usage{
aggt2data(input = lllf, what.flag = "Japan", what.effort = "NO.HOOKS")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
Task II data in the long format
}
  \item{what.flag}{
Select flag country, e.g. Japan
}
  \item{what.effort}{
  Select type of effort, e.g. NO.HOOKS
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

#totals <- agg2data(input=lllf,what.flag="Chinese Taipei", what.effort = "NO.HOOKS") # Total catches by location, year, month and species for selected flag country


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
