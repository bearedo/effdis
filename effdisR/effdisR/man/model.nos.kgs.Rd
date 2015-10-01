\name{model.nos.kgs}
\alias{model.nos.kgs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
model.nos.kgs(input = pslf, which.dsn = "effdis-local", which.gear = "LL")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
%%     ~~Describe \code{input} here~~
}
  \item{which.dsn}{
%%     ~~Describe \code{which.dsn} here~~
}
  \item{which.gear}{
%%     ~~Describe \code{which.gear} here~~
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
function (input = pslf, which.dsn = "effdis-local", which.gear = "LL") 
{
    pp0 <- aggregate(list(measured_catch = input$measured_catch), 
        by = list(trend = input$trend, month = input$month, flagname = input$flagname, 
            catchunit = input$catchunit, species = input$species), 
        sum, na.rm = T)
    pp0_nr <- pp0[pp0$catchunit == "nr", ]
    dimnames(pp0_nr)[[2]][6] <- "measured_catch_nr"
    pp0_kg <- pp0[pp0$catchunit == "kg", ]
    dimnames(pp0_kg)[[2]][6] <- "measured_catch_kg"
    pp0_kg_nr <- merge(pp0_kg[, -4], pp0_nr[, -4])
    library(lattice)
    xyplot(log(measured_catch_kg) ~ log(measured_catch_nr), groups = flagname, 
        data = pp0_kg_nr[pp0_kg_nr$species == "bft", ])
    xyplot(log(measured_catch_kg) ~ log(measured_catch_nr) | 
        species, auto.key = TRUE, groups = flagname, data = pp0_kg_nr)
    pp0_kg_nr$lnr <- log(pp0_kg_nr$measured_catch_nr)
    pp0_kg_nr$lkg <- log(pp0_kg_nr$measured_catch_kg)
    pp0_kg_nr$lnr[pp0_kg_nr$lnr == "-Inf"] <- NA
    pp0_kg_nr$lkg[pp0_kg_nr$lkg == "-Inf"] <- NA
    m1 <- lm(lkg ~ lnr, data = pp0_kg_nr, na.action = "na.omit")
    m2 <- lm(lkg ~ lnr + trend, data = pp0_kg_nr, na.action = "na.omit")
    m3 <- lm(lkg ~ lnr + trend + species, data = pp0_kg_nr, na.action = "na.omit")
    best.model <- step(m3, direction = "both")
    print(summary(best.model))
    best.model
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
