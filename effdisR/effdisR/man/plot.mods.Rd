\name{plot.mods}
\alias{plot.mods}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to plot output of fitted GAMs
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.mods(input = bft.aa, cmod = bft, what.year = 1995, what.month = 1, grid.res = 5, what.value = "prob", what.gear = "PS", plot.samples.only = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
%%     ~~Describe \code{input} here~~
}
  \item{cmod}{
%%     ~~Describe \code{cmod} here~~
}
  \item{what.year}{
%%     ~~Describe \code{what.year} here~~
}
  \item{what.month}{
%%     ~~Describe \code{what.month} here~~
}
  \item{grid.res}{
%%     ~~Describe \code{grid.res} here~~
}
  \item{what.value}{
%%     ~~Describe \code{what.value} here~~
}
  \item{what.gear}{
%%     ~~Describe \code{what.gear} here~~
}
  \item{plot.samples.only}{
%%     ~~Describe \code{plot.samples.only} here~~
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
function (input = bft.aa, cmod = bft, what.year = 1995, what.month = 1, 
    grid.res = 5, what.value = "prob", what.gear = "PS", plot.samples.only = TRUE) 
{
    min.lat <- min(cmod$pmod.data$latitude)
    max.lat <- max(cmod$pmod.data$latitude)
    min.lon <- min(cmod$pmod.data$longitude)
    max.lon <- max(cmod$pmod.data$longitude)
    what.species <- as.character(input$species[1])
    lonnie <- seq(min.lon, max.lon, by = grid.res)
    lattie <- seq(min.lat, max.lat, by = grid.res)
    lo <- length(lonnie)
    la <- length(lattie)
    if (what.value == "prob") {
        if (plot.samples.only == TRUE) {
            input[, what.value][input$observation == F] <- NA
        }
        image(lonnie, lattie, matrix(input[, what.value][input$year == 
            what.year & input$month == what.month], lo, la), 
            col = topo.colors(100))
        contour(lonnie, lattie, matrix(input[, what.value][input$year == 
            what.year & input$month == what.month], lo, la), 
            add = T)
        map("worldHires", add = T, fill = T)
        title(paste(toupper(what.species), "-", what.value, month.abb[what.month], 
            what.year, what.gear))
        w0 <- (1:length(cmod$pmod.data[, 1]))[cmod$pmod.data$year == 
            what.year & cmod$pmod.data$month == what.month]
        points(cmod$pmod.data$longitude[w0], cmod$pmod.data$latitude[w0], 
            pch = ".")
    }
    else {
        if (plot.samples.only == TRUE) {
            input[, what.value][input$observation == F] <- NA
        }
        image(lonnie, lattie, matrix(log(input[, what.value])[input$year == 
            what.year & input$month == what.month], lo, la), 
            col = topo.colors(100))
        contour(lonnie, lattie, matrix(log(input[, what.value])[input$year == 
            what.year & input$month == what.month], lo, la), 
            add = T)
        map("worldHires", add = T, fill = T)
        title(paste(toupper(what.species), "-", what.value, month.abb[what.month], 
            what.year, what.gear))
        w0 <- (1:length(cmod$pmod.data$longitude))[cmod$pmod.data$year == 
            what.year & cmod$pmod.data$month == what.month]
        points(cmod$pmod.data$longitude[w0], cmod$pmod.data$latitude[w0], 
            pch = ".")
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
