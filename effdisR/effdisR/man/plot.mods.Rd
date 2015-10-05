\name{plot.mods}
\alias{plot.mods}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to plot output of fitted GAMs
}
\description{
The function uses the predicted numbers from predict.effdis.t2.data and plots them on a map. The user can select to plot at locations for which there are data, plot.samples.only = T or interpolate over the entire area for which data have ever been available.
}
\usage{
plot.mods(input = bft.aa, cmod = bft, which.year = 1995, which.month = 1, grid.res = 5, which.value = "prob", which.gear = "PS", plot.samples.only = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
Predicted object output by predict.effdis.t2.data.
}
  \item{cmod}{
GAM model object from fit2stageGAMtoCatch.
}
  \item{which.year}{
Select year to plot
}
  \item{which.month}{
Select month to plot
}
  \item{grid.res}{
Select grid resolution.
}
  \item{which.value}{
Select value to plot, ie. probability, measured_catch or cpue.
}
  \item{which.gear}{
Select gear to plot
}
  \item{plot.samples.only}{
Select fully interpolated data or only locations where data were actually recorded.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Only a plot is returned
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
function (input = bft.aa, cmod = bft, which.year = 1995, which.month = 1, 
    grid.res = 5, which.value = "prob", which.gear = "PS", plot.samples.only = TRUE) 
{
    min.lat <- min(cmod$pmod.data$latitude)
    max.lat <- max(cmod$pmod.data$latitude)
    min.lon <- min(cmod$pmod.data$longitude)
    max.lon <- max(cmod$pmod.data$longitude)
    which.species <- as.character(input$species[1])
    lonnie <- seq(min.lon, max.lon, by = grid.res)
    lattie <- seq(min.lat, max.lat, by = grid.res)
    lo <- length(lonnie)
    la <- length(lattie)
    if (which.value == "prob") {
        if (plot.samples.only == TRUE) {
            input[, which.value][input$observation == F] <- NA
        }
        image(lonnie, lattie, matrix(input[, which.value][input$year == 
            which.year & input$month == which.month], lo, la), 
            col = topo.colors(100))
        contour(lonnie, lattie, matrix(input[, which.value][input$year == 
            which.year & input$month == which.month], lo, la), 
            add = T)
        map("worldHires", add = T, fill = T)
        title(paste(toupper(which.species), "-", which.value, month.abb[which.month], 
            which.year, which.gear))
        w0 <- (1:length(cmod$pmod.data[, 1]))[cmod$pmod.data$year == 
            which.year & cmod$pmod.data$month == which.month]
        points(cmod$pmod.data$longitude[w0], cmod$pmod.data$latitude[w0], 
            pch = ".")
    }
    else {
        if (plot.samples.only == TRUE) {
            input[, which.value][input$observation == F] <- NA
        }
        image(lonnie, lattie, matrix(log(input[, which.value])[input$year == 
            which.year & input$month == which.month], lo, la), 
            col = topo.colors(100))
        contour(lonnie, lattie, matrix(log(input[, which.value])[input$year == 
            which.year & input$month == which.month], lo, la), 
            add = T)
        map("worldHires", add = T, fill = T)
        title(paste(toupper(which.species), "-", which.value, month.abb[which.month], 
            which.year, which.gear))
        w0 <- (1:length(cmod$pmod.data$longitude))[cmod$pmod.data$year == 
            which.year & cmod$pmod.data$month == which.month]
        points(cmod$pmod.data$longitude[w0], cmod$pmod.data$latitude[w0], 
            pch = ".")
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
