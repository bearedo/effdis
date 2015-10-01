\name{add.covariates}
\alias{add.covariates}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
add.covariates(input = t2ce_lf_ll, what.dsn = "effdis-tuna-cc1")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
%%     ~~Describe \code{input} here~~
}
  \item{what.dsn}{
%%     ~~Describe \code{what.dsn} here~~
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
function (input = t2ce_lf_ll, what.dsn = "effdis-tuna-cc1") 
{
    chan <- odbcConnect(what.dsn, case = "postgresql", believeNRows = FALSE)
    t2ce_distinct_locations_covariates <- sqlQuery(chan, "SELECT *, ST_AsText(the_geom_4326) AS the_point from t2ce_distinct_locations_covariates;")
    tdlc <- SpatialPointsDataFrame(cbind(x = an(ac(t2ce_distinct_locations_covariates$longitude)), 
        y = an(ac(t2ce_distinct_locations_covariates$lat))), 
        data = t2ce_distinct_locations_covariates)
    geogWGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    tdlc@proj4string <- geogWGS84
    input$depth_m <- tdlc@data$depth_m[match(paste(input$longitude, 
        input$latitude), paste(tdlc@data$longitude, tdlc@data$latitude))]
    input$m_ann_sst <- tdlc@data$m_ann_sst[match(paste(input$longitude, 
        input$latitude), paste(tdlc@data$longitude, tdlc@data$latitude))]
    input$aq_prim_pro <- tdlc@data$aq_prim_pro[match(paste(input$longitude, 
        input$latitude), paste(tdlc@data$longitude, tdlc@data$latitude))]
    input$m_ann_chla <- tdlc@data$m_ann_chla[match(paste(input$longitude, 
        input$latitude), paste(tdlc@data$longitude, tdlc@data$latitude))]
    out <- input
    out
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
