\name{add.covariates}
\alias{add.covariates}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to add depth, sst, chlorophyll a and primary production to a data frame }

\description{

This was set up to investigate whether it was worth using coveriates (other than latitude, longitude, trend, and month) such as bottom depth, sea-surface temperature etc. These covariates are climatological means from NOAA's websites.

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
add.covariates(input = t2ce_lf_ll, what.dsn = "effdis-tuna-cc1")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
  Task II data frame in the long format.
}
  \item{what.dsn}{
DSN for database.
}
}
\details{
depth_m - ocean depths from the SRTM30 Plus bathymetry dataset downloaded from MARSPEC (http://www.marspec.org/Modern_Data.html)
m_ann_sst - mean annual sst (2009-2013) from UNEP-WCMC (http://data.unep-wcmc.org/datasets/36)
m_ann_chla - mean annual chlorophyll-a concentration (2009-2013) from UNEP-WCMC (http://data.unep-wcmc.org/datasets/37)
aq_primpro - Aquamaps HCAF v4 primary productivity from SeaWiFS data from GMED (http://gmed.auckland.ac.nz/data.html#)
bo_ph - pH data from WOD 2009 from GMED (http://gmed.auckland.ac.nz/data.html#)
}
\value{
Output is a new data frame with columns added for depth, sea surface temperature, primary production and chlorophyll a concentration.

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Doug Beare and Jason Jon Benedict
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
require(RODBC)
#lln  <- get.effdis.t2.data(which.dsn='effdis-tuna-cc1',which.gear='LL',which.flag='Brasil',which.dsettype = 'n-') # Get data from cloud server
#require(reshape2)
#llnlf <- convert.to.long.format.t2(input = lln) # Convert to long format
#llnlf <- add.covariates(input=llnlf) # Add on covariates



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
