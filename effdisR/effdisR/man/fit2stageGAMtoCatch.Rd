\name{fit2stageGAMtoCatch}
\alias{fit2stageGAMtoCatch}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to fit generalised additive models to the EFFDIS catch data in 2 stages. The first is a Bernoulli model the second is a Gamma model fitted to the positive part of the data
}
\description{
The function extracts a relevant subset of the Task II data. For the first model a binary variable is constructed where zero catch = 0 and positive catch = 1 which is modeled as a function of longitude, latitude, trend and season using a GAM (mgcv library) from the quasibinomial family.  In the second model the catches that = 0 are removed and the positive component is modeled using a GAM (mgcv library) from the Gamma family. In the call se

The seasonal component is estimated using harmonics.
}
\usage{
fit2stageGAMtoCatch(input = pslf, which.species = "bft", start.year = 1950, end.year = 2015, which.flag = "Japan", kk = 6)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
Task II data in the long format
}
  \item{which.species}{
Select which species to model, e.g. alb or skj
}
  \item{start.year}{
Select from which year you want to start
}
  \item{end.year}{
Select from which year you want to end
}
  \item{which.flag}{
Select which flag state, e.g. Japan
}
  \item{kk}{
Select smoothing parameter. Default is 6. 
}
}
\details{

}
\value{

  \item{pmod}{Model object from the binary GAM}
 \item{pmod.data }{Data used in the binary GAM fit}
\item{gmod}{Model object from the Gamma GAM fitted to positive data}
\item{gmod.data}{Data used in the Gamma GAM fitted to positive data}

}
\references{
https://cran.r-project.org/web/packages/mgcv/index.html
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

#alb <- fit2stageGAMtoCatch(input=lllf,which.flag='Japan',which.species='alb',start.year=1995,end.year=2010)


## The function is currently defined as
function (input = pslf, which.species = "bft", start.year = 1950, 
    end.year = 2015, which.flag = "Japan", kk = 6) 
{
    input <- input[input$species == which.species, ]
    input <- input[input$year >= start.year & input$year <= end.year, 
        ]
    if (which.flag == "All") {
        input <- input
        print("Modeling all data")
    }
    else {
        input <- input[input$flagname == which.flag, ]
    }
    bin <- ifelse(input$measured_catch == 0, 0, 1)
    input$bin <- bin
    tbin <- table(input$bin)
    if (sum(bin) >= 10) {
        input <- input[input$catchunit == "kg", ]
        input$lmeasured_catch <- log(input$measured_catch + 1)
        bbs <- "cr"
        dat0 <- input
        ss = cc = matrix(NA, nr = length(dat0[, 1]), nc = 6)
        for (i in 1:6) {
            cc[, i] <- cos(2 * pi * i * dat0$trend/12)
            ss[, i] <- sin(2 * pi * i * dat0$trend/12)
        }
        ss <- ss[, -6]
        dat1 <- cbind(dat0, ss, cc)
        dd <- dim(dat0)
        dimnames(dat1)[[2]][(dd[2] + 1):(dim(dat1)[2])] <- c(paste("sin", 
            1:5, sep = ""), paste("cos", 1:6, sep = ""))
        input <- dat1
        b1 <- gam(bin ~ te(longitude, latitude, k = 6, bs = bbs) + 
            te(trend, k = 6, bs = bbs) + sin1 + cos1 + sin2 + 
            cos2 + sin3 + cos3 + sin4 + cos4 + sin5 + cos5 + 
            cos6, data = input, family = quasibinomial(link = "logit"), 
            method = "REML", select = TRUE)
        input1 <- aggregate(list(measured_catch = input$measured_catch), 
            by = list(trend = input$trend, month = input$month, 
                longitude = input$longitude, latitude = input$latitude), 
            sum, na.rm = T)
        bin <- ifelse(input1$measured_catch == 0, 0, 1)
        input1$bin <- bin
        input2 <- input1[input1$bin == 1, ]
        dat0 <- input2
        ss = cc = matrix(NA, nr = length(dat0[, 1]), nc = 6)
        for (i in 1:6) {
            cc[, i] <- cos(2 * pi * i * dat0$trend/12)
            ss[, i] <- sin(2 * pi * i * dat0$trend/12)
        }
        ss <- ss[, -6]
        dat1 <- cbind(dat0, ss, cc)
        dd <- dim(dat0)
        dimnames(dat1)[[2]][(dd[2] + 1):(dim(dat1)[2])] <- c(paste("sin", 
            1:5, sep = ""), paste("cos", 1:6, sep = ""))
        input2 <- dat1
        g1 <- gam(measured_catch ~ te(longitude, latitude, k = kk, 
            bs = bbs) + te(trend, k = kk, bs = bbs) + sin1 + 
            cos1 + sin2 + cos2 + sin3 + cos3 + sin4 + cos4 + 
            sin5 + cos5 + cos6, family = Gamma(link = "log"), 
            method = "REML", select = TRUE, data = input2)
        gc(reset = T)
        mods <- list(pmod = b1, pmod.data = input, gmod = g1, 
            gmod.data = input2)
        mods
    }
    else {
        print("Insufficient data to support model")
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
