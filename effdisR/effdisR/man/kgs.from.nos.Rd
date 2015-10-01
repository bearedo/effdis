\name{kgs.from.nos}
\alias{kgs.from.nos}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
kgs.from.nos(input = pslf)
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
function (input = pslf) 
{
    t2ce_lf_kg <- input[input$dsettype \%in\% c("-w", "nw"), ]
    t2ce_lf_kg <- t2ce_lf_kg[t2ce_lf_kg$catchunit != "--", ]
    t2ce_lf_nr <- input[input$dsettype == "n-", ]
    t2ce_lf_nr$measured_catch_nr <- t2ce_lf_nr$measured_catch
    dd <- dim(t2ce_lf_nr)
    if (dd[1] > 0) {
        t2ce_lf_nr$lnr <- log(t2ce_lf_nr$measured_catch_nr)
        aa <- exp(predict(bm, t2ce_lf_nr, type = "response"))
        ww <- (1:length(t2ce_lf_nr[, 1]))[t2ce_lf_nr$measured_catch == 
            0]
        aa[ww] <- 0
        t2ce_lf_nr$measured_catch <- aa
        t2ce_lf_nr$dsettype <- "-w"
        t2ce_lf_nr$catchunit <- "kg"
        t2ce_lf_nr <- t2ce_lf_nr[, -c(16, 17)]
        t2ce_lf <- rbind(t2ce_lf_kg, t2ce_lf_nr)
        t2ce_lf
    }
    else {
        print("All data are in kgs")
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
