\name{get.effdis.t2.data}
\alias{get.effdis.t2.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to get the Task II data from the postgreSQL database on the ICCAT cloud server
}
\description{
Extracts Task I data from the PostgreSQL database (effdis) on the ICCAT cloud server
}
\usage{
get.effdis.t2.data(which.dsn = "effdis-tuna-cc1", which.gear = "LL", which.region = "AT", which.flag = "All", which.dsettype = "-w")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{which.dsn}{
Data source name, see details.
}
  \item{which.gear}{
Select gear type, e.g. PS=Purse seine
}
  \item{which.region}{
Select region, e.g. AT = Atantic
}
  \item{which.flag}{
Select flag naton, e.g. Japan
}
  \item{which.dsettype}{
Select dsettype. 'n-' = numbers, '-w' = weight in kgs, 'nw'= both numbers and weights collected.
}
}
\details{
which.dsn is a text string string telling R which datbase to connect to. You need to have the PostgreSQL drivers installed and the simplest way to do this is install PostgreSQL. In linux you will then need to edit the 
etc/odbc.ini file, see example odbc.ini below. Hence which.dsn = 'effdis-tuna-cc1'  
[ODBC Data Sources]  
effdis = effdis-tuna-cc1  
[effdis-tuna-cc1]  
Driver = /usr/lib/x86_64-linux-gnu/odbc/psqlodbcw.so  
Database = effdis  
Servername = 134.213.29.249  
Username = postgres  
Password = Postgres1  
Protocol = 8.2.5  
ReadOnly = 0  
[ODBC]  
InstallDir = /usr/lib  
}
\value{
Data frame of the Task II data extracted from the database.
}
\references{
http://www.r-bloggers.com/getting-started-with-postgresql-in-r/
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
#llnw <- get.effdis.t2.data(which.dsn='effdis-tuna-cc1',which.gear='LL',which.flag='All',which.dsettype = 'nw')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
