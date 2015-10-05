trend <- function(year,month,start.year=1958){
  #Function to calculate 'trend' vector for time-series:
  nyear=year-start.year
  trend=month+nyear*12
  trend
}
