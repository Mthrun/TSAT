# tsobject = ConvertNumerical2TSobject(NumericVectorOrMatrix, Time, Frequency = "days")
#
# Description:
# Converts a numerical vector or matrix to a time series object of the stats package
#
# INPUT
# NumericVectorOrMatrix   [1:n,1:d] matrix or vector with n cases and d variables
# Time                    [1:n] Time object of class Date
# Frequency               either days, weeks, months or quarters or years
#
# OUTPUT 
# ts                      ts object with [1:n,1:d]                
#
#
# Author: MCT

ConvertNumerical2TSobject=function(NumericVectorOrMatrix,Time,Frequency='days'){
  # Create a daily Date object - helps my work on dates
  requireNamespace('lubridate')
  year=lubridate::year(min(Time))
  if(is.null(ncol(NumericVectorOrMatrix))){
    n=length(NumericVectorOrMatrix)
  }else{
    n=nrow(NumericVectorOrMatrix)
  }
  switch (Frequency,
          days = {
            inds <- seq(from = min(Time), to = max(Time), by = "day")
            if(length(inds)!=n) warning('"NumericVectorOrMatrix" with daily frequency has less or more entries than "Time" has dates.')

              TSobject <- ts(NumericVectorOrMatrix,     # random data
                       start = c(year, as.numeric(format(inds[1], "%j"))),
                       frequency = 365.25)
          },
          weeks = { 
            TSobject=ts(NumericVectorOrMatrix, frequency=365.25/7, start=lubridate::decimal_date(lubridate::ymd(min(Time))))
          },
          months = {
            TSobject=ts(NumericVectorOrMatrix, frequency=12, start=c(year,lubridate::month(min(Time))))
          },
          quarters = {
            TSobject=ts(NumericVectorOrMatrix, frequency=4, start=c(year,lubridate::quarter(min(Time))))
          },
          years = {
            TSobject=ts(NumericVectorOrMatrix, frequency=1, start=year)
          },
          {
            stop('unknown Frequency set')
          }
  )

  return(TSobject)
}