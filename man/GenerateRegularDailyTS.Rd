\name{GenerateRegularDailyTS}
\alias{GenerateRegularDailyTS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
GenerateRegularDailyTS
}
\description{
Generates a regular time series with daily resolution out of an daily but irregular time series.
}
\usage{
GenerateRegularDailyTS(TimeChar, Datavec, na.rm = TRUE, format = '\%Y-\%m-\%d',

tz = 'UTC',option = 'stine',Header=c('Time','Data'), Start,End, PlotIt = FALSE,

AggregateFun=sum,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TimeChar}{
[1:n] Time in character or date format
}
  \item{Datavec}{
[1:n] numerical data vector
}
  \item{na.rm}{
Either \code{TRUE}, \code{FALSE}, \code{zero}, \code{mean}, \code{min}, \code{max}, \code{ff}: ForwardFill or ,\code{bf}: BackwardFill, \code{weighted_ff}, \code{weighted_bf}
}

  \item{format}{
if not default, then check \code{\link{strptime}} function
}
  \item{tz}{
  sometimes durations and time difference cannot be calculated if timezone is not chosen,
                         default: 'UTC'
}
  \item{option}{
in case of \code{na.rm=TRUE} the option of interpolation: "linear",  "spline" or "stine" . Please see  \code{\link[imputeTS]{na.interpolation}} documentation for further details
}
  \item{Header}{
  Optional, Default is \code{c('Time','Data')} vector of character defining the two column names of the tibble data frame.
  }
  \item{Start}{
  Optional, \code{as.Date} object defining the beginning of the timeseries, default is the minimum point of time in \code{TimeChar}
  }
  \item{End}{
  Optional, \code{as.Date} object defining the ending of the timeseries, default is the minimum point of time in \code{TimeChar}
  }
    \item{PlotIt}{
TRUE: plotting, FALSE: no plots
}
    \item{AggregateFun}{
  Optional, in the special case that there are several values in \code{Datavec} per date in \code{TimeChar} a warning is given back but the  Datavec values can be aggregated by a function defined with this argument. This function has to have an \code{na.rm} argument, such like \code{\link{mean}} or \code{\link{sum}} because missing values are ignored in the aggregation process. 
  }
      \item{\dots}{
  in the case that \code{AggregateFun} is needed, further argument for aggregate, see example
  }

}
\details{
Regular spaced time series with missing values filled depending on \code{na.rm} choice. The choices are

\code{ff} - ForwardFill: The first value before a \code{NaN} defines all \code{NaN} until the next value is available 

\code{bf} - BackwardFill: The last value after a \code{NaN} defines all \code{NaN} before that value

\code{zero}: \code{NaN} are replaced with zeros

\code{mean}/\code{min}/\code{max}: \code{NaN} are replaced with the average/minimum/maximum of \code{Datavec}

\code{weighted_ff}/\code{weighted_bf}: The either weighted ForwardFill or BackwardFill. weighted means that the relevant value is normalized by the number \code{NaN} before or after and this value replaces the given value and all relevant \code{NaN}
}
\value{
Regular TS of equidistance as a tibble data frame
}

\author{
Michael Thrun
}

\seealso{
\code{\link{GenerateRegularNonDailyTS}}
}
\examples{
##tobefilled

\dontrun{
#take the last value of a day
V=GenerateRegularDailyTS(Time,Price,Header = c("UTC_Time","Price"),na.rm = "ff",AggregateFun = tail,n=1)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\concept{irregular ts}% use one of  RShowDoc("KEYWORDS")
\concept{regular ts}% __ONLY ONE__ keyword per line

