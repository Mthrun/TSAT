\name{GenerateRegularNonDailyTS}
\alias{GenerateRegularNonDailyTS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
GenerateRegularNonDailyTS
}
\description{
Generates a non daily regular time series out of an non-daily irregular time series.
}
\usage{
GenerateRegularNonDailyTS(Datavec,TimeChar,TimeResolutionBegin='secs',

TimeResolutionEnd='mins',Minutes=1,PlotIt=FALSE,

formating="(\%y-\%m-\%d \%H:\%M:\%S)",

tz='UTC')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{Datavec}{[1:n] numerical data vector
}

  \item{TimeChar}{[1:n]
		Time, a chron object or a POSIXlt object or a character vector,  Devices sepcific time, 
		if not chron object, please adjust format neceassry
		It hast to be convertible to character
}
\item{TimeResolutionBegin}{'secs','mins','hours' but not 'days'
}
\item{TimeResolutionEnd}{'secs','mins','hours' but not 'days'
}

\item{Minutes}{timedifference between two points in time in minutes
}
\item{PlotIt}{TRUE: plotting, FALSE: no plots
}

  \item{formating}{default="(\%y-\%m-\%d \%H:\%M:\%S)", else check \code{strptime} function
}
  \item{tz}{
  sometimes durations and time difference cannot be calculated if timezone is not chosen,
                         default: 'UTC'
  }

}
\details{
Irregular time-series objects are scalar or vector valued time series indexed by a time-stamp of class
\code{POSIXlt} used to represent irregularly temporal-spaced measurements.
For daily time see please \code{\link{GenerateRegularDailyTS}}
}
\value{
Regular TS of equidistance as a zoo object
}
\references{
...
}
\author{
Michael Thrun
}


\seealso{
\code{\link{GenerateRegularDailyTS}}
}

\keyword{irregular ts}% use one of  RShowDoc("KEYWORDS")
\keyword{regular ts}% __ONLY ONE__ keyword per line
