\name{GenerateRegularTS}
\alias{GenerateRegularTS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
GenerateRegularTS
}
\description{
Generates a regular time series out of an irregular time series.
}
\usage{
GenerateRegularTS(Datavec,TimeChar,TimeResolutionBegin='secs',

TimeResolutionEnd='mins',Minutes=1,PlotIt=FALSE,

formating="(\%y-\%m-\%d \%H:\%M:\%S)",

tz='UTC')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{Datavec}{[1:n] numerical data vector
}

  \item{TimeChar}{[1:n]
		Time, a chron object or a POSIXlt object or a character vector,  Devicespezifische Zeit, 
		if not chron object, please adjust format neceassry
		It hast to be convertible to character
}
\item{TimeResolutionBegin}{to be filled...
}
\item{TimeResolutionEnd}{to be filled...
}

\item{Minutes}{to be filled...
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
\keyword{irregular ts}% use one of  RShowDoc("KEYWORDS")
\keyword{regular ts}% __ONLY ONE__ keyword per line
