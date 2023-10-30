\name{InterpolateOutliers}
\alias{InterpolateOutliers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
InterpolateOutliers
}
\description{
Interpolates Outliers setting them NaN and then applying spline interpolation
}
\usage{
InterpolateOutliers(Time, Datavector, OutliersTime, option = "stine",

PlotIt = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Time}{
[1:n] vector of time, \code{POSIXlt} or \code{POSIXct} are accepted
}
  \item{Datavector}{
[1:n] numerical vector of data
}
  \item{OutliersTime}{
[1:k] vector of time of outliers, in the format of the Time values
}
  \item{option}{
see \code{\link[imputeTS]{na_interpolation}}
}
  \item{PlotIt}{
Default: FALSE, TRUE: Evaluates output of function versus input by plots
}
}
\details{
Assumption is that outliers should be ignore in timeseries analysis.
}

\value{
[1:n] interpolated vector of data
}


\seealso{
In case of missing data, e.g. NA values, see \code{\link{InterpolateMissingValues}} instead
}

\examples{
data(airquality)

ind = which(is.finite(airquality$Solar.R))

# Remove non finite values or alternatively, interpolate missing values:
# data = InterpolateMissingValues(dates, airquality$Solar.R)
date_strings = paste("1973", airquality$Month[ind], airquality$Day[ind], sep = "-")
dates = as.Date(date_strings)

data = airquality$Solar.R[ind]
res = WaveletOutlierDetection(data)
outliersInd = which(res!=0)

vals = InterpolateOutliers(dates,data,dates[outliersInd])
}

\author{
Michael Thrun
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{InterpolateOutliers}% use one of  RShowDoc("KEYWORDS")
\keyword{Outliers}% __ONLY ONE__ keyword per line
\keyword{Interpolation}
\keyword{Spline}

