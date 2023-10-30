\name{InterpolateMissingValues}
\alias{InterpolateMissingValues}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
InterpolateMissingValues
}
\description{
Interpolates missing values, by setting them NaN and then applying spline interpolation
}
\usage{
InterpolateMissingValues(Time, Datavector, option = "stine",

PlotIt = TRUE)
}

\arguments{
  \item{Time}{
[1:n] vector of time, \code{POSIXlt} or \code{POSIXct} are accepted
}
  \item{Datavector}{
[1:n] numerical vector of data
}
  \item{option}{
See \code{\link[imputeTS]{na.interpolation}}. Default is "stine"
}
  \item{PlotIt}{
TRUE: Evaluates output of function versus input by plots. Default is TRUE
}
}
\details{
Wrapper for \code{\link{InterpolateOutliers}}. Missing values are all values which are not \code{is.finite(Datavector)==TRUE}.
}
\value{
[1:n] interpolated vector of data
}

\author{
Michael Thrun
}

\seealso{
\code{\link{InterpolateOutliers}}
}

\examples{
data("airquality")
which(!is.finite(airquality$Ozone))

date_strings = paste("1973", airquality$Month, airquality$Day, sep = "-")
dates = as.Date(date_strings)

vals = InterpolateMissingValues(dates, airquality$Ozone)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{InterpolateOutliers}% use one of  RShowDoc("KEYWORDS")
\keyword{MissingValues}% __ONLY ONE__ keyword per line
\keyword{Interpolation}
\concept{Value Imputation}