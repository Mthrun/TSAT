\name{WriteTS}
\alias{WriteTS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Write Time Series
}
\description{
  Save series consisting of Time and SeriesVal and eventual further texts to a *.TS file.
}
\usage{
WriteTS(FileName, Time, SeriesVal, FurtherTexts, Header, OutDirectory = getwd(), ForceNumeric = FALSE, Comments)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FileName}{
 String, name of the  file to be written
}
  \item{Time}{
[1:n] vector of row type, typically UnixTime or other time in seconds: unique key for each line, by default: [1:n] 
}
  \item{SeriesVal}{
[1:n] vector with text without blanks or numerical data to be put in each line
}
  \item{FurtherTexts}{
Optional: [1:n,1:c] string matrix with row FurtherTexts to be put in third column
}
  \item{Header}{
Optional: [1:(c+1)] vector for the headers including for the names and FurtherTexts: Array of chars, this line will be inserted at top of file with leading #
}F
  \item{OutDirectory}{
Optional: The OutDirectory where to write into; if not given: current dir
}
  \item{ForceNumeric}{
Optional: TRUE: saves numeric format of SeriesVal, FALSE: accepts text
}
  \item{Comments}{
Optional: Vector, Char Array or matrix, these lines will be inserted at top of file with leading #


Note: Also allowed: Comments='first line \code{\\n} # second line'}
}

\author{
Michael Thrun
}

\seealso{
\link{ReadTS}
}

\examples{
data(ElectricityBRD)
WriteTS("ElectricityBRD", ElectricityBRD$Time, ElectricityBRD$Mrd_KWh)
}

\keyword{TimeSeries}
\keyword{Write}
\keyword{Save}
\concept{Time Series}
