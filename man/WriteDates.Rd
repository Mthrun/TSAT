\name{WriteDates}
\alias{WriteDates}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Write Dates
}
\description{
  Saves univariate or multivariate time series in daily resolution to a *.csv file similar to a *.lrn
}
\usage{
WriteDates(FileName, TSdata, Key = c(), OutDirectory = getwd(), Comments = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FileName}{
 String, name of the  file to be written
}
  \item{TSdata}{
 [1:n,1:(d+1)] dataframe of tibble of data n cases in rows, d variables in columns, may contain NaN, first column is the time in as.Date() format
}
  \item{Key}{
 A numeric vector of length(n)  
}
  \item{OutDirectory}{
Optional: String, name of directory the data will be saved in, default \code{getwd()} 
}
  \item{Comments}{
A string which is inserted as a comment in the first line in the file
}
}
\details{
  Comfortably writes univariate or multivariate time series in daily resolution to a file similar to the LRN Format. The header is always the named columns of the data.frame or tibble.
  The length of \code{Key} has to be \code{ncol(Data)} or \code{ncol(Data[[1]])} respectively.
  If \code{Key} is empty it is replaced by a vector \code{1:ncol(Data)}
  
}

\author{
Michael Thrun
}

\seealso{
\code{\link{ReadDates}}
}
\examples{
#todo
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{WriteDates}% use one of  RShowDoc("KEYWORDS")
