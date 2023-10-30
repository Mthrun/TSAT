\name{ReadDates}
\alias{ReadDates}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ReadDates
}
\description{
  read univariate or multivariate time series similar to the LRN format
}
\usage{
ReadDates(FileName = NULL, InDirectory = getwd(), Silent = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FileName}{
 string, name of the  file to be written
}
  \item{InDirectory}{
Optional: string, name of directory the data will be saved in, default \code{getwd()} 

}
  \item{Silent}{
If FALSE: Comments and messages are not printed out. Message contains the information if any column is named "Time".
}
}
\details{
Internally the tibble format is used. If it can be transformed a list is given back as output as defined below. If not, the tibble frame is directly given back as output.

If no column is named "Time", the function assumes that the first column after key contains the time information. In this case the name of that
column is given back as the first argument of the output.
}
\value{
 a list with following elements:
 \item{Time}{ time in as.Date() form of n rows and 1 column. If the user named the column any other way the name is adapted, see details. }
  \item{Data}{ a numeric matrix containing n rows and d columns}
  \item{Key}{  a numeric vector of length(n)  }
  \item{Header}{ d column names for Data}

}

\author{
Michael Thrun
}

\seealso{
\code{\link{WriteDates}}
}
\examples{
##todo
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ReadDates}% use one of  RShowDoc("KEYWORDS")