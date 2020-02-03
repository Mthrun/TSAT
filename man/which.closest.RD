\name{which.closest}
\alias{which.closest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
which.closest
}
\description{
finds the closes match in a given vector of time
}
\usage{
which.closest(Time, x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Time}{
vector of time in any format abs() accepts
}
  \item{x}{
object of time in any format abs() accepts
}
}

\value{
Missing and NaN values are discarded.

an integer or on 64-bit platforms, if length(x) =: n>= 2^31 an integer valued double of length 1 or 0 (iff x has no non-NAs), giving the index of the first minimum or maximum respectively of x.

If this extremum is unique (or empty), the results are the same as (but more efficient than) which(x == min(x, na.rm = TRUE)) or which(x == max(x, na.rm = TRUE)) respectively.
}

\author{
Michael Thrun
}


\seealso{
\code{\link{abs}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{which.closest}% use one of  RShowDoc("KEYWORDS")
