\name{setDiffTS}
\alias{setDiffTS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
setdiff between two time series (TS)
}
\description{
symmetric setdiff between two time series (TS)
}
\usage{
setDiffTS(Time1, Time2, Vector1 = NULL, Vector2 = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Time1}{
[1:n] POSIXct, or similar time object
}
  \item{Time2}{
[1:m] POSIXct, or similar time object
}
  \item{Vector1}{
Default: NULL, else [1:n] vector
}
  \item{Vector2}{
Default: NULL, else [1:m] vector
}
}
\details{
normal setdiff return only numerical values, this function accepts all type of vectors,
}
\value{
List with
\item{TS1notinTS2}{List with two elements: Time1 and Vector1 according to left side \code{setdiff}}
\item{TS2notinTS1}{List with two elements: Time2 and Vector2 according to right side \code{setdiff}}

}
\note{
for an \code{intersect} operation transform arguments to data.frames and use \code{merge}
}
\author{
Michael Thrun
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{setdiff}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{setdiff}% use one of  RShowDoc("KEYWORDS")
\keyword{setDiffTS}% __ONLY ONE__ keyword per line
\keyword{time series}% __ONLY ONE__ keyword per line