\name{GetYahooSymbols}
\alias{GetYahooSymbols}
\title{Stored Yahoo ticker symbols}
\description{Return the ticker list stored in the package.}
\usage{
GetYahooSymbols() 
}

\details{
This is a historical list. Symbols are not checked against the current Yahoo service.
}
\value{
Character vector of stored symbols.
}
\examples{
stopifnot(is.character(GetYahooSymbols()))
}

\keyword{ts}
