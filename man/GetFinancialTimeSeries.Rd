\name{GetFinancialTimeSeries}
\alias{GetFinancialTimeSeries}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
GetFinancialTimeSeries
}
\description{
returns value of a stock in a time frame
}
\usage{
GetFinancialTimeSeries(url="https://finance.yahoo.com",start = "1998-01-01",
                                quote = "Adj",instrument='ibm',PlotIt=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{url}{
}
  \item{start}{
}
  \item{quote}{
}
  \item{instrument}{
}
  \item{PlotIt}{
}
}
\details{
...
}
\value{
zoo timeseries
}
\references{
to be filled
}
\author{
Michael Thrun
}



\seealso{
\code{\link{GetFinancialStatement}}
}
\examples{
GetFinancialTimeSeries(url="https://finance.yahoo.com",start = "1998-01-01",
                                quote = "Adj",instrument='sap',PlotIt=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{FinancialTimeSeries}% use one of  RShowDoc("KEYWORDS")
\keyword{stocks}% __ONLY ONE__ keyword per line
