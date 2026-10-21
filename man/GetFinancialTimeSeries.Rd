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
GetFinancialTimeSeries(url = "https://finance.yahoo.com", start = "1998-01-01",
  end, quote = "Adj", instrument = "ibm", PlotIt = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{url}{Financial data service URL.}
  \item{start}{Start date for the requested observations.}
  \item{quote}{Price field requested from the service.}
  \item{instrument}{Instrument ticker symbol.}
  \item{PlotIt}{If TRUE, display the plot.
}
\item{end}{End date for the requested series.}
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
# Requires a live data service or browser driver.
\dontrun{
GetFinancialTimeSeries(url="https://finance.yahoo.com",start = "1998-01-01",end="2020-01-01",
                                quote = "Adj",instrument='sap',PlotIt=TRUE)

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{FinancialTimeSeries}% use one of  RShowDoc("KEYWORDS")
\keyword{stocks}% __ONLY ONE__ keyword per line
