\name{GetFinancialStatement}
\alias{GetFinancialStatement}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
GetFinancialStatement
}
\description{
Get data of quaterly Financial Statements
}
\usage{
GetFinancialStatement(Symbol='SAP',

URL='morningstar', Silent = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Symbol}{
ticker string
}
  \item{URL}{
provider of data as a string
}
  \item{Silent}{
if no warnings=TRUE
}
}
\details{
yahoo and morningstar work currently
}
\value{
dataframe of financial statements
}
\references{
Thrun, M.C., Knowledge Discovery in Quarterly Financial Data of Stocks Based on the Prime Standard using a Hybrid of a Swarm with SOM, in European Symposium on Artificial Neural Networks, Computational Intelligence and Machine Learning (ESANN), M. Verleysen, Editor. 2019: Bruges, Belgium
}
\author{
Michael Thrun, Hamza Tayyab
}
\note{
morningstar api shuts requests randomly out
}

\seealso{
\code{\link{GetFinancialTimeSeries}}
}
\examples{
DF=GetFinancialStatement(Symbol='SAP',URL='morningstar')
}

