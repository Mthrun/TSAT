\name{GetCashFlow}
\alias{GetCashFlow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
GetCashFlow
}
\description{
Get data of quaterly Financial Cash Flows
}
\usage{
GetCashFlow(Symbol = "SAP",

 URL = "yahoo", Silent = TRUE)
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
dataframe of financial Cash Flow
}
\references{
Thrun, M.C., Knowledge Discovery in Quarterly Financial Data of Stocks Based on the Prime Standard using a Hybrid of a Swarm with SOM, in European Symposium on Artificial Neural Networks, Computational Intelligence and Machine Learning (ESANN), M. Verleysen, Editor. 2019: Bruges, Belgium
}
\author{
Michael Thrun, Hamza Tayyab
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{GetFinancialTimeSeries}}
}
\examples{
#DF=GetCashFlow(Symbol='SAP',URL='morningstar')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
