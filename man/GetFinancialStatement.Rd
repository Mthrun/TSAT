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
GetFinancialStatement(Symbol='SAP',URL='morningstar')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Symbol}{
ticker string
}
  \item{URL}{
provider of data as a string
}
}
\details{
yahoo and morningstar work currently
}
\value{
dataframe of financial statements
}
\references{
to be filled
}
\author{
Michael Thrun
}
\note{
morningstar api shuts requests randomly out
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{GetFinancialTimeSeries}}
}
\examples{
DF=GetFinancialStatement(Symbol='SAP',URL='morningstar')
}

