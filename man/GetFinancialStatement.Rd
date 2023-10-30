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

URL='yahoo', Silent = TRUE,Port=4445L,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Symbol}{
ticker string
}
  \item{URL}{
provider of data as a string

yahoo: automatic web scrapping

morningstar: uses api to data bases
}
  \item{Silent}{
if no warnings=TRUE
}

  \item{Port}{
in the caseof yahoo: port accessable for direct internet connection
}
  \item{\dots}{
in the case of yahoo: further arguments for \code{\link[RSelenium]{rsDriver}} 
}
}
\details{
yahoo and morningstar work currently.

In the case of yahoo each stock puts up a mozilla rust profile in 
.../Users/<admin>/AppData/Local/Temp
of 100 mb which has to be deleted manually after the process of web scrapping
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

\donttest{

Symbols=GetYahooSymbols()
n=length(Symbols)
FinancialStatement=c()
for(i in 1:n){
  try({
  temp=GetFinancialStatement(Symbols[i],'yahoo',check=F)
  FinancialStatement=c(FinancialStatement,temp)
  })
  print(i)
}
}
}

