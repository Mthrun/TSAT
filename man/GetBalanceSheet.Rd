\name{GetBalanceSheet}
\alias{GetBalanceSheet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
GetBalanceSheet
}
\description{
Get data of quaterly Financial Balance Sheets
}
\usage{
GetBalanceSheet(Symbol = "SAP",

 URL = "yahoo", Silent = TRUE,Port=4445L,\dots)
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
yahoo and morningstar work currently
}
\value{
dataframe of financial Balance Sheet
}

\author{
Michael Thrun, Hamza Tayyab
}
\note{
morningstar api shuts requests randomly out
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\references{
Thrun, M.C., Knowledge Discovery in Quarterly Financial Data of Stocks Based on the Prime Standard using a Hybrid of a Swarm with SOM, in European Symposium on Artificial Neural Networks, Computational Intelligence and Machine Learning (ESANN), M. Verleysen, Editor. 2019: Bruges, Belgium
}
\seealso{
\code{\link{GetFinancialTimeSeries}}
}
\examples{
#DF=GetBalanceSheet(Symbol='SAP',URL='morningstar')
\donttest{

Symbols=GetYahooSymbols()
n=length(Symbols)
BalanceSheet=c()
for(i in 1:n){
  try({
  temp=GetBalanceSheet(Symbols[i],'yahoo',check=F)
  BalanceSheet=c(BalanceSheet,temp)
  })
  print(i)
}
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
