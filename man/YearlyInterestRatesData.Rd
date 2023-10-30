\name{YearlyInterestRatesData}
\alias{YearlyInterestRatesData}
\docType{data}
\title{
Yearly Interest Rates Data

}
\description{
Eight centuries of global real interest rates published in [Schmelzing, 2019].
}
\usage{data("YearlyInterestRatesData")}
\format{
  A data frame with 713 observations on the following 5 variables.
  \describe{
    \item{\code{Year}}{The year}
    \item{\code{Nominal_Italy}}{Nominal interest rate per year in in Italym}
    \item{\code{Nominal_UK}}{Nominal interest rate per year in in United Kingdom}
    \item{\code{Inflation_Italy}}{inflation in Italy per year}
    \item{\code{Inflation_UK}}{Inflation in United Kingdom per year}
  }
}
\details{
The annual interest rate is the rate over a period of one year. Interest rates are the the amount of interest as a proportion of the amount lent, deposited or borrowed (called the principal sum). Nominal interest rate refers to the interest rate before taking inflation into account. Real interest rates are interest rates that have been adjusted for developments in purchasing power. They are broadly calculated as the difference between the nominal interest rate and the inflation rate (in the case of low interest rates and relatively stable prices).
}
\source{
eight-centuries-of-global-real-interest-rates-r-g-and-the-suprasecular-decline-1311-2018-data.xlsx, IV. Country level, 1310-2018.
}
\references{
[Schmelzing, 2020]  Schmelzing, P.: Eight Centuries of Global Real Rates, R-G, and the 'Suprasecular' Decline, 1311-2018, Bank of England, Staff Working Paper No. 845, pp. 1-108. 2020.
}
\examples{
data(YearlyInterestRatesData)
RealInterestRates_UK=YearlyInterestRates$Nominal_UK-YearlyInterestRates$Inflation_UK
plot.ts(YearlyInterestRates$Year,RealInterestRates_UK,type='l')

RealInterestRates_Italy=YearlyInterestRates$Nominal_Italy-YearlyInterestRates$Inflation_Italy
plot.ts(YearlyInterestRates$Year,RealInterestRates_Italy,type='l')
}
\keyword{datasets}
