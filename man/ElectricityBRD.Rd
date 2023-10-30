\name{ElectricityBRD}
\alias{ElectricityBRD}
\docType{data}
\title{
Electricity production in BRD
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("ElectricityBRD")}
\format{
  A data frame with 0 observations on the following 2 variables.
  \describe{
    \item{\code{Mrd_KWh }}{Value in units}
    \item{\code{Time}}{as.Date object}
  }
}
\details{
unit= Mrd. KWh
}
\source{
\url{http://www.wifak.uni-wuerzburg.de/wilan/wifak/vwl/ewf/projekte/zra_ss03.htm}
}
\examples{
data(ElectricityBRD)
## maybe str(ElectricityBRD) ; plot(ElectricityBRD) ...
}
\keyword{datasets}
