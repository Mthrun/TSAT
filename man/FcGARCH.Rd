\name{FcGARCH}
\alias{FcGARCH}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Forecasting with Model for Generalized Autoregressive Conditional Heteroscedasticity
}
\description{
autoregressive conditional heteroskedasticity (ARCH) model is a statistical model for time series data that describes the variance of the current error term or innovation as a function of the actual sizes of the previous time periods' error terms often the variance is related to the squares of the previous innovations - Wikipedia
}
\usage{
FcGARCH(Data, VarianceModel = list(model = "sGARCH", garchOrder = c(1, 1)),

 ForecastHorizon, DistributionModel = 'norm', 

PlotIt = TRUE, Summary = FALSE, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
[1:n] Datavector
}
  \item{VarianceModel}{
see \code{\link{ugarchspec}}, The The ARCH (q) and GARCH (p) orders. be chosen manually accordingly to the problem at hand, possible model variants are

“sGARCH”, “fGARCH”, “eGARCH”, “gjrGARCH”, “apARCH” and “iGARCH” and “csGARCH”.
}
  \item{ForecastHorizon}{
Number of Forecast units of time
}
  \item{DistributionModel}{
Optional, see \code{\link{ugarchspec}}
}

  \item{PlotIt}{
Optional, If TRUE, plots the forecast against the true values
}
  \item{Summary}{
Optional, If TRUE, plots the summary of the model
}
  \item{\dots}{
Optional, Further argument passed on to \code{\link{ugarchspec}}
}
}
\details{
The ARCH model is appropriate when the error variance in a time series follows an autoregressive (AR) model; if an autoregressive moving average model (ARMA) model is assumed for the error variance, the model is a generalized autoregressive conditional heteroskedasticity (GARCH) model - Wikiepdia
}
\value{
List of 
\item{Forecast}{[1:ForecastHorizon] of Data}
\item{Model}{Model, the output of \code{\link{ugarchspec}}}
in mode \code{\link{invisible}}
}

\references{
R. F. Engle: Autoregressive Conditional Heteroskedasticity   with Estimates of the Variance of UK. Inflation. In:   Econometrica, Vol.: 50, pp. 987 - 1008, 1982.

T. Bollerslev: Generalized Autoregressive Conditional   Heteroskedasticity. In: Journal of Econometrics, Vol.: 31   No.: 3, pp. 307 - 327, 1986.

Franke, J., Härdle, W., Hafner, C. M.: Statistics of Financial Markets: An Introduction. Springer, Berlin, Heidelberg, New York, 2. Auflage, 2008.
}
\author{
Michael Thrun
}
\note{
wrapper for \code{\link{ugarchspec}}
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{ugarchspec}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ARCH}% use one of  RShowDoc("KEYWORDS")
\keyword{GARCH}% __ONLY ONE__ keyword per line
