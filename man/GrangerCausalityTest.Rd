\name{GrangerCausalityTest}
\alias{GrangerCausalityTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Granger Causality Test
}
\description{
Granger Causality Test is a statistical hypothesis test for determining whether one time series is useful in forecasting another 
}
\usage{
GrangerCausalityTest(TimeSeries1, TimeSeries2, Order = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TimeSeries1}{
[1:n] numerical vector
}
  \item{TimeSeries2}{
[1:n] numerical vector
}
  \item{Order}{
Lags include into rauxiliary regression
}
}
\details{
\code{TimeSeries1} is assumed as the cause for \code{TimeSeries2}. Function uses \code{\link[lmtest]{grangertest}} and \code{\link[vars]{causality}}.
}
\value{
List with 
\item{WaldTest}{a Wald test comparing the unrestricted model—in which y is explained by the lags (up to order order) of y and x—and the restricted model—in which y is only explained by the lags of y, see \code{\link[lmtest]{grangertest}}}

\item{VARtest}{Computes the test statistics for Granger- and Instantaneous causality for a VAR(p), see \code{\link[vars]{causality}}
}

}
\references{
Granger, C. W. J.: Investigating Causal Relations by Econometric Models and Cross-spectral Methods, Econometrica, 37 (3), pp. 424–438, doi:10.2307/1912791, 1969.
}
\author{
Michael Thrun
}

\seealso{
\code{\link[lmtest]{grangertest}}, \code{\link[lmtest]{waldtest}}, \code{\link[vars]{causality}}
}
\examples{
#...
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{grangertest}% use one of  RShowDoc("KEYWORDS")
\keyword{granger}% __ONLY ONE__ keyword per line
\keyword{waldtest}
\keyword{CausalityTest}