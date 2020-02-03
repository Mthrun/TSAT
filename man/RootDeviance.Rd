\name{RootDeviance}
\alias{RootDeviance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Root Distance Measure
}
\description{
Given two equidistant time series, X and Y, having the same underlying time step set (in particular having the same length), represented by numerical vectors, the sum root distance describes the period-wise square-rooted deviance to the upper and lower side of X respectively Y seperated.
We can describe this behavior mathematically by the complex root:

Let E denote the difference time series of X respectively Y: \ifelse{html}{\out{E<sub>t</sub>=X<sub>t</sub>-Y<sub>t</sub>}}{\eqn{E_t := X_t - Y_t}}. We see that in period \eqn{t} \eqn{X} is larger than \eqn{Y} iff (if and only if) \ifelse{html}{\out{E<sub>t</sub><0}}{\eqn{E_t < 0}}. Conversely \eqn{X} is less than \eqn{Y} in period \eqn{t} iff \ifelse{html}{\out{E<sub>t</sub><0}}{\eqn{E_t < 0}}. The equality periods are given by the zeroes of \eqn{E}.

By applying the complex root function on the time series E we get a complex time series \eqn{\sqrt{E}}{sqrt(E)}. In period \eqn{t} we formulate \eqn{\sqrt{E_t} = A_t + i B_t}{sqrt(E_t) = A_t + i B_t}. We easily see from the earlier observation that \eqn{A} is the time series consisting of the roots of \eqn{E} in periods \eqn{t}, where \eqn{X} is larger than \eqn{Y} and is zero else.
Analogously \eqn{B} is the time series with the values root of \eqn{E} in periods where \eqn{E} is less than zero, i.e. the periods where \eqn{X} is less than \eqn{Y}.

Hence summing over all periods yields a complex number \eqn{a + i b := \sum_{t\in T} A_t + i\sum_{t\in T} B_t}{a + i b = sum_t A_t + i sum_t B_t} with 1-norm \eqn{\text{SRD}(X,Y) = a+b = \sum_{t\in T} \sqrt{|X_t-Y_t|}}{SRE(X,Y) = a + b = sum_t sqrt(X_t - Y_t)}, the sum root deviance, being the sum of the period-wise root deviances, being zero iff \eqn{X = Y}.
Using this we can also define the mean root deviance \eqn{\text{MRE}(X,Y) := \frac{\text{SRE}(X,Y)}{n}}{MRE(X,Y) = SRE(X,Y) / n}, where n denotes the size of T, respectively the length of X and Y.

But we can harvest even more information about the similarity of our time series from the complex number a+ib if it is not 0. If it is, we already have the same time series.
Understanding this as a vector in the euclidean plane along with the 1-norm, the length of this vector becomes the sum root deviance SRD(X,Y). The angle it encloses with the x-axis (since both components are non-negative, meaning it lives in the first quadrant [upper right]) is \eqn{\text{arctan}(\frac{b}{a})}{arctan(b / a)} (where we calculate in \eqn{\overline{\mathbb{R}}}{the extended real number line}, meaning if a is 0 [i.e. in every period t where X and Y differ, we always have \eqn{X_t < Y_t}{X_t < Y_t}] we set the fraction to be \eqn{\infty}{infinity} and \eqn{\text{arctan}(\infty) = \frac{\pi}{2}}{arctan(infinity) = pi / 2} per limit of the arcus-tangus). Since \eqn{\frac{b}{a}}{b / a} lives between 0 and \eqn{\infty}{infinity} the arcustangens yields a number between 0 and \eqn{\frac{\pi}{2}}{pi / 2}, by which we now can read off the tendency of X to deviate over or under Y. \eqn{a+ib} is on the diagonal \eqn{f(x) = x} iff \eqn{a = b}, meaning that we have the same rooted deviance to the top as we have to the bottom. To make this even more easy to read off, we map this number to the interval \eqn{[-1,1]}, defining \eqn{\text{bias}(X,Y) := 1 - \frac{4}{\pi}\text{arctan}(\frac{b}{a})}{bias(X,Y) := 1 - 4 / pi * arctan(b / a)}.
The upper observation yields, that the bias is 0 iff a = b (same rooted error to the upper as to the lower side), 1 iff \eqn{\text{arctan}(\frac{b}{a}) = 0}{arctan(b / a) = 0}, i.e. iff \eqn{b = 0} and -1 iff \eqn{\text{arctan}(\frac{b}{a}) = \frac{\pi}{2}}{arctan(b / a) = pi / 2}, i.e. iff \eqn{a = 0}.

We also like to consider this complex number in its polar coordinate representation, its length is \eqn{\sqrt{a^2+b^2}}{sqrt(a^2 + b^2)} with the angle bias(X,Y). The length of this vector will be put out in the length attribute.

The special case is the one of a forecast Y of X, we measure the quality of the forecast in terms of having a tendency/bias to rather be over or under the actual value or the overall quality with outliers smoothed out in terms of fitting near. The square root has a really slow growth (\eqn{\sqrt{x}\ll x}{sqrt(x) << x}) causing this outlier insensitivity.
Therefore minimizing the norm of \eqn{a+ib}, i.e. the SRD, we optimize the forecasts overall fit (while nearly neglecting outliers) and depending on the wanted forecast behavior (undercast or overcast - i.e. the forecast deviating to the top or bottom side) one can optimize parameters to make forecast strategy tend to do so.
In this application we also like to call the sum root deviance (SRD) the sum root error (SRE) of the forecast. Same goes for the mean.
}
\usage{
RootDeviance(x,y,Silent=FALSE)
}
\arguments{
  \item{x}{
Time series data given by a numerical vector
  }
  \item{y}{
Another time series given by a numerical vector. It should be the same length as \code{x}. This may be a forecast (order matters for the bias to have the right sign).
  }
  \item{Silent}{
  TRUE: No Warnings or errors are given back
  }
}
\details{
Calculating the above for the time series represented by \code{x} and \code{y}.
}
\value{
A list with the following named members

* SRD = SRD(x,y) - Sum of the root errors.

* MRD = MRD(x,y) - Mean of the root errors. (SRD / length(x))

* length = sqrt(a^2 + b^2) - Length of the complex number with real part the errors where X is over Y and imaginary part the errors where X is under Y.

* bias = bias(x,y) - Is a number bound between -1 and 1. It is 0 iff Y has the same rooted error to the upper as it has to the lower side of X. Positivity means Y deviates more to the lower side of X and conversly negativity reads as Y deviates more the upper side of X.
}
\author{
Julian Maerte
}
\references{
Kourentzes, N., Trapero, J. R., & Svetunkov, I.: Measuring the behaviour of experts on demand forecasting: a complex task, 2014.
}

\keyword{forecast quality measure}
\keyword{similarity measure}
