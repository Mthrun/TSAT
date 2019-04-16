\name{AdditiveDecompositionModel}
\alias{AdditiveDecompositionModel}

\title{
Additive Decomposition Model
}
\description{
Triple exponential smoothing method to fit an additive decomposition model.

Similar component modelling approach to Lewandowski's Forsys [Lewandowski, 1982, 1984] and Holt-Winters [Holt 1957; Winters 1960].
The three parameters have to be set between zero and one. 
The parameters describe the memory decay rate, the higher they are the faster the method "forgets" its past average, season or trend.
}
\usage{
AdditiveDecompositionModel(Data, SeasonalLength, NoSteps,

 alpha, beta, gamma,Silent=FALSE)
}
\arguments{
  \item{Data}{
[1:n] numerical vector, a.k.a. training set, but without test set
}
  \item{SeasonalLength}{
A season is an interval which is repetitive. such an regular interval is called a season and has a length. 
Seasonal Length L (length of expected season) has to be defined by user, e.g. often 12 for monthly Data data
}
  \item{NoSteps}{
number of steps to extrapolate model
}
  \item{alpha}{
Data smoothing factor: If you select the big number (near to 1) for alpha, it means you rely more on recent past data rather than old past data.
}
  \item{beta}{
Trend smoothing factor: If you choose the big number (near to 1) for beta, it means you rely more on past data's trend and you believe the trend will go on in future too. (you increase the weight of trend smoothing)

}
  \item{gamma}{
Seasonal change smoothing factor: If you choose the big number (near to 1) for gamma, it means you rely more on past data's seasonality and you believe the seasonality factors will remain in future.
}

  \item{Silent}{
FALSE: Warnings are printed, TRUE: no Warnings are printed
  }
}
\details{
This approach called exponential, because the formula is mulitplied with itself for each value and added a mulitplication of the previous weight before the current one,  recursivly. The parameter alpha is defined in such a way to be sure that all weights for an weighted average (level) sum up to one (the same goes for trend and seasonal). It is called smoothing because a moving average smooths the current values of a time series by forecasting future values (e.g. outliers are "averaged") if you look at the formula form the point of view of a graph.

Formula:

Let m be the number if steps ahead and L the length of the Season then,

\code{Level}: l_x=alpha(y_x - s_(x-L)) + (1 - alpha)(l_(x-1) + b(x-1))

\code{Trend}: b_x=betha(l_x - l_(x-1)) + (1 - betha)b_(x-1)   

\code{Seasonal}: s_x=gamma(y_x - l_x) + (1 - gamma)s_(x-L)     

\code{Forecast}: yhat_(x+m)=l_x + mb_x + s_(x-L+1+(m-1)mod(L)) 

Note, that the level \code{l} is sometimes also called the baseline or intercept in general, or in this special context also known as the exponentially weighted moving average.

Note, that the trend \code{b} is sometimes also called the slope in the case the time series is regular and for each two values the difference in time is one and therefore instead of dividing by the difference in time, it is sufficient to calculate only the difference in values.

Note, that only the usage of a season allows this method to foecast more than one point in future (\code{m}). Disregarding a seasing means that this method is only able to forecast one step in future.

The main difference is that Holt-Winters [Holt 1957; Winters 1960] is a static algorithm in which the alpha, beta, and gamma values do not change until they are manually altered.

In Contrast, the Lewandowski algorithm [Lewandowski, 1982, 1984], is an adaptive algorithm where alpha, beta, and gamma are tweaked automatically by proprietary algorithms. Contrary to this code, the Lewandowski method uses an multiplicative decomposition model.

The Lewandoweski method was evaluated positively on 1001 time series in [Makridakis, 1982].

}
\value{
[1:(n+NoSteps)] numerical vector of fitting (on Training set or full data) and forecasting (often of Test set) where no difference is made between training and test set.

Training set and test set have to be splitted up priorly (if required for model evaluation), and testset forecast length is defined by \code{NoSteps} which defines the number of forecast steps into the future, see example.

}
\references{
[Lewandowski, 1984]  Lewandowski, R. "Lewandowski's FORSYS Method." The forecasting Accuracy of Major Time Series Methods (1984): 245-253

[Makridakis, 1982]  Makridakis, S., Andersen, A., Carbone, R., Fildes, R., Hibon, M., Lewandowski, R., ... & Winkler, R. (1982). The accuracy of extrapolation (time series) methods: Results of a forecasting competition. Journal of forecasting, 1(2), 111-153.

[Lewandowski, 1982]  Lewandowski, Rudolf. "Practitioners' forum. Data forecasting by forsys." Journal of Forecasting 1.2 (1982): 205-214.

[Holt, 1957] C. C. Holt (1957) Forecasting seasonals and trends by exponentially weighted moving averages, ONR Research Memorandum, Carnegie Institute of Technology 52. (reprint at http://dx.doi.org/10.1016/j.ijforecast.2003.09.015).

[Winters 1960] P. R. Winters (1960). Forecasting Data by exponentially weighted moving averages. Management Science, 6, 324–342. doi: 10.1287/mnsc.6.3.324.
}
\author{
Michael Thrun
}
\note{
State-space models are models that use state variables to describe a system by a set of first-order differential or difference equations. An additive decomposition Model ("additives Komponentenmodel") is a sub-catagory.
}

\seealso{
\code{\link{HoltWinters}}, \code{\link[smooth]{es}},  \code{\link[forecast]{ses}}, \code{\link[forecast]{ets}}
}
\examples{
data("AirPassengers")
Data=as.numeric(AirPassengers)
Train=Data[1:132]
fit=AdditiveDecompositionModel(Train,12,12,0.1,0.2,0.9)
plot(Data,type='l')
points(fit,col='red')

#in principle it should be the same as
#model=forecast::ets(Train,model=c('ZZZ')) #automatic model choice
#plot(forecast(model))
#resulting in
#model=forecast::ets(Train,model=c('AAA')) #setting for additive model with seasonality
#but the latter has the error Nonseasonal data
#and the former just fits a trend

}

\keyword{compound model}
\keyword{component model}
\keyword{decomposition model}
\keyword{additive decomposition model}
\keyword{decomposition}
\keyword{lewandowski}
\keyword{forsys}
\keyword{holt-winters}
\keyword{exponential smoothing}
\keyword{state-space model}
