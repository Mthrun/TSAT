\name{QualityMeasuresForecasting}
\alias{QualityMeasuresForecasting}
\title{Evaluate Response Forecasts}
\description{
Calculate forecast errors with one definition for direct evaluation and TSAT
forecast results. Use the training response to calculate the MASE scale.
}
\usage{
QualityMeasuresForecasting(Actual = NULL, Forecast = NULL, Indices = NULL,
  SeasonalPeriod = NULL, NonDuplicatedForecasts = FALSE, Silent = TRUE,
  TrainingData = NULL, Y = NULL, FOR = NULL, Mase_Season = NULL)
}
\arguments{
\item{Actual}{A numeric vector or matrix of observed responses. Rows are
observations. Columns are response series.}
\item{Forecast}{Predicted responses with the same dimensions as \code{Actual}.
Rows and columns must have the same order.}
\item{Indices}{Optional row indices to evaluate. The default uses every row.}
\item{SeasonalPeriod}{A positive integer for the MASE training lag. The default is 1.}
\item{NonDuplicatedForecasts}{Deprecated. All observations are retained.
TRUE gives a warning. Repeated values are valid time observations.}
\item{Silent}{A logical value retained for compatibility. Validation errors
are always reported. The function does not print the result.}
\item{TrainingData}{Optional training responses, with one column per series.
Supply these data to calculate MASE. Do not include test observations.}
\item{Y}{Legacy name for \code{Actual}. Conflicting values cause an error.}
\item{FOR}{Legacy name for \code{Forecast}. Conflicting values cause an error.}
\item{Mase_Season}{Legacy name for \code{SeasonalPeriod}. Conflicting values
cause an error.}
}
\details{
The error is actual minus forecast. Evaluate finite response and forecast
pairs in each column. Missing pairs do not change the order of the training
response used for MASE. Do not remove repeated observations.

MAE, RMSE, and Bias use response units. MAPE uses percent. It is NA if any
evaluated actual value is zero. SMAPE uses the range 0 to 200 percent.
An actual value and a forecast that are both zero contribute zero to SMAPE.

MASE is MAE divided by the mean absolute seasonal difference in the training
response. It is NA if training data are absent, contain nonfinite values,
are too short, or have a zero scale. The function does not use test data as
a replacement training scale and does not add random noise.

RAE and RSE compare the absolute and squared forecast errors with errors
from the mean of the evaluated actual values. A zero reference error gives NA.
The legacy measure AbsoluteError is \eqn{100 |\sum e / \sum y|}. It is NA
when the sum of actual values is zero. It is distinct from MAE.

MRD is the mean square root of the absolute errors. MRD_BIAS is the
directional score from \code{\link{RootDeviance}}. A perfect forecast has
MRD_BIAS equal to zero. It is distinct from the mean error named Bias.

Compatibility change: the result is now a matrix for both vector and
matrix inputs. Select a measure with \code{result[, "MAE"]}. Previous
versions returned a named vector. Named legacy input arguments remain valid.
}
\value{
A numeric matrix with one row per response series and these columns, in order:
\code{N}, \code{MAE}, \code{RMSE}, \code{Bias}, \code{MASE}, \code{MAPE},
\code{SMAPE}, \code{AbsoluteError}, \code{RAE}, \code{RSE}, \code{MRD},
and \code{MRD_BIAS}. N counts finite comparisons. Undefined scores are NA.
If no series has a finite comparison, return NULL.
}
\seealso{\code{\link{ForecastingInterface}}, \code{\link{RollingForecast}}}
\examples{
score <- QualityMeasuresForecasting(Actual = c(4, 6), Forecast = c(3, 8),
  TrainingData = c(1, 2, 3))
stopifnot(score[1, "N"] == 2, score[1, "MASE"] == 1.5,
          score[1, "Bias"] == -0.5)

actual <- cbind(first = c(4, 6), second = c(8, 12))
forecast <- cbind(first = c(3, 8), second = c(6, 16))
training <- cbind(first = 1:3, second = 2 * (1:3))
score <- QualityMeasuresForecasting(actual, forecast, TrainingData = training)
stopifnot(nrow(score) == 2, all(score[, "MASE"] == 1.5))
}
\keyword{ts}
