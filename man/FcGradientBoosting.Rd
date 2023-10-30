\name{FcGradientBoosting}
\alias{FcGradientBoosting}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Forecast Gradient Boosting
}
\description{
Gradient Boosting as published by [Chen/Guestrin, 2016] is a machine learning technique for regression and classification problems, which produces a prediction model in the form of the ensemble of weak prediction models, typically decision trees. However it is clamed that an ensemble of weak predictors when combined together should hive a strong model with superior accuracy.
}
\usage{
FcGradientBoosting(Time, Datavector, Frequency = "days", Horizon, FUN = sum, PlotIt = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Time}{
vector, daily timestamps
}
  \item{Datavector}{
vectorm daily data
}
  \item{Frequency}{
if aggregation required, months or weeks possible
}
  \item{Horizon}{
forecast horizon, splits data to test and train
}
  \item{FUN}{
aggregation function, usually sum
}
  \item{PlotIt}{
plots results if TRUE
}
}
\details{
"Gradient Boosting algorithm is a machine learning technique used for building predictive tree-based models.Boosting is an ensemble technique in which new models are added to correct the errors made by existing models. Models are added sequentially until no further improvements can be made. The ensemble technique uses the tree ensemble model which is a set of classification
and regression trees (CART)." [see url] 

Do not confuse with bagging: simple ensembling technique in which we build many independent predictors/models/learners and combine them using some model averaging techniques. (e.g. weighted average, majority vote or normal average.

It should be noted that in order to forecast more than one step saisonality extracted by either an arma model (\pkg{gbm}) or and fourier model (\pkg{forecastxgb}) has to be used. 
}
\value{
List with
  \item{Forecast}{
vector
}
  \item{Model}{
output of
}
  \item{ForecastStats}{
output of
}
  \item{TestData}{
data.frame
}
  \item{TrainData}{
data.frame
}

}

\references{
[Chen/Guestrin, 2016]   Chen, Tianqi ans Guestrin, Carlos: XGBoost: A Scalable Tree Boosting System, In Krishnapuram, Balaji Shah, Mohak Smola, Alexander J. Aggarwal, Charu C., Shen, Dou, Rastogi, Rajeev, Proceedings of the 22nd ACM SIGKDD International Conference on Knowledge Discovery and Data Mining, San Francisco, CA, USA, August 13-17, 2016, ACM, pp. 785–794. arXiv:1603.02754,  doi:10.1145/2939672.2939785, 2016.

\url{https://medium.com/@rakesh.poojary/forecasting-markets-using-extreme-gradient-boosting-xgboost-9c62605c4b83}
}
\author{
Michael Thrun
}
\note{
\pkg{forecastxgb} not on cran, available through: devtools::install_github("ellisp/forecastxgb-r-package/pkg")
}
\seealso{
\code{\link[forecastxgb]{xgbar}}
}
\examples{
##ToDo
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{GradientBoosting}% use one of  RShowDoc("KEYWORDS")
\keyword{xgboost}% __ONLY ONE__ keyword per line
\concept{Gradient boosting}% use one of  RShowDoc("KEYWORDS")
