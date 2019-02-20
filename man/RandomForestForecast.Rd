\name{RandomForestForecast}
\alias{RandomForestForecast}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Random Forest for Forecasting
}
\description{
Random forest for forecasting using multivariate regression as published in [Breiman, 2001].

This function was succesfully used in [Thrun et al., 2019].
}
\usage{
RandomForestForecast(Time, DF, formula=NULL,Horizon,

Package='randomForest', AutoCorrelation,NoOfTree=200,

PlotIt=TRUE,Holidays,SimilarPoints=TRUE,...)
}
\arguments{
  \item{Time}{
Time [1:n] bector of objects of \code{as.Date}, if not missing all possible saisonalites are used as indicators for the predictor. \code{formula} should be set to "\code{predictor} ~ ." in order that Time is used. This idea is applied in [Thrun et al., 2019] was not published in [Breiman, 2001].

}
  \item{DF}{
Dataframe [1:n,1:d] with \code{d} variables defined by \code{d-1} indicators and one predictor.
}
  \item{formula}{
Either a formula describing praediktors and indicators or NULL. Usually set to \code{formula} should be set to "\code{predictor} ~ ." if all \code{d-1} indicators should be used. 
}
  \item{Horizon}{
Forecast horizon as a number of days. The test set is defined by \code{[n-Horizon,n,1:d]} and the trainings set by \code{[1:(n-Horizon),1:d]}.
}
  \item{Package}{
Either 'ranger' or 'randomForest'
}
  \item{AutoCorrelation}{
If not missing a name of variable stored in DF can be given, it should be the predictor and be also defined in \code{formula}. The lag of autocorrelation is defined by Horizon and the strength can be checkt by \code{\link[TSAT]{AutoCorr}}
}

  \item{NoOfTree}{
Number of trees to grow, [Hastie et al., 2013] suggests that random forest often stabilize aroundd 200, for big numbers there is no improvement contrary to boosting.
}

  \item{PlotIt}{
Plots MAE results, but if \code{formula=NULL} plots time series of forecasted values of test set.
}
  \item{Holidays}{
Either German Holidays are used if missing, else a data frame or vector of \code{as.Date} objects defining the holidays.

will only be used if \code{Time} given, 
}
\item{SimilarPoints}{highly experimental, please set FALSE if you want to publish your results
}

  \item{\dots}{
Further parameters of random forest such as \code{mtry}, \code{nodesize}, \code{maxnodes}, or \code{min.node.size}.
}
}
\details{
\code{mtry}: Number of variables randomly sampled as candidates at each split, usually d/3 or higher but lower than d

\code{nodesize}('randomForest') or \code{min.node.size} ('ranger'): Default 5, Setting this number larger causes smaller trees to be grown . Trees are grown to the maximum node size possible. [Hastie et al., 2013] to grow as large trees as possible

\code{maxnodes}('randomForest' only):Maximum number of terminal nodes trees in the forest can have,If not given, trees are grown to the maximum possible (subject to limits by nodesize). [Hastie et al., 2013] to grow as large trees as possible

if NULL than autocorralation defined by Horizon is used as predictor.
}
\value{
List with
  \item{Forecast}{
Vector [1:Horizon] of predicted forecast values of the test data, names if  \code{Time} given
}
  \item{TestDataPredictor}{

See also \code{Horizon} for definiton, rownames are defined by \code{Time} if given.
}


\item{FeatureImportance}{
	Importance of Features for Forecast, see \code{\link[randomForest]{importance}} or \code{\link[ranger]{importance}} for details 
}

  \item{Accuracy}{
Output of \code{\link[forecast]{accuracy}}
}
  \item{Model}{
Output of either \code{\link[randomForest]{randomForest}} or  \code{\link[ranger]{ranger}}
}

  \item{TestDataIndicators}{
data.frame[1:(d-1),1:Horizon], in the multivariate case all variables except predictor, in the other case NULL. 

See also \code{Horizon} for definiton of length, rownames are defined by \code{Time} if given.
}
  \item{TrainData}{
data.frame[1:d,1:k], see \code{Horizon} for definiton, rownames are defined by \code{Time} if given.
}
}
\references{
[Breiman, 2001]  Breiman, L., Random Forests, Machine Learning 45(1), 5-32, 2001.

[Hastie, 2014]  Hastie, TREVOR, Tibshirani, ROBERT, Friedman, JH: The elements of statistical learning: data mining, inference, and prediction, pages 587ff, 2013.

[Thrun et al., 2019]   Thrun, M., Maerte, J., Boehme, P, and Gehlert, T.: Applying Two Theorems of Machine Learning to the Forecasting of Biweekly Arrivals at a Call Center, Proceedings of ECDA, accepted, Bayreuth, 2019.

}
\author{
Michael Thrun
}


%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
\code{\link[randomForest]{randomForest}}, \code{\link[ranger]{ranger}}
}
\examples{
##ToDo
}
\note{For n=1 example of forecasting [Thrun et al.,2019] it was visible to the data scientist that even with the choice of the same parameters and data \pkg{randomForest} extremly outperformed \pkg{ranger}. The reason is unknown and this information remains unpublished.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{RandomForestForecast}% use one of  RShowDoc("KEYWORDS")
\keyword{randomForest }% __ONLY ONE__ keyword per line
\keyword{Forecast}