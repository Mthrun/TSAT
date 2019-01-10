\name{RandomForestForecast}
\alias{RandomForestForecast}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Random Forest for Forecast
}
\description{
RandomForestForecast
}
\usage{
RandomForestForecast(DF, formula = NULL,

Package='randomForest',NoOfTree=100, Horizon,

 PlotIt = TRUE,\dots)
}
\arguments{
  \item{DF}{
dataframe [1:n,1:d]
}
  \item{formula}{
either a formula describing praediktors or NULL
}
  \item{Package}{
either 'ranger' or 'randomForest'
}
  \item{NoOfTree}{
Number of trees to grow, [Hastie et al., 2013] suggests that random forest often stabilize aroundd 200, for big numbers there is no improvement contrary to boosting.
}
  \item{Horizon}{
forecast horizon
}
  \item{PlotIt}{
plots results
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
vector
}
  \item{Model}{
output of either \code{\link[randomForest]{randomForest}} or  \code{\link[ranger]{ranger}}
}
  \item{Accuracy}{
output of  \code{\link[forecast]{accuracy}}
}
  \item{TestData}{
data.frame
}
  \item{TrainData}{
data.frame
}
}
\references{
Breiman, L. (2001), Random Forests, Machine Learning 45(1), 5-32.

Hastie, TREVOR, Tibshirani, ROBERT, Friedman, JH: The elements of statistical learning: data mining, inference, and prediction, pages 587ff, 2013.

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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{RandomForestForecast}% use one of  RShowDoc("KEYWORDS")
\keyword{randomForest }% __ONLY ONE__ keyword per line
\keyword{Forecast}