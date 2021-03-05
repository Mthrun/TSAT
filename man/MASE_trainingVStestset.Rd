\name{MASE_trainingVStestset}
\alias{MASE_trainingVStestset}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mean absolute scaled error (MASE_trainingVStestset)
}
\description{
Seasonal Mean absolute scaled error (MASE_trainingVStestset)
}
\usage{
MASE_trainingVStestset(Forecast, TrainingSet, TestSet, SeasonalLength)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Forecast}{
[1:Horizont] numeric vector,
}
  \item{TrainingSet}{
[1:Horizon] Trainingset
}
  \item{TestSet}{
[Horizon+1:n] TestSet
}
  \item{SeasonalLength}{
1: Non-Seasonal, where the denominator is the mean absolut error of the one-step naive forecast method of the trainings set

Other number: A season is an interval which is repetitive. such an regular interval is called a season and has a length. Seasonal Length L (length of expected season) has to be defined by user, e.g. often 12 for monthly sales data
}
}
\details{
The mean absolute scaled error has the following desirable properties:

1.)	Symmetry
error penalizes positive and negative forecast errors equally

penalizes errors in large forecasts and small forecasts equally

2.)
Predictable behavior as y t->0: Percentage forecast accuracy measures such as the Mean absolute percentage error (MAPE) rely on division of y t skewing the distribution of the MAPE for values of y t  near or equal to 0. This is especially problematic for data sets whose scales do not have a meaningful 0, such as temperature in Celsius or Fahrenheit, and for intermittent demand data sets, where y t = 0 occurs frequently.

3.)
Interpretability: The mean absolute scaled error can be easily interpreted, as values greater than one indicate that in-sample one-step forecasts from the naïve method perform better than the forecast values under consideration.

4.)
Asymptotic normality of the MASE: The Diebold-Mariano test for one-step forecasts is used to test the statistical significance of the difference between two sets of forecasts. To perform hypothesis testing with the Diebold-Mariano test statistic, it is desirable for DM ~ N(0,1), where DM is the value of the test statistic. The DM statistic for the MASE has been empirically shown to approximate this distribution, while the mean relative absolute error (MRAE), MAPE and sMAPE do not.


}
\value{
MASE error value
}
\references{
Hyndman, R. J. (2006). "Another look at measures of forecast accuracy", FORESIGHT Issue 4 June 2006, p. 46
}
\author{
Shubham Saini
}
\note{
code extracted from \url{https://stackoverflow.com/questions/11092536/forecast-accuracy-no-mase-with-two-vectors-as-arguments} by Michael Thrun
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{MASE}% use one of  RShowDoc("KEYWORDS")
