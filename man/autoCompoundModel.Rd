\name{autoCompoundModel}
\alias{autoCompoundModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Automatic Compound Model
}
\description{
Automatic approach for compound model generation. Dataset is divided into training and test data by SplitDataAt.
}
\usage{
autoCompoundModel(DataFrame, TimeColumnName = "Time",

FeatureName = "Absatz", SplitDataAt, Frequency = "day",

ForecastPeriods = 10, Holidays = c(), PlotIt, 

xlab = "Time", ylab = "Feature", EquiDist=TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DataFrame}{
data frame of at least two columns of Time and a Feauture, see example
}
  \item{TimeColumnName}{
name of time column in data frame, see example
}
  \item{FeatureName}{
Feature name of variable which should be forecasted, see example
}
  \item{SplitDataAt}{
index of row where the \code{DataFrame} is divided into test and train data, see example
}
  \item{Frequency}{
'day', 'week', 'month', 'quarter', 'year', 1(1 sec), 60(1 minute) or 3600(1 hour)

}
  \item{ForecastPeriods}{
how many days, weeks, ..., should the model be foracested on?
}
  \item{Holidays}{
data frame of time and dates, see example
}
  \item{PlotIt}{
Plots data model and prediction with

black dots: data

dark green line: model based on training data

red dots: prediction

blue range: confidence intervall of model
}
  \item{xlab}{
see ggplot2
}
  \item{ylab}{
see ggplot2
}
  \item{Equidist}{
TRUE: time difference between each two points is equidisant, if not set FALSE
}
  \item{dots}{
model paramters, see example
}
}
\details{
training data is used for model generation, test data is used for evaluating the quality by a comparision to given data.
}
\value{
  \item{Model}{
model paramters, see example
}
  \item{Forecast}{
Forecast, see example
}
  \item{TrainingData}{
TrainingData, see example
}
  \item{TestData}{
TestData, see example
}
  \item{ggObject}{
ggplot object to change plot with

black dots: data

dark green line: model based on training data

red dots: prediction

blue range: confidence intervall of model
}
  \item{Accuracy}{
ME, RMSE, MAE, MPE, MAPE of training and test dataset in a matrix
}
}
\references{
About the package behind it
\url{https://facebook.github.io/prophet/}

Examplary published approach of an (manual) compund model:

Aubert, A. H., Thrun, M. C., Breuer, L., Ultsch, A.: Knowledge discovery from high-frequency stream nitrate

concentrations: hydrology and biology contributions, Scientific Reports, Nature, Vol. 6(31536), pp. 1-8, 2016.

}
\author{
Michael Thrun for the easy-to-use wrapper, algorithm of prophet is used (see url)
}
\note{
Usually, a model should also have a third part of data only used once  and for manual verification.
}

\examples{
dates=c("2015-01-01","2015-01-02")
names=c("New Year's Day","New Year's weekend")
Holidays=data.frame(ds=as.Date(dates),holiday=names
)
data('WeeklySalesUniformRandom')
\donttest{

##Forcasting 10 weeks of 166 weeks data of sales

weeklyres=autoCompoundModel(DataFrame = WeeklySalesUniformRandom,TimeColumnName ='Time' ,

FeatureName = 'Sales',SplitDataAt = 156,Frequency = 'week',

ForecastPeriods = 10,PlotIt = T,

Holidays = holidays)

#complete accurcay showing among other things
# average sales difference between prediction and data (MAE)
weeklyres$Accuracy

#mean Prediction quality
1-abs(mean(weeklyres$TestData$y) - weeklyres$Accuracy[2,3])/weeklyres$Accuracy[2,3]
)
#Plot components
prophet::plot_forecast_component(weeklyres$Forecast,name = c('seasonal'))
prophet::plot_forecast_component(weeklyres$Forecast,name = c('trend'))
#further compoents in names(weeklyres$Forecast)
}

\donttest{
###Forcasting thirdy days of 1093 days sales data frame
##Grid Search fo Best Parameters

#Generate all combinations of relevant paramters
prophetGrid <- expand.grid(changepoint_prior_scale = c(0.05, 0.5, 0.001),
                           seasonality_prior_scale = c(150,100,50, 10, 1,0.1),
                           holidays_prior_scale = c(150,100,50, 10, 1,0.1),
                           daily_seasonality=c(TRUE,FALSE)
)

results <- matrix(NaN, ncol=2,nrow = nrow(prophetGrid))
#calculate all
for (i in seq_len(nrow(prophetGrid))) {
  parameters <- prophetGrid[i, ]
  temp=autoCompoundModel(
    DataFrame = DailySales,TimeColumnName ='Time' ,
    FeatureName = 'Sales',SplitDataAt = 1093,Frequency = 'day',
    ForecastPeriods = 30,PlotIt = F,Holidays = holidays2,
    seasonality.prior.scale = parameters$seasonality_prior_scale, 
    changepoint.prior.scale = parameters$changepoint_prior_scale,
    holidays.prior.scale = parameters$holidays_prior_scale,
    daily.seasonality=parameters$daily_seasonality
  )
  results[i,]=temp$Accuracy[1,2:3]
}
# choose rmse or mae
plot(results)
#rmse chosen
prophetGrid <- cbind(prophetGrid, results[,1])
#best paramters found
DailyParameters <- prophetGrid3[prophetGrid3results == min(results[,1]), ]

dailyres=autoCompoundModel(DataFrame = DailySales,TimeColumnName ='Time',

FeatureName = 'Sales',SplitDataAt = 1093,Frequency = 'day',

ForecastPeriods = 30,PlotIt = T,Holidays = holidays2,

seasonality.prior.scale = DailyParameters$seasonality_prior_scale, 

changepoint.prior.scale = DailyParameters$changepoint_prior_scale,

holidays.prior.scale = DailyParameters$holidays_prior_scale,

daily.seasonality=DailyParameters$daily_seasonality)

dailyres$Accuracy

1-abs(mean(dailyres$TestData$y) - dailyres$Accuracy[2,3])/dailyres$Accuracy[2,3]
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{compound}% use one of  RShowDoc("KEYWORDS")
\keyword{compound model}% __ONLY ONE__ keyword per line
\keyword{prophet}%