autoCompoundModel=function(DataFrame,TimeColumnName="Time",FeatureName="Absatz",SplitDataAt,Frequency='day',ForecastPeriods=10,Holidays=NULL,PlotIt=TRUE,xlab='Time',ylab='Feature',EquiDist=TRUE,MinLowerBound=NULL,MaxUpperBound=NULL,...){
  #res=autoCompoundModel(DataFrame, TimeColumnName = "Time", FeatureName = "Absatz", SplitDataAt, Frequency = "day", ForecastPeriods = 10, Holidays = c(), PlotIt, xlab = "Time", ylab = "Feature", EquiDist=TRUE)
#
#     Automatic Compound Model
#     Automatic approach for compound model generation. Dataset is divided into training and test data by SplitDataAt.
#     training data is used for model generation, test data is used for evaluating the quality by a comparision to given data.
#     Usually, a model should also have a third part of data only used once  and for manual verification.
#  
#  INPUT
#     \item{DataFrame}{
#       data frame of at least two columns of Time and a Feauture, see example
#     }
#     \item{TimeColumnName}{
#       name of time column in data frame, see example
#     }
#     \item{FeatureName}{
#       Feature name of variable which should be forecasted, see example
#     }
#     \item{SplitDataAt}{
#       index of row where the \code{DataFrame} is divided into test and train data, see example
#     }
#     \item{Frequency}{
#       'day', 'week', 'month', 'quarter', 'year', 1(1 sec), 60(1 minute) or 3600(1 hour)
#       
#     }
#     \item{ForecastPeriods}{
#       how many days, weeks, ..., should the model be foracested on?
#     }
#     \item{Holidays}{
#       data frame of time and dates, see example
#     }
#     \item{PlotIt}{
#       Plots data model and prediction
  # ggplot object to change plot with
  # 
  # black dots: data
  # 
  # dark green line: model based on training data
  # 
  # red dots: prediction
  # 
  # blue range: confidence intervall of model
#     }
#     \item{xlab}{
#       see ggplot2
#     }
#     \item{ylab}{
#       see ggplot2
#     }
#     \item{Equidist}{
#       TRUE: time difference between each two points is equidisant, if not set FALSE
#     }
#     \item{dots}{
#       model paramters, see example
#     }
#   }
#
# OUTPUT
#     \item{Model}{
#       model paramters, see example
#     }
#     \item{Forecast}{
#       model paramters, see example
#     }
#     \item{TrainingData}{
#       model paramters, see example
#     }
#     \item{TestData}{
#       model paramters, see example
#     }
#     \item{ggObject}{
#       ggplot object to change plot
#     }
#     \item{Accuracy}{
#       ME, RMSE, MAE, MPE, MAPE of training and test dataset in a matrix
#     }
#   }
#   \references{
#     About the package behind it
#     \url{https://facebook.github.io/prophet/}
#     
#     Examplary published approach of an (manual) compund model:
#       
#       Aubert, A. H., Thrun, M. C., Breuer, L., Ultsch, A.: Knowledge discovery from high-frequency stream nitrate concentrations: hydrology and biology contributions, Scientific Reports, Nature, Vol. 6(31536), pp. 1-8, 2016.
#     
#   }
#
#   author: 04/2018, Michael Thrun for the easy-to-use wrapper, algorithm of prophet is used (see url)
#
# ## Begin of Examples
#     dates=c("2015-01-01","2015-01-02")
#     names=c("New Year's Day","New Year's weekend")
#     Holidays=data.frame(ds=as.Date(dates),holiday=names
#     )
#     data('WeeklySalesUniformRandom')
#       
#       ##Forcasting 10 weeks of 166 weeks data of sales
#       
#       weeklyres=autoCompoundModel(DataFrame = WeeklySalesUniformRandom,TimeColumnName ='Time' ,
#                                   
#                                   FeatureName = 'Sales',SplitDataAt = 156,Frequency = 'week',
#                                   
#                                   ForecastPeriods = 10,PlotIt = T,
#                                   
#                                   Holidays = holidays)
#       
#       #complete accurcay showing among other things
#       # average sales difference between prediction and data (MAE)
#       weeklyres$Accuracy
#       
#       #mean Prediction quality
#       1-abs(mean(weeklyres$TestData$y) - weeklyres$Accuracy[2,3])/weeklyres$Accuracy[2,3]
#       )
# #Plot components
# prophet::plot_forecast_component(weeklyres$Forecast,name = c('seasonal'))
# prophet::plot_forecast_component(weeklyres$Forecast,name = c('trend'))
# #further compoents in names(weeklyres$Forecast)
#     
### Second Example ###
#   
#       ###Forcasting thirdy days of 1093 days sales data frame
#       ##Grid Search fo Best Parameters
#       
#       #Generate all combinations of relevant paramters
#       prophetGrid <- expand.grid(changepoint_prior_scale = c(0.05, 0.5, 0.001),
#                                  seasonality_prior_scale = c(150,100,50, 10, 1,0.1),
#                                  holidays_prior_scale = c(150,100,50, 10, 1,0.1),
#                                  daily_seasonality=c(TRUE,FALSE)
#       )
#       
#       results <- matrix(NaN, ncol=2,nrow = nrow(prophetGrid))
#       #calculate all
#       for (i in seq_len(nrow(prophetGrid))) {
#         parameters <- prophetGrid[i, ]
#         temp=autoCompoundModel(
#           DataFrame = DailySales,TimeColumnName ='Time' ,
#           FeatureName = 'Sales',SplitDataAt = 1093,Frequency = 'day',
#           ForecastPeriods = 30,PlotIt = F,Holidays = holidays2,
#           seasonality.prior.scale = parameters$seasonality_prior_scale, 
#           changepoint.prior.scale = parameters$changepoint_prior_scale,
#           holidays.prior.scale = parameters$holidays_prior_scale,
#           daily.seasonality=parameters$daily_seasonality
#         )
#         results[i,]=temp$Accuracy[1,2:3]
#       }
#       # choose rmse or mae
#       plot(results)
#       #rmse chosen
#       prophetGrid <- cbind(prophetGrid, results[,1])
#       #best paramters found
#       DailyParameters <- prophetGrid3[prophetGrid3results == min(results[,1]), ]
#       
#       dailyres=autoCompoundModel(DataFrame = DailySales,TimeColumnName ='Time',
#                                  
#                                  FeatureName = 'Sales',SplitDataAt = 1093,Frequency = 'day',
#                                  
#                                  ForecastPeriods = 30,PlotIt = T,Holidays = holidays2,
#                                  
#                                  seasonality.prior.scale = DailyParameters$seasonality_prior_scale, 
#                                  
#                                  changepoint.prior.scale = DailyParameters$changepoint_prior_scale,
#                                  
#                                  holidays.prior.scale = DailyParameters$holidays_prior_scale,
#                                  
#                                  daily.seasonality=DailyParameters$daily_seasonality)
#       
#       dailyres$Accuracy
#       
#       1-abs(mean(dailyres$TestData$y) - dailyres$Accuracy[2,3])/dailyres$Accuracy[2,3]
#     
### End of Examples     
  #ToDo: Frequency Input abruefen auf tippfehler

  library(Rcpp)#hier funktioniert requireNamespace nicht, description und impprt geht auch nicht :-()
  # Error in cpp_object_initializer(.self, .refClassDef, ...) : 
  #   could not find function "cpp_object_initializer"
  
  requireNamespace('prophet')

  Header=colnames(DataFrame)
  Names = paste0("\\<", FeatureName, "\\>")
  if (length(Names) == 1) 
    ColNum <- grep(paste0(Names), Header)
  if (length(Names) > 1) 
    ColNum = sapply(Names, grep, Header, ignore.case = FALSE)
  if (length(ColNum) == 0) 
    stop("FeatureName not found.")
  
  Names2 = paste0("\\<", TimeColumnName, "\\>")
  if (length(Names2) == 1) 
    ColNum2 <- grep(paste0(Names2), Header)
  if (length(Names2) > 1) 
    ColNum2 = sapply(Names2, grep, Header, ignore.case = FALSE)
  if (length(ColNum2) == 0) 
    stop("TimeColumnName not found.")
  
if(EquiDist)
  # history <- data.frame(ds = seq(
  #   from=as.Date(min(DataFrame[,ColNum2])), to=as.Date(max(DataFrame[,ColNum2])), by =Frequency
  # ),
  # y = DataFrame[,ColNum])
  history <- data.frame(ds = as.Date(DataFrame[,ColNum2]),
                        y = DataFrame[,ColNum])
else{
  history <- data.frame(ds = as.Date(DataFrame[,ColNum2]),
  y = DataFrame[,ColNum])
  warnings('Working progress. May not work properly yet.')
  # su muesste man es machen
  # x=seq(from=as.Date(min(DailySales7441884and7571348$Time)),to=as.Date(max(DailySales7441884and7571348$Time)),by='day')
  # 
  # y=DailySales7441884and7571348
  # y$Time=as.Date(y$Time)
  # 
  # x=data.frame(Time=x,Absatz=0)
  # x[x$Time%in%y$Time,2]=y$Absatz
}
  xlab=paste0(xlab,' in ',Frequency,'s')
  
  if(SplitDataAt>nrow(history)) stop('Number SplitDataAt is higher than number of rows in data'
  )
  if(!is.null(MinLowerBound)){
    history$floor=MinLowerBound
  }
  if(!is.null(MaxUpperBound)){
    history$cap=MaxUpperBound
  }
 
  train=history[1:SplitDataAt,]
  testind=seq(from=(SplitDataAt+1),to=nrow(history),by=1)
  testdata=history[testind,]
  
  m <- prophet::prophet(train,holidays=Holidays,...)

  #m$logistic.floor=T
  future=prophet::make_future_dataframe(m, periods = ForecastPeriods, freq = Frequency)
 
  if(!is.null(MinLowerBound)){
    future$floor=MinLowerBound
  }
  if(!is.null(MaxUpperBound)){
    future$cap=MaxUpperBound
  }

  forecast <- predict(m, future)
  
  if(!is.null(MinLowerBound)){
    forecast$yhat_lower[forecast$yhat_lower<=MinLowerBound]=MinLowerBound
  }
  if(!is.null(MaxUpperBound)){
    forecast$yhat_upper[forecast$yhat_upper>=MaxUpperBound]=MaxUpperBound
  }
  if(!is.null(MinLowerBound)){
    forecast$yhat[forecast$yhat<=MinLowerBound]=MinLowerBound
  }
  if(!is.null(MaxUpperBound)){
    forecast$yhat[forecast$yhat>=MaxUpperBound]=MaxUpperBound
  }
  #plot(m, forecast)
  if(!is.numeric(Frequency)){ #days weeks months,quarters and years
    m$history$ds=as.Date(m$history$ds)
    forecast$ds=as.Date(forecast$ds)
  }
  ggObject=NULL
  ggObject = ggplot()
  #todo aes toi aes_string
  ggObject = ggObject + geom_ribbon(data = forecast, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), fill = "blue", alpha = 0.3)
  ggObject = ggObject + geom_line(data = forecast, aes(x = ds, y = yhat), color = "darkgreen",linetype = "solid",size=1)
  ggObject = ggObject + geom_point(data = train, aes(x = ds, y = y), size = 2)
  ggObject = ggObject + geom_point(data = testdata, aes(x = ds, y = y), size = 2, color = 'red')
  ggObject = ggObject+xlab(xlab)+ylab(ylab)
  if(PlotIt){
    print(ggObject)
  }
  if(EquiDist){
  AccuracyTest=forecast::accuracy(forecast[forecast$ds %in% testdata$ds, 'yhat'], testdata$y)
  AccuracyTrain=forecast::accuracy(forecast[forecast$ds %in% train$ds, 'yhat'], train$y)
  acc=rbind(AccuracyTrain,AccuracyTest)
  }else{
    acc=matrix(ncol=0,nrow=2)
  }
  rownames(acc)=c('Train set', 'Test set')
  return(list(Model=m,Forecast=forecast,TrainingData=train,TestData=testdata,ggObject=ggObject,Accuracy=acc))
}
