FcDecompositionModelByRegression=function(DataFrame,TimeColumnName="Time",FeatureName="Absatz",SplitDataAt,Frequency='day',ForecastPeriods,Holidays=NULL,PlotIt=TRUE,xlab='Time',ylab='Feature',EquiDist=TRUE,MinLowerBound=NULL,MaxUpperBound=NULL,Cycles=list(),Growth="linear",...){
  #res=DecompositionModelByRegressio(DataFrame, TimeColumnName = "Time", FeatureName = "Absatz", SplitDataAt, Frequency = "day", ForecastPeriods = 10, Holidays = c(), PlotIt, xlab = "Time", ylab = "Feature", EquiDist=TRUE)
#
#     Additive a or multiplicative Decomposition Model by Regression
#     Automatic approach for Decomposition model generation. Dataset is divided into training and test data by SplitDataAt.
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
#       weeklyres=DecompositionModelByRegressio(DataFrame = WeeklySalesUniformRandom,TimeColumnName ='Time' ,
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
#         temp=DecompositionModelByRegressio(
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
#       dailyres=DecompositionModelByRegressio(DataFrame = DailySales,TimeColumnName ='Time',
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
  requireNamespace('dplyr') 
  library(dplyr) #schalter warnungen ab die ansosnten kommen

  
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
  
if(EquiDist){
  # history <- data.frame(ds = seq(
  #   from=as.Date(min(DataFrame[,ColNum2])), to=as.Date(max(DataFrame[,ColNum2])), by =Frequency
  # ),
  # y = DataFrame[,ColNum])
  history <- data.frame(ds = as.Date(DataFrame[,ColNum2]),
                        y = DataFrame[,ColNum])
  if(Growth=="logistic"){
    history$cap=DataFrame$cap
    history$floor=DataFrame$floor
  }
}else{
  history <- data.frame(ds = as.Date(DataFrame[,ColNum2]),
  y = DataFrame[,ColNum])
  warnings('Working progress. May not work properly yet.')
  if(Growth=="logistic"){
    history$cap=DataFrame$cap
    history$floor=DataFrame$floor
  }
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

    if(SplitDataAt<nrow(history)){
    testind=seq(from=(SplitDataAt+1),to=nrow(history),by=1)
    testdata=history[testind,]
    if(missing(ForecastPeriods)){
      ForecastPeriods=nrow(testdata)
    }
  }else{
    if(missing(ForecastPeriods)){
      ForecastPeriods=1
    }
    testdata=history[SplitDataAt,]
    for(rr in 2:(ForecastPeriods+1))
      testdata=rbind(testdata,history[SplitDataAt,])

    testdata$ds=seq(from=testdata$ds[1],length.out = ForecastPeriods+1,by = Frequency)
    testdata=testdata[-1,]
  }
  if(!is.null(Cycles[["Mirrored"]])){
    if(isTRUE(Cycles[["Mirrored"]])){
      indtrain=1:nrow(train)
      Time=train$ds
      Time2=seq(from=as.Date(min(Time)),length.out =3*nrow(train)+1,by=paste0('-1 ',Frequency))
      Time2=sort(Time2,decreasing = FALSE)
      train=rbind(train[rev(indtrain),],train,train[rev(indtrain),],train)
      #train=rbind(train[rev(indtrain),],train)
      
      train$ds=c(Time2,Time[-1])
    }
    
  }
  #Create Prophet object with parametter settings
  m <- prophet::prophet(holidays=Holidays,growth = Growth,...)
  
  if(!is.null(Cycles[["Orders"]])){
    Orders=Cycles[["Orders"]]
    if(length(Orders)!=3){
      warning('Length of vector of "Orders" should be 3 but is not. Using default.')
      Orders=c(24,8,5)
    }
  }else{
    Orders=c(24,8,5)
  }
  
  if(!is.null(Cycles[["PriorScale"]])){
    PriorScale=Cycles[["PriorScale"]]
    if(length(PriorScale)!=3){
      warning('Length of vector of "Orders" should be 3 but is not. Using default.')
      PriorScale=c(0.8,0.8,0.8)
    }
  }else{
    PriorScale=c(0.8,0.8,0.8)
  }
  
  if(!is.null(Cycles[["monthly"]])){
    if(isTRUE(Cycles[["monthly"]])){
      if(Orders[1]<1){
          Orders[1]=1
          warning('Monthly Orders below 1. Setting 1')
      }
      m <- prophet::add_seasonality(m, name='monthly', period=30.5, fourier.order=Orders[1],prior.scale = PriorScale[1])
    print(Orders)
    print(PriorScale)
    }
  }
  if(!is.null(Cycles[["quaterly"]])){
    if(isTRUE(Cycles[["quaterly"]])){
      if(Orders[2]<1){
        Orders[2]=1
        warning('Quaterly Orders below 1. Setting 1')
      }
      m <- prophet::add_seasonality(m, name='quaterly', period=365.25/4, fourier.order=Orders[2],prior.scale = PriorScale[2])
    }
  }
  if(!is.null(Cycles[["biyearly"]])){
    if(isTRUE(Cycles[["biyearly"]])){
      if(Orders[3]<1){
        Orders[3]=1
        warning('Bi-Yearly Orders below 1. Setting 1')
      }
      m <- prophet::add_seasonality(m, name='biyearly', period=2*365.25,fourier.order = Orders[3],prior.scale = PriorScale[3])
    }
  }
  
  #if(Frequency=='month'){
    #m <- prophet::prophet(weekly.seasonality=FALSE)
    
    #m <- prophet::prophet(m,train,holidays=Holidays,fit=T,...)  
  #} 
  #train model

  m <- prophet::fit.prophet(m, train)

  #m$logistic.floor=T
  #fitmodel
  PastAndfuture=prophet::make_future_dataframe(m, periods = ForecastPeriods, freq = Frequency,include_history=T)
 #print(future)
  if(!is.null(MinLowerBound)){
    PastAndfuture$floor=MinLowerBound
  }
  if(!is.null(MaxUpperBound)){
    PastAndfuture$cap=MaxUpperBound
  }
  
  if(Growth=="logistic"){
    PastAndfuture$cap=DataFrame$cap[1:length(PastAndfuture$cap)]
    PastAndfuture$floor=DataFrame$floor[1:length(PastAndfuture$cap)]
  }
  regression <- predict(m, PastAndfuture)
  
  if(!is.null(MinLowerBound)){
    regression$yhat_lower[regression$yhat_lower<=MinLowerBound]=MinLowerBound
  }
  if(!is.null(MaxUpperBound)){
    regression$yhat_upper[regression$yhat_upper>=MaxUpperBound]=MaxUpperBound
  }
  if(!is.null(MinLowerBound)){
    regression$yhat[regression$yhat<=MinLowerBound]=MinLowerBound
  }
  if(!is.null(MaxUpperBound)){
    regression$yhat[regression$yhat>=MaxUpperBound]=MaxUpperBound
  }
  #plot(m, forecast)
  if(!is.numeric(Frequency)){ #days weeks months,quarters and years
    m$history$ds=as.Date(m$history$ds)
    regression$ds=as.Date(regression$ds)
  }
  ggObject=NULL
  ggObject = ggplot()
  #todo aes toi aes_string
  ggObject = ggObject + geom_ribbon(data = regression, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), fill = "blue", alpha = 0.3)
  ggObject = ggObject + geom_line(data = regression, aes(x = ds, y = yhat), color = "darkgreen",linetype = "solid",size=1)
  ggObject = ggObject + geom_point(data = train, aes(x = ds, y = y), size = 2)
  ggObject = ggObject + geom_point(data = testdata, aes(x = ds, y = y), size = 2, color = 'red')
  ggObject = ggObject+xlab(xlab)+ylab(ylab)
  if(PlotIt){
    print(ggObject)
  }
  forecast=regression[regression$ds %in% testdata$ds,]
  TrainingSetForecast=regression[regression$ds %in% train$ds,]
  acc=matrix(ncol=0,nrow=2)
  # if(EquiDist){
  #   #AccuracyTest=c(0,0)
  #   # print(forecast[, 'yhat'])
  #   # print(forecast[forecast$ds %in% testdata$ds, 'yhat'])
  #   # print(testdata$y)
  # AccuracyTest=forecast::accuracy(forecast[, 'yhat'], testdata$y)
  # #print(forecast[forecast$ds %in% train$ds, 'yhat'])
  # #print(train$y)
  # AccuracyTrain=forecast::accuracy(regression[regression$ds %in% train$ds, 'yhat'], train$y)
  # acc=rbind(AccuracyTrain,AccuracyTest)
  # }else{
  #   acc=matrix(ncol=0,nrow=2)
  # }
  rownames(acc)=c('Train set', 'Test set')
  return(list(Forecast=forecast,Accuracy=acc,TestData=testdata,Model=m,TrainingData=train,TrainingSetForecast=TrainingSetForecast,ggObject=ggObject))
}
