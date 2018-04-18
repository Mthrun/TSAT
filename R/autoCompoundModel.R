autoCompoundModel=function(DataFrame,TimeColumnName="Time",FeatureName="Absatz",SplitDataAt,Frequency='day',ForecastPeriods=10,Holidays=c(),PlotIt,xlab='Time',ylab='Feature'){
  
  requireNamespace('prophet')
#Frequency	'day', 'week', 'month', 'quarter', 'year', 1(1 sec), 60(1 minute) or 3600(1 hour).
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
  

  history <- data.frame(ds = seq(
    from=as.Date(min(DataFrame[,ColNum2])), to=as.Date(max(DataFrame[,ColNum2])), by =Frequency
  ),
  y = DataFrame[,ColNum])

  
  xlab=paste0(xlab,' in ',Frequency,'s')
  
  if(SplitDataAt>nrow(history)) stop('Number SplitDataAt is higher than number of rows in data'
  )
    
  train=history[1:SplitDataAt,]
  testind=seq(from=(SplitDataAt+1),to=nrow(history),by=1)
  testdata=history[testind,]

  if(Frequency=='day')
    m <- prophet(train,daily.seasonality=TRUE)
  else if(Frequency=='week')
    m <- prophet(train,weekly.seasonality=TRUE)
  else
    m <- prophet(train)

  ff=make_future_dataframe(m, periods = ForecastPeriods, freq = Frequency, include_history = TRUE)
  forecast <- predict(m, ff)
  plot(m, forecast)
  forecast$ds=as.Date(forecast$ds)
  p=NULL
  p <- ggplot()
  p <- p + geom_line(data = forecast, aes(x = ds, y = yhat), color = "#0072B2")
  p
  p <- p + geom_point(data = train, aes(x = ds, y = y), size = 2)
  
  p <- p + geom_ribbon(data = forecast, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), fill = "#0072B2", alpha = 0.3)
  p
  p <- p + geom_point(data = testdata, aes(x = ds, y = y), size = 2, color = 'red')
  
  p=p+xlab(xlab)+ylab(ylab)#+ylim(0,2500)
  if(PlotIt){
    print(p)
  }
  return(list(Model=m,Forecast=forecast,TrainingData=train,TestData=testdata,ggObject=p))
}