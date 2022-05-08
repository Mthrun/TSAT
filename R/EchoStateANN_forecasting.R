EchoStateANN_forecasting=function(Datavector,
                          Percentage=90,Forecasthorizon=1,Seasonality=28,Scaled=TRUE,
                          Time,PlotIt=FALSE,NoRolling=TRUE,...){
  requireNamespace("echos")
  requireNamespace("tsibble")
  
  if(isTRUE(Scaled)){
    toRange=function(data, lower, upper){
        #taken from dbt.Transforms
        data <- as.matrix(data)
        if(lower==upper){
          error('interval width can not be 0!')
        }
        if (lower > upper){
          temp <- upper;
          upper <- lower;
          lower <- upper;
        }
        range <- upper - lower
        n <- dim(data)[1]
        d <- dim(data)[2]
        if ((n==1) & (d > 1)){ # row vector to colum vector
          data <- t(data)  
          wasRowVector <- 1
        }
        else{
          wasRowVector <- 0
        }
        nRow <- dim(data)[1]
        nCol <- dim(data)[2]
        min <-apply(data,2,min,na.rm=TRUE)
        min <- matrix(min,nRow,nCol,byrow=TRUE)
        max <- apply(data,2,max,na.rm=TRUE)
        max <- matrix(max,nRow,nCol,byrow=TRUE)
        # Range = Max-Min;
        range <- max-min
        range[range==0]<-1
        scaleData <- (data-min)/range
        scaleData <- lower + scaleData * (upper-lower)
        if(wasRowVector==1){
          scaleData = t(scaleData)
        }
        return(scaleData)
      }
    Datavector=toRange(Datavector,-1,1)
  }

  if(missing(Time)) Time=1:length(Datavector)
  
  V=TSAT::SplitPercentageTS(Datavector,Time,Percentage = Percentage)
  train=V$TrainingSet
  test=V$TestSet
  TimeTrain=V$TrainingTime
  TimeTest=V$TestTime
  
  DF=data.frame(date_time=V$TrainingTime,value=train)
  rownames(DF)=1:nrow(DF)
  TSdata=tsibble::build_tsibble(x=DF)
  date_time=V$TrainingTime
  value=train
  
  model=NULL
  out=NULL
  
  if(isTRUE(NoRolling)){#swift
  model=echos::train_esn(TSdata,lag=Seasonality,...)
  #take the last saisonality batch to predict the forecast horizon
  date_time=V$TrainingTime
  value=train
  out = echos::forecast_esn(model,n_ahead=length(test))
  future_forecast=out$point
  }else{
    #timestep 1
    model=echos::train_esn(TSdata,lag=Seasonality,...)
    #take the last saisonality batch to predict the forecast horizon
    date_time=V$TrainingTime
    value=train
    out = echos::forecast_esn(model,n_ahead=Forecasthorizon)
    future_forecast=out$point

    #im prinzip das,aber dauert zu lange
    for(i in 1:(length(test)-Forecasthorizon)){#timestep 2:n

      #rolling forecast
      #take one point of the test set to predict again the next seasonality
      date_time=c(date_time,TimeTest[i])
      value=c(value,test[i])

      DF=data.frame(date_time=date_time,value=value)
      rownames(DF)=1:nrow(DF)
      TSdata=tsibble::build_tsibble(x=DF)

      model_cur=echos::train_esn(TSdata,lag=Seasonality,...)

      currentpred = echos::forecast_esn(model_cur,n_ahead=Forecasthorizon)
      if(Forecasthorizon==1)
        future_forecast=c(future_forecast,currentpred$point)
      else
        future_forecast[[i]]=currentpred

    }
    model=model_cur
    out=currentpred
  }

  # if(is.list(future_forecast)){
  #   names(future_forecast)=TimeTest
  # }
  
  if (PlotIt) {
    if(Forecasthorizon==1){
      future_forecast_cur=future_forecast
    get('plotEvaluationFilteredTS',
        envir = asNamespace('TSAT'),
        inherits = FALSE)(TimeTest, test, future_forecast_cur, FALSE)
    }else{
      #ToDo
    }
  }
  return(list(
    Model = model,
    FitStats = out,
    Forecast = future_forecast,
    TestData =test,
    TestTime=TimeTest,
    TrainData=train,
    TrainTime=TimeTrain
  ))
  
}