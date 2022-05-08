LSTM_forecasting=function(Datavector,
                          Percentage=90,Forecasthorizon=1,Seasonality=28,Scaled=TRUE,
                          ErrorLoss="MSE",Epochs=100,Neurons=28,	
                          ActivationFunction="relu",RecurrentActivation="sigmoid",Batch_size=1,Time,PlotIt=FALSE,Silent=TRUE,...){
  
  #ToDo fuer spaeteres multivariates forecasting
  NumberFeatures=1
  #Todo muessen vermutlich fuer Forecasthorizon>1 angepasst werden
  batch_size_ts_gen=1
  time_steps=1
  
  #damit es in python keinen fehler gibt, wird explizit ein numerischer vektor erwartet. keine matrize und kein int vector
  
  if(!is.vector(Datavector)) Datavector=as.vector(Datavector)
  
  if(mode(Datavector)!="numeric") Datavector=as.numeric(Datavector)
  ###############################################################
  # Function for keras backend calculating the sum root error.  #
  # Can't use the upper one due to a bug in tensorflow backend. #
  ###############################################################
  tensor_srd = function(x,y) {
    #require(tensorflow)
    requireNamespace('tensorflow')
    requireNamespace('keras')
    return(
      keras::k_sum(
        keras::k_sqrt(
          keras::k_abs(
            x - y
          )
        )
      )
    )
  }
  
  tensor_mrd = function(x,y) {
    #require(tensorflow)
    requireNamespace('tensorflow')
    requireNamespace('keras')
    return(
      keras::k_mean(
        keras::k_sqrt(
          keras::k_abs(
            x - y
          )
        )
      )
    )
  }
  
  
switch(ErrorLoss,
       "MRD"={
         loss_function=tensor_mrd
       },
       "SRD"={
         loss_function=tensor_srd
       },
       "MSE"={
         loss_function='mean_squared_error'
       },
       "MAE"={
         loss_function="mean_absolute_error"
       },
       {
         stop('LSTM_forecasting: Please select correct ErrorLoss')
       })

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
  # Tensor Format:
  #   
  #   Predictors (X) must be a 3D Array with dimensions: [samples, timesteps, features]: The first dimension is the length of values, 
  # the second is the number of time steps (lags), and the third is the number of predictors (1 if univariate or n if multivariate)
  # Outcomes/Targets (y) must be a 2D Array with dimensions: [samples, timesteps]: The first dimension is the length of values and the second is the number of time steps (lags)
  #
  generator = keras::timeseries_generator(data = train,targets = train,length=Seasonality,batch_size=batch_size_ts_gen)

  model <- keras::keras_model_sequential()
  keras::layer_lstm(
    model,
    units            = Neurons,
    input_shape      = c(time_steps, NumberFeatures),
    return_sequences = TRUE,
    activation=ActivationFunction,
    recurrent_activation=RecurrentActivation,...
  )
  keras::time_distributed(model,keras::layer_dense(units = 1))
  keras::compile(model,loss=loss_function, optimizer='adam') #adam: gradient descent
  if(isFALSE(Silent)){
    summary(model)
    verbose=1
  }else{
    verbose=0
  }
  
  out = keras::fit(
    model,
    generator,
    batch_size = Batch_size,
    epochs = Epochs,verbose=verbose
  )
  
  pred_out1 = predict(model,train)
  #plot(train,type="l")
  #points(as.numeric(pred_out1),type="l",col="red")
  
  currentpred=c()
  #take the last saisonality batch to predict the forecast horizon
  training=tail(train,Seasonality)
  future_forecast=c()
  for(i in 1:length(test)){
    currentpred = head(predict(model,array(data = tail(training,Seasonality),c(1,1,1))),Forecasthorizon)
    if(Forecasthorizon==1)
      future_forecast=c(future_forecast,currentpred)
    else
      future_forecast[[i]]=currentpred
    
    #rolling forecast
    #take one point of the test set to predict again the next seasonality
    training=tail(c(training,test[i]),Seasonality)
  }
  
  if(is.list(future_forecast)){
    names(future_forecast)=TimeTest
  }
  
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
    TrainTime=TimeTrain,
    ForecastTrain=pred_out1
  ))
  
}