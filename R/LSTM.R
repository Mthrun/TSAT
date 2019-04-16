LSTM=function(vec_data, forecast_length, batch_size = HighestPrimePotence(forecast_length),
              hidden_layers = 1, time_steps = 1, epochs = 100, neurons = 50,
              stateful = TRUE, loss_function, optimizer = "adam",
              plot_evaluation = FALSE, plot_future = TRUE, 
              activation_function = c('tanh','hard_sigmoid')) {
  

  if(forecast_length%%batch_size != 0) stop(paste("The batch size has to be a divisor of the forecast length.
                                            Please choose one of the following: ", Divisors(forecast_length)))
  c = floor(length(vec_data) / forecast_length)
  if(c < 4) stop("Since we want to divide data into 3 pieces, where two have length forecast_length and the third
                 is atleast twice as big, length(data) / forecast_length has to be atleast 4.")
  remainder = length(vec_data)%%forecast_length
  if(remainder != 0) warning(paste("The dataset length is not divisible by the forecast length. We will neglect the last ", remainder,
                                   " places of the dataset while forecasting.", collapse=""))
  vec_training = vec_data[1:((c-1) * forecast_length)]
  vec_lag_training = head(vec_training, length(vec_training) - forecast_length)
  vec_training = tail(vec_training, length(vec_training) - forecast_length)
  vec_valid = vec_data[((c-1) * forecast_length + 1):(c * forecast_length)]
  
  #require(tensorflow)
  requireNamespace('tensorflow')
  requireNamespace('keras')
  requireNamespace('TSAT')
  
  
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
  
  
  
  if(missing(loss_function)){
    # require(tensorflow)
    loss_function = tensor_mrd
  }
  
  #Initialize ANN
  model <- keras::keras_model_sequential()
  
  #Input Layer
  keras::layer_lstm(
    model,
    units            = neurons,
    input_shape      = c(time_steps, 1),
    batch_size       = batch_size,
    return_sequences = TRUE,
    stateful         = stateful,
	activation = activation_function[1],
	recurrent_activation = activation_function[2]
  )

  if(hidden_layers > 1){
    for (i in 1:(hidden_layers - 1))
      keras::layer_lstm(
        model,
        units            = neurons,
        return_sequences = TRUE, #enables more than one layer
        stateful         = stateful,
		activation = activation_function[1],
		recurrent_activation = activation_function[2]
      )
    }
  keras::layer_lstm(
    model,
    units            = neurons,
    activation=activation_function[1], 
    recurrent_activation=activation_function[2],
    return_sequences = FALSE, #last layer has to be false
    stateful         = stateful
  )
  #Output Layer
  keras::layer_dense(model, units = 1)
  
  keras::compile(model, loss = loss_function, optimizer = optimizer) #adam: gradient descent
  
  # Tensor Format:
  #   
  #   Predictors (X) must be a 3D Array with dimensions: [samples, timesteps, features]: The first dimension is the length of values, 
  # the second is the number of time steps (lags), and the third is the number of predictors (1 if univariate or n if multivariate)
  # Outcomes/Targets (y) must be a 2D Array with dimensions: [samples, timesteps]: The first dimension is the length of values and the second is the number of time steps (lags)
  #
  arr_training <- array(data = vec_lag_training, dim = c(length(vec_training), 1, 1))
  arr_testing <- array(data = vec_valid, dim = c(length(vec_valid), 1, 1))
  
  # Building and Evaluation Model: Eierlegende-Wollmilchsau :-(
  out = keras::fit(
    model,
    x = arr_training,
    y = as.numeric(vec_training),
    batch_size = batch_size,
    epochs = epochs,
    Verbosity = 1,
    # validation_split = 1/(c-2),
    validation_split = 0
  )
  
  # for (i in 1:epochs) {
  #   model %>% fit(x          = arr_training, 
  #                 y          = as.numeric(vec_training), 
  #                 batch_size = batch_size,
  #                 epochs     = 1,
  #                 verbose    = 1,
  #                 shuffle    = FALSE)
  #   
  #   model %>% reset_states()
  # }
  
  # out sollte einen Forecast auf historischen Daten liefern damit loess den evaluieren kann, tut es aber nicht
  if (plot_evaluation)
    plot(out) #evaluierungsplot funktioniert aber anhand forecast historischer daten, komisch...
  
  #Hier uebregen wir stringend keine Daten und erwarten vom Model einen "zukuenftigen" Forecast
  # moeglicherweise ist nur jeweils ein forecast fuer t+1 moeglich und dann per rollforward verfahren erneut aufzurufen
  pred_out2 = predict(model, array(tail(vec_training, forecast_length), dim=c(forecast_length, 1, 1)), batch_size = batch_size) #probably calls predict.keras.engine.training.Model
  # TestSet=vec_data[((c-1) * forecast_length + 1):length(vec_data)]
  Forecast = as.numeric(pred_out2)
  if (plot_future) {
    # plot(x = ((c-1) * forecast_length + 1):length(vec_data), y = vec_data[((c-1) * forecast_length + 1):length(vec_data)], type = "l", xlab="time", ylab="value")
    # lines(x = ((c-1) * forecast_length + 1):(c * forecast_length), y = pred_out2, col="red")
    
    # Achtung: Evaluiert wird nur auf der Laenge des Forecasts, Laenge des Testsets kan groesser sein, dann wird er abgeschnitten
    get('plotEvaluationFilteredTS',
        envir = asNamespace('TSAT'),
        inherits = FALSE)(1:forecast_length, vec_valid, Forecast, FALSE)
    
  }
  return(list(
    Model = model,
    FitStats = out,
    Forecast = Forecast,
    TestData =vec_valid,
    TrainData=vec_training
  ))
}
