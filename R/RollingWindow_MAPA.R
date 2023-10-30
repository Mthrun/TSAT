RollingWindow_MAPA = function(Data, Time, Horizon = 14, Window = 365, Period = 1,
                             Aggregation = "mean"){
  # DESCRIPTION
  # Rolling window evaluation for Multi Aggregation Prediction Algorithm MAPA
  # 
  # INPUT
  # Data[1:n]      Numeric vector
  # Time[1:n]      Character vector
  # Horizon        Integer
  # Window         Integer
  # Period         Integer
  # Aggregation    Character
  # 
  # OUTPUT
  # Error[1:Window, 1:Horizon]       Numeric Matrix
  # Forecast[1:Window, 1:Horizon]    Numeric Matrix
  # RWData[1:Window, 1:Horizon]      Numeric Matrix
  # RWTime[1:Window, 1:Horizon]      Numeric Matrix
  # Setting                          List
  # 
  # Author: QMS, 2022
  
  # Error capturing
  
  # Init
  N = length(Data)      # Length time series
  Error    = rbind()    # Forecast error: e_{t+1}, ..., e_{t+h}
  Forecast = rbind()    # Forecast      : f_{t+1}, ..., f_{t+h}
  RWData   = rbind()    # True values   : y_{t+1}, ..., y_{t+h}
  RWTime   = rbind()    # Forecast      : t_{t+1}, ..., t_{t+h}
  FOrigin  = c()
  # Rolling Window
  if(requireNamespace("MAPA")) {
    for(i in 1:Window){
      int_CFCP = N - Window - Horizon + i # Current Forecast Position
      dfTrain  = Data[1:int_CFCP]
      dfTest   = Data[int_CFCP+1:Horizon]
      res = MAPA::mapa(y         = dfTrain,       # Vector of values for training
                       ppy       = Period,        # Periods in a season of the time series at the sampled freq
                       fh        = Horizon,       # Forecast horizon
                       ifh       = 1,             # Lower aggregation level
                       minimumAL = 1,             # Lowest aggregation level
                       #maximumAl = 1,            # Highest aggregation level - DEFAULT: ppy
                       comb      = Aggregation,   # Combination operator
                       paral     = 1,             # Parallel processing
                       display   = 0,             #
                       outplot   = 0,             #
                       hybrid    = 1)             #
      # Settings for a one step forecast
      ft_Forecast = as.numeric(unlist(res[2]))    # Forecasted values for error computation
      # Settings for a multi step forecast
      tmpError = dfTest - ft_Forecast
      Error    = rbind(Error,    tmpError)
      Forecast = rbind(Forecast, ft_Forecast)
      RWData   = rbind(RWData,   dfTest)
      RWTime   = rbind(RWTime,   Time[int_CFCP+1:Horizon])
      FOrigin  = c(FOrigin, Time[int_CFCP])
    }
  }
  # Rolling Window Setting
  rownames(Error)    = NULL
  rownames(Forecast) = NULL
  rownames(RWData)   = NULL
  rownames(RWTime)   = NULL
  
  ModelSetting       = list("Period_ppy"       = Period,
                            "Aggregation_comb" = Aggregation)
  
  ForecastSetting    = list("Window"  = Window,
                            "Horizon" = Horizon)
  
  Setting            = list("Data"     = Data,
                            "Time"     = Time,
                            "Model"    = ModelSetting,
                            "Forecast" = ForecastSetting)
  # Return
  return(list("Error"       = Error,
              "Forecast"    = Forecast,
              "RWData"      = RWData,
              "RWTime"      = RWTime,
              "Setting"     = Setting))
}