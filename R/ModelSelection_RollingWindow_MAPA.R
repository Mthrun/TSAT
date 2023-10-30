ModelSelection_RollingWindow_MAPA = function(Data, Time,
                                             Horizon = 14,
                                             EvaluationWindow = 365,
                                             ModelSelectonWindow = 2,
                                             Model = NULL,
                                             QualityMeasure = "MAE"){
  # DESCRIPTION
  # Model selection with rolling window evaluation for Multi Aggregation
  # Prediction Algorithm MAPA
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
  if(is.null(Model)){
    message("Please state a set of models for the model selection.")
    return()
  }
  
  # Init
  N = length(Data)      # Length time series
  
  # Split Data in test and training
  TrainX = Data[1:(N-EvaluationWindow)]
  TrainT = Time[1:(N-EvaluationWindow)]
  
  if(requireNamespace("MAPA")) {
    # Model selection
    ModelsQM = c()
    for(i in 1:length(Model)){
      iPeriod      = Model[[i]]$Period
      iAggregation = Model[[i]]$Aggregation
      
      V1 = RollingWindow_MAPA(Data = TrainX, Time = TrainT, Horizon = Horizon,
                              Window = ModelSelectonWindow, Period = iPeriod,
                              Aggregation = iAggregation)
      # Quality Measure
      QMRes = Eval_RW_QM(Forecast = V1$Forecast, RWData = V1$RWData, QM = QualityMeasure)
      QM = QMRes$QM_1_1
      ModelsQM = c(ModelsQM, QM)
    }
    # Model selection
    MinQM  = min(ModelsQM)
    MinIdx = which.min(ModelsQM)
    # Best model
    BestPeriod      = Model[[MinIdx]]$Period
    BestAggregation = Model[[MinIdx]]$Aggregation
    # Evaluation of best model on test set
    V2 = RollingWindow_MAPA(Data = Data, Time = Time, Horizon = Horizon,
                            Window = EvaluationWindow, Period = BestPeriod,
                            Aggregation = BestAggregation)
  }
  
  # Return
  return(V2)
}
