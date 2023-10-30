Eval_RW_QM = function(Forecast, RWData, QM = NULL){
  # DESCRIPTION
  # Evaluate a quality measure for a rolling window forecast.
  # 
  # INPUT
  # Forecast[1:Window, 1:Horizon]    Numeric Matrix
  # RWData[1:Window, 1:Horizon]      Numeric Matrix
  # QM                               Character
  # 
  # OUTPUT
  # QM[1:Window, 1:Horizon]    Numeric Matrix
  # QM[1:Window, 1:Horizon]    Numeric Matrix
  # 
  # DETAILS
  # This method assumes results from a rolling window forecast, where there
  # is a numeric matrix of forcast errors of dimension [1:Window, 1:Horizon],
  # where the window denotes the number of forecast origins on which a forecast
  # was computed and the horizon denotes that there were 1 to Horizon many
  # forecasts ahead starting from the forecast origin.
  # 
  # Author: QMS, 2022
  
  # Error capturing
  if(is.null(QM)){
    QM = "MAE"
  }
  
  # Init
  N = dim(RWData)[1]
  D = dim(RWData)[2]
  if(QM == "MAE"){                        # Mean Absolute Error
    QM_NxD = abs(RWData - Forecast)
    QM_1_D = colSums(QM_NxD)/N
    QM_1_N = apply(QM_NxD, 1, sum)/D
    QM_1_1 = sum(QM_NxD)/(N*D)
  }else if(QM == "SMAPE"){                # Symmetric Mean Absolute Percentage Error
    QM_NxD = 100*abs((RWData - Forecast)/((abs(RWData) + abs(Forecast))/2))
    QM_1_D = colSums(QM_NxD)/N
    QM_1_N = apply(QM_NxD, 1, sum)/D
    QM_1_1 = sum(QM_NxD)/(N*D)
  }else if(QM == "MAPE"){                 # Mean Absolute Percentage Error
    QM_NxD = 100*abs((RWData - Forecast)/RWData)
    QM_1_D = colSums(QM_NxD)/N
    QM_1_N = apply(QM_NxD, 1, sum)/D
    QM_1_1 = sum(QM_NxD)/(N*D)
  }else if(QM == "MRE"){                  # Mean Root Error
    QM_NxD = sqrt(abs(RWData - Forecast))
    QM_1_D = colSums(QM_NxD)/N
    QM_1_N = apply(QM_NxD, 1, sum)/D
    QM_1_1 = sum(QM_NxD)/(N*D)
  }else{
    message("Eval_RW_QM: Could not find quality measure.\n")
  }
  
  # Return
  return(list("QM_NxD" = QM_NxD,
              "QM_1_D" = QM_1_D,
              "QM_1_N" = QM_1_N,
              "QM_1_1" = QM_1_1))
}