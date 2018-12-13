SplitPercentageTS=function(Datavector,Time,Percentage=80){
  
  N = length(Datavector)
  Nt = NULL
  if (!missing(Time)) {
    Nt = length(Time)
    if (N != Nt)
      stop('Time vector is not of equal length to DataVector')
  }
  
  if (Percentage > 1)
    Percentage = Percentage / 100
  
  Split = ceiling(N * (1 - Percentage))
  if (Split %% 2 != 0) {
    Split = Split + 1
  }
  
  Train = head(Datavector, N - Split)
  Test = tail(Datavector, Split)
  if (!is.null(Nt)) {
    TrainT = head(Time, N - Split)
    TestT = tail(Time, Split)
  } else{
    TrainT = NULL
    TestT = NULL
  }
  
  return(list(
    TrainingSet = Train,
    TrainingTime = TrainT,
    TestSet = Test,
    TestTime = TestT
  ))
}