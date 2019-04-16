AdditiveDecompositionModel=function(Data,SeasonalLength,NoSteps,alpha,beta,gamma,Silent=FALSE){
  #author: MCT
  if(length(Data)<(2*SeasonalLength)-1) warning('Data is to small for Seasonal length. Either generarate more data or reduce the length of season.')

    if(alpha>1){
      if(isTRUE(Silent)){
      warning(
        'Please set alpha only below one.'
      )
      }
     alpha=0.999
    }  
    if(alpha<0){
      if(isTRUE(Silent)){
      warning(
        'Please set alpha only above zero.'
      )
      }
      alpha=0.001
    }
    if(beta>1){
      if(isTRUE(Silent)){
      warning(
        'Please set beta only below one.'
      )
      }
      beta=0.999
    }  
    if(beta<0){
      if(isTRUE(Silent)){
      warning(
        'Please set beta only above zero.'
      )
      }
      beta=0.001
    }
    if(gamma>1){
      if(isTRUE(Silent)){
      warning(
        'Please set gamma only below one.'
      )
      }
      gamma=0.999
    }  
    if(gamma<0){
      if(isTRUE(Silent)){
      warning(
        'Please set gamma only above zero.'
      )
      }
      gamma=0.001
    }
  
  ## Trend part ----
  InitialTrend=0
  for(i in 1:(SeasonalLength)){
    InitialTrend=InitialTrend+(Data[(i-1)+SeasonalLength]-Data[i])/SeasonalLength
  }
  InitialTrend=InitialTrend/SeasonalLength
  InitialTrend
  
  # Seasonal part ----
  NoSeasons=floor(length(Data)/SeasonalLength)
  if(NoSeasons!=length(Data)/SeasonalLength) warning('Length of Data is cannot be divided by SeasonalLength without a rest.')
  
  SeasonsAverage=c()#[1]=sum(Data[seq(from=1,to=SeasonalLength,by=1)])/SeasonalLength
  for(i in 1:NoSeasons){
    SeasonsAverage[i]=sum(Data[seq(from=SeasonalLength*(i-1)+1,to=(SeasonalLength*(i-1)+SeasonalLength),by=1)])/SeasonalLength
  }

  Seasonals=c()
  for(i in 1:SeasonalLength){
    Bin=0
    for(j in 1:NoSeasons){
      Bin=Bin+Data[SeasonalLength*(j-1)+i]-SeasonsAverage[j]
    }
    Seasonals[i]=Bin/NoSeasons
  }
 
  #Additive Decomposition
  FitAndForecast=c()
  for(i in 1:(length(Data)+NoSteps)){
    if(i==1){
      Level=Data[1]
      Trend=InitialTrend
      FitAndForecast[i]=Level
    }
    if(i>length(Data)){
      m=i-length(Data)
      FitAndForecast[i]=Level+m*Trend+Seasonals[(i)%%SeasonalLength+1]
    }else{
      cur=Data[i]
      LastLevel=Level
      #Level
      Level=alpha*(cur-Seasonals[(i)%%SeasonalLength+1])+(1-alpha)*(Level+Trend)
      #Trend
      Trend=beta*(Level-LastLevel)+(1-beta)*Trend
      #Seasonal
      Seasonals[(i)%%SeasonalLength+1]=gamma*(cur-Level)+(1-gamma)*Seasonals[(i)%%SeasonalLength+1]
      #FitAndForecast
      FitAndForecast[i]=Level+Trend+Seasonals[(i)%%SeasonalLength+1]
    }
  }
  return(FitAndForecast)
}