FcOptimizeAdditiveDecompositionModel=function(Data,SeasonalLength=52,Horizon=12,Iterations=100,PlotIt=TRUE){
  #author: Michael Thrun
  requireNamespace('optimx')
  requireNamespace('forecast')

  n=length(Data)
  SplitAt=n-Horizon
  Train=head(Data,SplitAt)
  ntrain=length(Train)
  #Kuenstlich verlaengern
  if(!ntrain>(2*SeasonalLength-1)){
    if(ntrain<SeasonalLength){
      Train=c(rev(Train),Train,rev(Train),Train,rev(Train),Train)
    }else{
      #ntrain<2*SeasonalLength & ntrain>SeasonalLength
        Train=c(rev(Train),Train)
    }
  }
  ntrain=length(Train)
  #SeasonalLength has to divide Data without a rest
  if(ntrain>(2*SeasonalLength-1)){
    m=length(Train)%%SeasonalLength
    Train=tail(Train,length(Train)-m)
  }
  Test=tail(Data,Horizon)
  #quick an dirty: wir nutzen aus, das variablen immer eine ebene hoer gesucht werden
  #wenn sie nicht in der tieferen ebene gefunden werden koennen
  opt=function(x){
    # data("AirPassengers")
    # Data=as.numeric(AirPassengers)
    # Train=Data[1:132]
    alpha=x[1]
    beta=x[2]
    gamma=x[3]
    y=FcAdditiveDecompositionModel(Train,SeasonalLength,0,alpha,beta,gamma,Silent=TRUE)
    if(length(y)>1)
      return(RootDeviance(Train,y)$MRD)
    #return(sum(abs(RelativeDifference(Train,y))))
    #return(accuracy(y,Train)[1,3])
    else
      return(99999)
  }
  for(i in 1:Iterations){

    init=runif(3,min=0.01,max=0.99)
    outtmp=optimx::opm(fn=opt,par =  init,lower = c(0.01,0.01,0.01),upper = c(0.99,0.99,0.99),method = 'L-BFGS-B')
    fit=FcAdditiveDecompositionModel(Train,SeasonalLength = SeasonalLength,Horizon,outtmp$p1,outtmp$p2,outtmp$p3,Silent=TRUE)
    ForecastTrain=head(fit,length(Train))
    mnew=RootDeviance(Train,ForecastTrain)$MRD

    if(i==1){
      out=outtmp
      m=RootDeviance(Train,ForecastTrain)$MRD
    }
    if(mnew<m){
      m=mnew
      out=outtmp
    }
    #print(c(out$p1,out$p2,out$p3))
  }
  print(c(out$p1,out$p2,out$p3))
  fit=FcAdditiveDecompositionModel(Train,SeasonalLength = SeasonalLength,Horizon,out$p1,out$p2,out$p3)
  Forecast=tail(fit,Horizon)
  acc=forecast::accuracy(Forecast,Test)
  if(PlotIt){
    par(pty="m")
    plot(c(Train,Test),type='l',ylim=c(min(c(Train,Test,Forecast),na.rm = T),max(c(Train,Test,Forecast),na.rm=T)))
    points(fit,col='red')
    abline(v = length(Train),col='magenta')
  }
  return(list(Forecast=Forecast,Accurary=acc,Train=Train,ForecastTrain=head(fit,SplitAt),Test=Test,AlphaBetaGamma=c(out$p1,out$p2,out$p3)))
}