InterpolateMissingValues=function(Time,Datavector,option = 'stine',PlotIt=TRUE){

  ind=which(!is.finite(Datavector))
  
  return(InterpolateOutliers(Time=Time,Datavector=Datavector,OutliersTime=Time[ind],option = option,PlotIt=PlotIt))
  
}