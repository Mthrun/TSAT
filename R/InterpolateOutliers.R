InterpolateOutliers=function(Time,Datavector,OutliersTime,option = 'stine',PlotIt=TRUE){
  
  DataBefore=Datavector
  ind=Time %in% OutliersTime
  Datavector[ind]=NaN
  
  # DataAfter=imputeTS::na.interpolation(Datavector,option = 'spline')
  # DataAfter[ind]=NaN
  DataAfter=imputeTS::na.interpolation(Datavector,option = option)
  
  if(PlotIt){
    plotEvaluationFilteredTS(Time,DataBefore,DataAfter,MarkedPoints=ind)
  }
  return(invisible(DataAfter))
}