InterpolateOutliers=function(Time,Datavector,OutliersTime,option = 'stine',PlotIt=TRUE){
#  #boolean=IdentifyOutliers(ElectricityBRD$Mrd_KWh,ElectricityBRD$Time,n_sigmas = 1,PlotIt = T)
# OutliersTime=ElectricityBRD$Time[boolean]
# vals=InterpolateOutliers(ElectricityBRD$Time,ElectricityBRD$Mrd_KWh,OutliersTime,option = 'stine',PlotIt=TRUE)
#
# DESCRIPTION:
# Interpolates outliers values, by setting them NaN and then applying spline interpolation

#
# INPUT
# Time               [1:n] vector of time, POSIXlt or POSIXct are accepted
# Datavector         [1:n] numerical vector of data
# OutliersTime       [1:k] vector of time of outliers, in the format of the Time values
# OPTIONAL
# option             See imputeTS::na_interpolation. Default is "stine"
# PlotIt             TRUE: Evaluates output of function versus input by plots. Default is TRUE
#
# OUTPUT
# [1:n] interpolated vector of data
#
#
# Author: MCT
# Details: Example shows, that it only makes sense if outliers are identified correctly

  
  DataBefore=Datavector
  ind=Time %in% OutliersTime
  Datavector[ind]=NaN
  
  # DataAfter=imputeTS::na.interpolation(Datavector,option = 'spline')
  # DataAfter[ind]=NaN
  DataAfter=imputeTS::na_interpolation(Datavector,option = option)
  
  if(PlotIt){
    plotEvaluationFilteredTS(Time=Time,DataBefore=DataBefore,DataAfter=DataAfter,MarkedPoints=ind,Short=TRUE)
  }
  return(invisible(DataAfter))
}