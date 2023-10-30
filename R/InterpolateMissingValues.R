# vals=InterpolateMissingValues(Time,Datavector,option = 'stine',PlotIt=TRUE)
#
# DESCRIPTION:
# Interpolates missing values, by setting them NaN and then applying spline interpolation

#
# INPUT
# Time               [1:n] vector of time, POSIXlt or POSIXct are accepted
# Datavector         [1:n] numerical vector of data
# OPTIONAL
# option             See imputeTS::na_interpolation. Default is "stine"
# PlotIt             TRUE: Evaluates output of function versus input by plots. Default is TRUE
#
# OUTPUT
# [1:n] interpolated vector of data
#
# DETAILS:
# Wrapper for TSAT::InterpolateOutliers. Missing values are all values which are not is.finite(Datavector)==TRUE.
#
#
# Author: MCT

InterpolateMissingValues=function(Time,Datavector,option = 'stine',PlotIt=TRUE) {

  ind=which(!is.finite(Datavector))
  
  return(InterpolateOutliers(Time=Time,Datavector=Datavector,OutliersTime=Time[ind],option = option,PlotIt=PlotIt))
  
}