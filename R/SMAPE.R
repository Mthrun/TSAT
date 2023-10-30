# V=(X,Y,epsilon=10^-10,na.rm=FALSE,Silent=TRUE)
#
# DESCRIPTION
# Calculates the relative difference between X (forecast) and y (historical data) [Armstrong,1985].
# Beware:Averaging has to be done by the user!
#
# INPUT
# X                     Either a value or numerical vector of [1:n]
# Y                     Either a value or numerical vector of [1:n]
# OPTIONAL
# epsilon               If both x and y are approximately zero the output is also zero. Default is 10^-10
# na.rm                 Function does not work with non finite values. If these cases should be automatically removed, 
#                       set parameter TRUE. Default is FALSE
# Silent                Optional, TRUE: No Warnings or errors are given back. Default is FALSE
#
# DETAILS
# This function was taken from DatabionicSwarm::RelativeDifferences and slightly adjusted: 
# The nominator is contrary to [Ultsch, 2008] in absolute values of X and Y resulting in the problem that 
# SMAPE ist not symmetric regarding different forecasts since over- and under-forecasts are not treated equally 
# (see example for further details). 
# Contrary to other approaches in this cases the range of values lies between [-100,100] in percent. 
# The approach is only valid for positive and negative values of X and Y.
# The relative difference R is defined with SMAPE = 100/n * (abs(Y-X)/(abs(X)+abs(Y)))
# Negative value indicate that X is higher than Y and positive values that X is lower than Y.
#
#
# author MCT


SMAPE=function(X,Y,epsilon=10^-10,na.rm=FALSE,Silent=TRUE){
# Symmetric mean absolute percentage error
# Armstrong, J. S. (1985) Long-range Forecasting: From Crystal Ball to Computer, 2nd. ed. Wiley. ISBN 978-0-471-82260-8
# Ultsch, A.: Is Log Ratio a Good Value for Measuring Return in Stock Investments? GfKl 2008, pp, 505-511, 2008.
  
  
  if(isTRUE(na.rm)){ #achtung irgendwas stimmt hier nicht
    noNaNInd <- which(is.finite(X)&is.finite(Y))
    X <- X[noNaNInd]
    Y <- Y[noNaNInd]
  }
  if(!isTRUE(Silent)){
    if(length(X)!=length(Y)) stop('Length of X and Y do not match.')
    if(length(X)>1) return(sum(mapply(X,FUN = SMAPE,Y))/length(X))
  }
  oben=abs(Y-X)
  #idee: eigentlich muesste man negative und positive betraege getrennt aufsummieren und betrachten, vielleicht als wurzel und dann als komplexe zahl
  unten=abs(X)+abs(Y)
  if(!isTRUE(Silent)){
    if(!is.finite(unten)) stop('Some of your values are not finite. Please use na.rm=TRUE')
  }
  
  if(any(abs(unten)<epsilon)){
    if(!isTRUE(Silent)){
      warning('X and Y are too small to calcualte Relative Differences. Returning 0')
    }
    ind=which(abs(unten)<epsilon)
    Rel=100*oben/unten
    Rel[ind]=0
    return(Rel)
  }else{
    return(100*oben/unten)
  }
  

  #  smooth::SMAPE(actual, forecast, digits = 3) # does not use same formula...
}