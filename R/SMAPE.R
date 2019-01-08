SMAPE =function(X,Y,epsilon=10^-10,na.rm=FALSE){
# Symmetric mean absolute percentage error
# Armstrong, J. S. (1985) Long-range Forecasting: From Crystal Ball to Computer, 2nd. ed. Wiley. ISBN 978-0-471-82260-8
# Ultsch, A.: Is Log Ratio a Good Value for Measuring Return in Stock Investments? GfKl 2008, pp, 505-511, 2008.
  
  
  if(isTRUE(na.rm)){ #achtung irgendwas stimmt hier nicht
    noNaNInd <- which(is.finite(X)&is.finite(Y))
    X <- X[noNaNInd]
    Y <- Y[noNaNInd]
  }
  if(length(X)!=length(Y)) stop('Length of X and Y do not match.')
  if(length(X)>1) return(sum(mapply(X,FUN = SMAPE,Y))/length(X))
  oben=abs(Y-X)
  #idee: eigentlich muesste man negative und positive betraege getrennt aufsummieren und betrachten, vielleicht als wurzel und dann als komplexe zahl
  unten=abs(X)+abs(Y)
  if(!is.finite(unten)) stop('Some of your values are not finite. Please use na.rm=TRUE')
  if(abs(unten)<epsilon){
    warning('X and Y are too small to calcualte Relative Differences. Returning 0')
    return(0)
  }
  return(100*oben/unten)
  #  smooth::SMAPE(actual, forecast, digits = 3) # does not use same formula...
}