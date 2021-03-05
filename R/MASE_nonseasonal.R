MASE_nonseasonal=function(Y,For,epsilon=10^-5,NonDuplicatedForecasts=TRUE){
#   \title{
#     MASE_nonseasonal
#   }
#   \description{
#     Mean absolute scaled error (MASE)
#   }
#   \usage{
#     MASE_nonseasonal(Y,For)
#   }
#   %- maybe also 'usage' for other objects documented here.
#   \arguments{
#     \item{Y}{
#       numerical vector of [1:n],n>1
#     }
#     \item{For}{
#       numerical vector of [1:n],n>1
#     }
#     
#   }
#   \details{
#     The mean absolute scaled error has the following desirable properties:
#       
#       1.)	Symmetry
# error penalizes positive and negative forecast errors equally
# 
# penalizes errors in large forecasts and small forecasts equally
# 
# 2.)
# Predictable behavior as y t->0: Percentage forecast accuracy measures such as the Mean absolute percentage error (MAPE) rely on division of y t skewing the distribution of the MAPE for values of y t  near or equal to 0. This is especially problematic for data sets whose scales do not have a meaningful 0, such as temperature in Celsius or Fahrenheit, and for intermittent demand data sets, where y t = 0 occurs frequently.
# 
# 3.)
# Interpretability: The mean absolute scaled error can be easily interpreted, as values greater than one indicate that in-sample one-step forecasts from the naïve method perform better than the forecast values under consideration.
# 
# 4.)
# Asymptotic normality of the MASE: The Diebold-Mariano test for one-step forecasts is used to test the statistical significance of the difference between two sets of forecasts. To perform hypothesis testing with the Diebold-Mariano test statistic, it is desirable for DM ~ N(0,1), where DM is the value of the test statistic. The DM statistic for the MASE has been empirically shown to approximate this distribution, while the mean relative absolute error (MRAE), MAPE and sMAPE do not.
# 
# 
#   }
#   \value{
#     MASE error value
#   }
#   \references{
#     Hyndman, R. J. (2006). "Another look at measures of forecast accuracy", FORESIGHT Issue 4 June 2006, p. 46
#   }
#   \author{
#     Michael Thrun
#   }
#   
#   
#   \examples{
#     #usage with vectors, x forecast, y test data
#     x=c(100,110,95)
#     y=c(90,100,93)
#     MASE_nonseasonal(x,y)
#     
#   }
  if(isTRUE(NonDuplicatedForecasts)){
    points=cbind(Y,For)
    ind=!duplicated(points)
    Y=Y[ind]
    For=For[ind]
  }
  n=length(Y)
  e=abs(Y-For)
  nenner=abs(Y[2:n]-Y[1:(n-1)])
  
  unten=(1/(n-1)*sum(nenner))
  oben=(1/n*sum(e))
  
  if(unten>epsilon){ #nennen ist numerisch i.O.
    mase=oben/unten
  }else{  #nennen geht gegen null
    if(abs(oben-unten)<epsilon) #zaehler ist auch null => gleich gut zu naiv
      mase=1
    else#zaehler ist nicht null, nehme maximale Genauigkeit aber nicht inf!
      mase=1/epsilon
  }
    
  return(mase)
}