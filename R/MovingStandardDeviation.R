MovingStandardDeviation = function(Datavec, lag,Robust=FALSE,na.rm=TRUE){
#
# INPUT
# Datavec[1:n,1:d]        time series as Datavector
# lag                     nr or previous Datavec points to average
# Robust                  FALSE: calse sd, TRUE: calls robust variance estimation
# na.rm                   remove nans, works only with sd
# OUTPUT
# FilteredData[1:n,1:d]   the filtered Datavector
# author: MCT

AnzDatavec = lenght(Datavec)
MovingStandardDeviation=Datavec*NaN
for(i in 1:AnzDatavec){
Ind = c((i-lag):(i+lag))
Ind[Ind<1] = 1
Ind[Ind>AnzDatavec] = AnzDatavec 
if(Robust)
  MovingStandardDeviation[i] = dbt.Statistics::stdrobust(Datavec[Ind])
else
  MovingStandardDeviation[i] = sd(Datavec[Ind],na.rm = na.rm)
}
 return(MovingStandardDeviation)                                   
}