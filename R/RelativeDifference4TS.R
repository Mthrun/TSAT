RelativeDifference4TS=function(Datavec,Lag=1,na.rm=FALSE,PlotIt=FALSE,Time){
#V=RelativeDifference4TS(ElectricityBRD$Mrd_KWh,PlotIt=TRUE,Time=(ElectricityBRD$Time))
#Calculates the relative difference between positive Datavec and and lagged Datavec
#INPUT
#Datavec  numerical vector of [1:n]
#Lag      The number of lags (in units of observations): back shifting: positiv value,forward shifting: negative value
#         Default is the difference to yesterday
#na.rm    FALSE; do nothing, TRUE: all nan to zero
# PlotIt  TRUE: plots data
#OUPUT
# Relativedifference numerical vector of [1:n], Negative value indicate that today is lower than yesterday and positive values that todays value is higher than yesterdays.
#Details: Contrary to linear filters in this cases the range of values lies always between [-2,2]
#Author: MCT, 2023
  
   neg=which(Datavec<0)
  if(length(neg)>0){
    Datavec[neg]=0
    warning("RelativeDifference4TS works only for positive values, negatives are set to zero per default which my be incorrect.")
  }
  nan_ind=which(!is.finite(Datavec))
  
  if(length(nan_ind)>0){
    if(isTRUE(na.rm))
      Datavec[nan_ind]=0
    warning("RelativeDifference4TS works only for positive values correctly but missing values were found. These cases are automatically removed.")
  }
  
 Today=Datavec
  Yesterday=LagVector(Datavec,k=1)
  
  #no nan
  if(Lag>0){
    Yesterday[1:Lag]=Yesterday[Lag+1]
  }
  if(Lag<0){
    indNan=tail(1:length(Yesterday),Lag)
    Yesterday[indNan]=Yesterday[min(indNan)-1]
  }
  
    Rel=DatabionicSwarm::RelativeDifference(Y = Today,X=Yesterday,na.rm = F)
  
    if(isTRUE(na.rm))
      Rel[!is.finite(Rel)]=0
    
  if(isTRUE(PlotIt)){
    if(missing(Time)){
      Time=1:length(Datavec)
    }
    Name=deparse1(substitute(Datavec))
    a=DataVisualizations::DualaxisLinechart(Time,Datavec,Rel,y1lab = Name,y2lab = paste0("RelDiff of ",Name," and its ","Lag = ",Lag),xlab = 'Time')
    print(a)
  }
  return(Rel)
}

