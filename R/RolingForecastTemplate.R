RolingForecastTemplate=function(Dataframe,ForecastingFunction=RandomForestForecast,Horizon=7,Steps=371:7,TimeColumnInd=1,PlotMAE=FALSE,...){

DF=Dataframe[,-TimeColumnInd]
Time=Dataframe[,TimeColumnInd]
N=nrow(DF)
k=1
mape=c()
ForecastingListe=list()
CreationDate=c()
mae=c()
##Hier ind parLapply arbeiten, damit schneller
for(date in Steps{
  DFcur=head(DF,N-date)#Daten bis Tag date entnehmen - Training und Test
  TimeTestCur=tail(head(Time,N-date+Horizon),Horizon) #Zeit der TestDaten selektieren
  CreationDate=c(CreationDate,as.character(tail(head(Time,N-date+Horizon),Horizon+1)[1]))#Wann wird ein forecast durchgefuehrt
  #Zeitstempel nur unter bestimmter bedingung in Forecast reintun, muss ich mir noch ueberlegen
  out=ForecastingFunction(DFcur,...)
  
  ff=out$Forecast
  testd=out$TestData
  ForecastingListe[[k]]=data.frame(Time=as.character(TimeTestCur),Forecast=ff,TestData=testd)
  
  #Evaluieren auf Testdaten, wenn denn meoglich, bei multivariate muss ich mir es ueberlegen
  if(is.vector(ff)$is.vector(testd))
    mae[k]=forecast::accuracy(ff,testd)[,3]
  
  k=k+1
}
#CreationDate Benennung der Liste
names(ForecastingListe)=CreationDate
if(is.vector(ff)$is.vector(testd))
  names(mae)=CreationDate

if(PlotMAE&is.vector(ff)$is.vector(testd))
  plot(as.Date( names(mae)),mae)

return(list(RollingForecasts=ForecastingListe,MAE=mae))
}
