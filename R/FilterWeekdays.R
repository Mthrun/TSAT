FilterWeekdays=function(Time,Datavector=NULL,EnglishOrGerman=FALSE,PlotIt=FALSE){
  
  weekdays=weekdays(Time)
  
  
  if(EnglishOrGerman){
    ind=which(weekdays=="Saturday"|weekdays=="Sunday")
  }else{
    ind=which(weekdays=="Samstag"|weekdays=="Sonntag")
  }
  
  
  weeks=intersect(1:length(Time),ind)
  
  if(!is.null(Datavector)){
    FilteredData=Datavector[weeks]
    if(PlotIt){
      plotEvaluationFilteredTS(Time,Datavector,FilteredData,TRUE)
    }
    
  }else
    FilteredData=NULL
  
  
  return(list(FilteredTime=Time[weeks],FilteredData=FilteredData))
}