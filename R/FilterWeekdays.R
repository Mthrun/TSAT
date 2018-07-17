FilterWeekdays=function(Time,Datavector=NULL,PlotIt=FALSE){
  
  weekdays=weekdays(Time)
  
  ind=which(weekdays=="Samstag"|weekdays=="Sonntag")
  
  weeks=setdiff(1:length(Time),ind)
  
  if(!is.null(Datavector)){
    FilteredData=Datavector[weeks]
    if(PlotIt){
      plotEvaluationFilteredTS(Time,Datavector,FilteredData,TRUE)
    }
    
  }else
    FilteredData=NULL
  

  return(list(FilteredTime=Time[weeks],FilteredData=FilteredData))
}