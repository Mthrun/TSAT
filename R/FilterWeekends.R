FilterWeekends=function(Time,Datavector=NULL,PlotIt=FALSE){
  
  weekdays=weekdays(Time)
  
  ind=which(weekdays=="Samstag"|weekdays=="Sonntag")
  
  weeksends=setdiff(1:length(Time),ind)
  
  if(!is.null(Datavector)){
    FilteredData=Datavector[weeksends]
    if(PlotIt){
      plotEvaluationFilteredTS(Time,Datavector,FilteredData,TRUE)
    }
    
  }else
    FilteredData=NULL
  

  return(list(FilteredTime=Time[weeksends],FilteredData=FilteredData))
}