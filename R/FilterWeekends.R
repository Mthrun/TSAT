FilterWeekends=function(Time,Datavector=NULL,EnglishOrGerman=FALSE,PlotIt=FALSE){
  
  weekdays=weekdays(Time)
  
  if(EnglishOrGerman){
    ind=which(weekdays=="Saturday"|weekdays=="Sunday")
  }else{
    ind=which(weekdays=="Samstag"|weekdays=="Sonntag")
  }

  
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