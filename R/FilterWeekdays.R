FilterWeekdays=function(Time,Datavector=NULL){
  
  weekdays=weekdays(Time)
  
  ind=which(weekdays=="Samstag"|weekdays=="Sonntag")
  
  weeks=setdiff(1:length(Time),ind)
  
  if(!is.null(Datavector)){
    FilteredData=Datavector[weeks]
  }else
    FilteredData=NULL
  
  return(list(FilteredTime=Time[weeks],FilteredData=FilteredData))
}