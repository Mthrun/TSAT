FilterWeekdays=function(Time,Datavector=NULL){
  
  weekdays=weekdays(Time)
  
  ind=which(weekdays=="Samstag"|weekdays=="Sonntag")
  
  weeks=setdiff(1:length(Time),ind)
  
  if(!is.null){
    FilteredData=Datavector[ind]
  }else
    FilteredData=NULL
  
  return(list(FilteredTime=Time[weeks],FilteredData=FilteredData))
}