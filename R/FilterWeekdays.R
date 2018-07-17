FilterWeekdays=function(Time,Datavector){
  
  weekdays=weekdays(Time)
  
  ind=which(weekdays=="Samstag"|weekdays=="Sonntag")
  
  weeks=setdiff(1:length(Time),ind)
  
  return(list(InweekTime=Time[weeks],InWeekData=Datavector[weeks]))
}