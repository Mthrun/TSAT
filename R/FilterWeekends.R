# FilterWeekends = FilterWeekends(Time, Datavector=NULL, EnglishOrGerman=FALSE, PlotIt=FALSE){
#
# Description:
# Filters out weekends.
#
# INPUT
# Time              Vector of Time values
#
# OPTIONAL
# Datavector        Vector of Data. Default is NULL.
# EnglishOrGerman   Boolean, TRUE if German days as index, FALSE if English. Default is FALSE.
# PlotIt            Boolean, TRUE if plot should be printed, FALSE else. Default is FALSE.
#
# OUTPUT
# list with:
# FilteredTime      Vector of Time values with weekdays removed.
# FilteredData      If Datavector given a vector of Data with those corresponding to weekdays removed.
#
# Author: 

FilterWeekends = function(Time, Datavector=NULL, EnglishOrGerman=FALSE, PlotIt=FALSE){
  
  weekdays=weekdays(Time)
  
  if(EnglishOrGerman) {
    ind = which(weekdays=="Saturday"|weekdays=="Sunday")
  } else {
    ind = which(weekdays=="Samstag" |weekdays=="Sonntag")
  }

  
  weeksends = setdiff(1:length(Time), ind)
  
  if(!is.null(Datavector)){
    FilteredData = Datavector[weeksends]
    if(PlotIt){
      plotEvaluationFilteredTS(Time, Datavector, FilteredData, TRUE)
    }
    
  } else {
    FilteredData = NULL
  }
  
  
  return(
    list(FilteredTime=Time[weeksends], FilteredData=FilteredData))
}