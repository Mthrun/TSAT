# tsobject = ConvertTS2DF(TSobject, Resolution = "Monthly", TimeZone = "UCT")
#
# Description:
# Converts a R time series to an R data.frame
#
# INPUT
# TSobject                time series (ts) object
# Resolution              Monthly or Quarterly
# TimeZone                See strptime. A empty string "" represents the current time zone
#
# OUTPUT 
# DF                      Dataframe with Time and Data columns
#
#
# Author: MCT


ConvertTS2DF=function(TSobject,Resolution='Monthly',TimeZone='UCT'){
  requireNamespace('zoo')
  switch(Resolution,Monthly={
    date=zoo::yearmon(time(TSobject))
    charpos=strptime(paste(1,as.character(date)),'%d %b %Y',tz = TimeZone)
    Dates=as.Date(charpos,tz=TimeZone)
  },
  Quarterly={
    Dates=zoo::yearqtr(time(TSobject))
  },
  Yearly={
    date=as.character(time(TSobject))
	charpos=strptime(paste(1,'01',date),'%d %m %Y',tz = TimeZone)
	Dates=as.Date(charpos,tz=TimeZone)
  },{
    stop('Resolution currently not implemented')
  })

 
  DF=data.frame(Time=Dates,Data=zoo::coredata(TSobject))
  return(DF)
}