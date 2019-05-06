ConvertTS2DF=function(TSobject,Resolution='Monthly',TimeZone='UCT'){
  requireNamespace('zoo')
  switch(Resolution,Monthly={
    date=zoo::yearmon(time(TSobject))
    charpos=strptime(paste(1,as.character(date)),'%d %b %Y',tz = TimeZone)
    Dates=as.Date(charpos)
  },
  Quarterly={
    Dates=zoo::yearqtr(time(TSobject))
  },
  Yearly={
    date=as.character(time(TSobject))
	charpos=strptime(paste(1,'01',date),'%d %m %Y',tz = TimeZone)
	Dates=as.Date(charpos)
  },{
    stop('Resolution currently not implemented')
  })

 
  DF=data.frame(Time=Dates,Data=zoo::coredata(TSobject))
  return(DF)
}