ConvertTS2DF=function(TSobject,Resolution='Monthly',TimeZone='UCT'){
  requireNamespace('zoo')
  switch(Resolution,Monthly={
    date=zoo::yearmon(time(TSobject))
    char=strptime(paste(1,as.character(date)),'%d %b %Y',tz = TimeZone)
    Dates=as.Date(char)
  },
  Quarterly={
    Dates=zoo::yearqtr(time(TSobject))
  },{
    stop('Resolution currently not implemented')
  })

 
  DF=data.frame(Time=Dates,Data=zoo::coredata(TSobject))
  return(DF)
}