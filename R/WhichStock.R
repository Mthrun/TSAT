WhichStock=function(TickerSymbols,MatchingTableNames='BoerseFR'){
  Data=TSAT::GermanPrimes
  if(!is.vector(TickerSymbols)){
    warning('"TickerSymbols" is not a vector, calling as.vector...')
    TickerSymbols=as.vector(TickerSymbols)
  }
  if(MatchingTableNames=='BoerseFR'){
    Key=Data$Trading.Symbol
  }
  if(MatchingTableNames=='yahoo'){
    Key=Data$Yahoo
  }
  n=length(TickerSymbols)
  m=setdiff(x = TickerSymbols,Key)
  if(length(m)!=0){
    b=paste(m,collapse = ', ')
    warning(paste('The following "TickerSymbols" couls not be found:',b,collapse = ' '))
  }
  ind=match(x = TickerSymbols,table=Key)
  #if(sum(!is.finite(ind))>0) warning('Some Ticker/Symbols could not be found')
  ind=ind[is.finite(ind)]
  return(Data[ind,c(1,2,3,4,5)])
}