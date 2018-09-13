GetFinancialStatement=function(Symbol){
  #GetFinancialStatement('SAP')
  
  url=paste0("https://finance.yahoo.com/quote/",Symbol,"/financials?p=",Symbol)

library(tidyverse)
library(rvest)
  #HTML parsen:
  out=read_html(url)
  out2=html_table(out)
  #Tiblemacht geschickt preprocessing
  raw= as.tibble(map_df(out2,bind_cols))
  
  
  n=nrow(raw)
  c=ncol(raw)
  for(i in 1:n){
    for(j in 1:c){
      raw[i,j]=gsub(',','',raw[i,j])
    }
  }
  Header=as.vector(as.matrix(raw[,1]))
  Time=as.vector(raw[1,2:c])
  Features=raw[2:n,2:c]
  colnames(Features)=Time
  Features=t(Features)
  colnames(Features)=Header[2:n] ##??
  # DFs=list()
  # for(i in 1:length(Time)){
  #   x=Features[,i]
  #   DFs[[i]]=
  # }
  

  n2=nrow(Features)
  c2=ncol(Features)
  if(c2==27){
    strings=c(4,11,19,24)
    if(sum(is.na(suppressWarnings(as.numeric(Features[,strings]))))==length(as.character(Features[,strings]))){
      Features=Features[,-strings]
      Features[which(Features=='-')]=NaN
      Features=apply(Features,2,as.numeric)
    }else{
      warning('unexpected numerical input in string lines.')
    }
  }else{
    warning('Number of columns unexpected, something went wrong...')
  }
  return(Features)
}