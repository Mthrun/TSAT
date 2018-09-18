GetFinancialStatement=function(Symbol='SAP',URL='morningstar'){
  #GetFinancialStatement('SAP')
  
  library(tidyverse)
  library(rvest)
  if(URL=='yahoo'){
  url=paste0("https://finance.yahoo.com/quote/",Symbol,"/financials?p=",Symbol)


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
    Time=rownames(Features)
    strings=c(4,11,19,24)
    if(sum(is.na(suppressWarnings(as.numeric(Features[,strings]))))==length(as.character(Features[,strings]))){
      Features=Features[,-strings]
      Features[which(Features=='-')]=NaN
      Features=apply(Features,2,as.numeric)
      rownames(Features)=Time
    }else{
      warning('unexpected numerical input in string lines.')
    }
  }else{
    warning('Number of columns unexpected, something went wrong...')
  }
  # Features=cbind(Time=as.character(a$Time),data.frame(Features))
  return(Features)
  }
  if(URL=='investing'){
    url='https://www.investing.com/equities/sap-ag-income-statement'
  }
  if(URL=='msn'){
    url='https://www.msn.com/en-us/money/stockdetails/financials/fi-126.1.SAP.NYS'
  }
  if(URL=='morningstar'){
    url=paste0('http://financials.morningstar.com/ajax/ReportProcess4CSV.html?t=XFRA:',Symbol,'&reportType=is&period=3&dataType=A&order=asc&denominatorView=raw&columnYear=5&number=3')
    
    tryCatch({
    raw=read.csv(file=url,header = T,sep = ',',skip = 1,stringsAsFactors = F)
    DF=raw[,c(2:6)]
    Header=raw[,1]
    Time=gsub('X','',colnames(raw)[2:6])
    DF=t(DF)
    colnames(DF)=Header
    rownames(DF)=Time
    DF[!is.finite(DF)]=NaN
    
    # DF=DF[,setdiff(1:ncol(DF),c(4,19,22))]
    return(DF)
    }, error=function(e) return(NULL))
  }
}