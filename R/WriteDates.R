WriteDates=function(FileName, TSdata, Key=c(), OutDirectory=getwd(),  Comments=NULL){
  #based on ReadLRN
  #author: MT 2018
  checkFilename(FileName,Directory=OutDirectory,Extension='csv',ReadOrWrite=FALSE,NameOfFunctionCalled='WriteDates()')
  
  CurrentDir = getwd()
  setwd(OutDirectory)
  FileName = addext(FileName,'csv')
  
  if(!is.tibble(TSdata)){
    warning("TSdata is not tibble. Please make sure that one column is a 'Date' column. Calling as.tibble...")
    TSdata=as.tibble(TSdata)
  }
  types=summarise_all(TSdata,class)
  dind=which(types=="Date")
  if(length(dind)==0){
    stop('No Date column found.')
  }
  RowsCols=dim(TSdata)
  Rows=RowsCols[1]
  Cols=RowsCols[2]+1 #Key counts as column
  if(missing(Key)|is.null(Key)){
    Key = rownames(TSdata)
  }
  if(length(Key) != Rows){
    
    warning('Key length is inconsistent with data length, new key is generated')
  }
  if(length(unique(Key))!=length(Key)) stop('Key is not unique')
  
  TibbleDF = cbind(Key=Key, Time=TSdata[,dind],TSdata[,-dind])
  Header=colnames(TibbleDF)
  
  header = c(paste('%\t',Rows),paste('%\t',Cols))
  #MT:
  if(is.character(Comments)){
    write.table(paste0('# ',Comments), FileName, quote=FALSE, row.names=FALSE, col.names=FALSE, na='NaN')
    write.table(header, FileName,append=TRUE, quote=FALSE,sep='\t', row.names=FALSE, col.names=FALSE, na='NaN')
  }else{ 
    write.table(header, FileName, quote=FALSE, row.names=FALSE, col.names=FALSE, na='NaN')
  }
  
  
  # write 'Header'-line
  cat('% ', file=FileName, append=TRUE)
  for(i in 1:length(Header))
    Header[i]=sub(' ','',Header[i]) #Blanks ersetzen
  cat(Header,'\n', file=FileName, append=TRUE, sep='\t')
  
  # write data
  write.table(TibbleDF, file=FileName, append=TRUE, quote=FALSE, sep='\t', row.names=FALSE, col.names=FALSE, na='NaN')
  
  setwd(CurrentDir)
}