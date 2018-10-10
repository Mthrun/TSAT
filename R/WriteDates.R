WriteDates=function(FileName, TSdata, Key=c(), OutDirectory=getwd(),  Comments=NULL){
  #based on ReadLRN
  #author: MT 2018
  checkFilename(FileName,Directory=OutDirectory,Extension='csv',ReadOrWrite=FALSE,NameOfFunctionCalled='WriteDates()')
  requireNamespace('dplyr')
  requireNamespace('tibble')
  CurrentDir = getwd()
  setwd(OutDirectory)
  FileName = addext(FileName,'csv')
  
  if(!tibble::is.tibble(TSdata)){
    warning("TSdata is not tibble. Please make sure that one column is a 'Date' column. Calling as.tibble...")
    TSdata=tibble::as.tibble(TSdata)
  }
  types=dplyr::summarise_all(TSdata,class)
  dind=which(types=="Date")
  
  if(length(dind)==0){
    warning('No Date type column found.')
    dind=0
  }
  if(length(dind)>1){
    warning('More than one Date type column found. Using the first one')
    dind=dind[1]
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
  
  if(dind==0){
    TibbleDF = cbind(Key=Key, TSdata)
  }else{
    TibbleDF = cbind(Key=Key, Time=TSdata[,dind],TSdata[,-dind])
    if(sum(!is.finite(TibbleDF$Time))==length(TibbleDF$Time)) 
      stop('Time has a format which as.Date does not recognize.')
    else
      if(sum(!is.finite(TibbleDF$Time))) warning(paste(sum(!is.finite(TibbleDF$Time)),"lines of the Time feature are missing values.")) 
  }
  
  if(length(TibbleDF$Time)!=length(unique(TibbleDF$Time))) warning('Time is not unique meaning that there are multiple days with same date.')
  
  orderedtime=order(TibbleDF$Time,decreasing = FALSE,na.last = NA)
  if(length(TibbleDF$Time)!=length(orderedtime)) warning('"Time" has NA dates, they are not removed.')
  if(!identical(TibbleDF$Time,TibbleDF$Time[orderedtime])) warning('"Time" was not ordered from past to future. "Time" and Data is not reordered accordingly.')
  
  HeaderRaw=colnames(TibbleDF)
  Header=HeaderRaw
  Header=gsub('\\.','',Header)
  Header=gsub(' ','',Header)
  Header=gsub('\t','',Header)
  Header=gsub(pattern = '[ä]', replacement = "ae",Header)
  Header=gsub(pattern = '[ü]', replacement = "ue",Header)
  Header=gsub(pattern = '[ö]', replacement = "oe",Header)
 
  if(!identical(Header,HeaderRaw)) warning('Header (ColumnNames) had either spaces, points, or german umlaute which were replaced.')
  
  header = c(paste('%\t',Rows),paste('%\t',Cols))
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