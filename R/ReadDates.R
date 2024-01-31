ReadDates=function(FileName=NULL,InDirectory=getwd(),Silent=TRUE,tz="UTC"){
#V=ReadDates(FileName = NULL, InDirectory = getwd(), Silent = TRUE)
# read univariate or multivariate time series similar to the LRN format
  # INPUT
  #   \item{FileName}{
  #     string, name of the  file to be written
  #   }
  #   \item{InDirectory}{
  #     Optional: string, name of directory the data will be saved in, default \code{getwd()} 
  #     
  #   }
  #   \item{Silent}{
  #     If FALSE: Comments are not printed out
  #   }
  #
  #
  #   Internally the tibble format is used. If it can be transformed a list is given back as output as defined below. If not, the tibble frame is directly given back as output.
  # 
  # OUTPUT
  #   a list with following elements:
  #   \item{Time}{ time in as.Date() form of n rows and 1 column}
  #   \item{Data}{ a numeric matrix containing n rows and d columns}
  #   \item{Key}{  a numeric vector of length(n)  }
  #   \item{Header}{ d column names for Data}
  #   
  #based on ReadLRN
  #author: MT 2018
  Comments=NULL
  Key=NULL
  Header=NULL
  
  checkFilename(FileName,Directory=InDirectory,Extension='csv',ReadOrWrite=TRUE,NameOfFunctionCalled='ReadDates()')
  currentWD=getwd() #Aktuelles verzeichnis merken
  requireNamespace('dplyr')
  requireNamespace('tibble')
  setwd(InDirectory)
  
  FileName  = addext(FileName,'csv');     #  checks for the right extension and adds it if necessary
  # see if the diretory is there and has a canonical name
  InDirectory = normalizePath(InDirectory);
  setwd(InDirectory) #Ins Verzeichnis wo sich Datei befindet wechseln

  ColNameFlag=T
  tryCatch({
    check=T
    beginHeader = 1
    while(check){
      Header = readLines(FileName,n=beginHeader)[beginHeader]
      if(gregexpr('%',Header)[1]==1){#Prozentzeichen muss als erstes Zeichen einer Zeile kommen
        check=F
      }else{
        beginHeader = beginHeader+1 #Hier muss ich das dynamisch anpassen
      }
    }
    
    if(beginHeader>1){
      Comments=as.matrix(read.table(FileName, comment.char = "%", header=FALSE,  fill=TRUE, stringsAsFactors = FALSE, na.strings=c('NA','NaN'),nrows=beginHeader-1,blank.lines.skip=T))
    }else{
      Comments=NULL
    }
    if(!any(grepl('#',Comments))){
      Comments=NULL
    }
    try(
      if(!is.null(Comments)){
        Comments=sub('# ','',Comments)
        Comments=sub('#','',Comments)
        Comments=sub('\t','',Comments)
        Comments=paste(apply(Comments,1,paste,collapse=" "), collapse=" ")
      }
    )
    
    HeaderLines=readLines(FileName,n=beginHeader+4)
    ZahlPattern = "[0-9]+"
    atomicVectorInd=regexpr(ZahlPattern, HeaderLines[beginHeader:(beginHeader+1)])
    
    StartRow = atomicVectorInd[1]
    StartCol = atomicVectorInd[2]
    EndRow = StartRow+attributes(atomicVectorInd)$match.length[1]-1
    EndCol = StartCol+attributes(atomicVectorInd)$match.length[2]-1
    rows=as.numeric(substr(HeaderLines[beginHeader],StartRow,EndRow))
    cols=as.numeric(substr(HeaderLines[beginHeader+1],StartCol,EndCol))
    
    # DataDefinedPatternStarts = "[0-9]+"
    # DataDefinedInd=gregexpr(DataDefinedPatternStarts, HeaderLines[beginHeader+2])
    # Laengen = attributes(DataDefinedInd[[1]])$match.length
    # Starts =   unlist(DataDefinedInd)
    # Last = Starts+Laengen-1
    # DataDefined=as.numeric(substring(HeaderLines[beginHeader+2],Starts,Last))
    
    Line=HeaderLines[beginHeader+2]
    Line=sub('%','',Line)
    
    HeaderPatternStarts = "([[:alnum:]]|[[:punct:]])+"
    HeaderInd=gregexpr(HeaderPatternStarts,Line )
    Laengen = attributes(HeaderInd[[1]])$match.length
    Starts =   unlist(HeaderInd)
    Last = Starts+Laengen-1
    Header=substring(Line,Starts,Last)
    
  },stop=function(f){
    warning(f)
    stop("ReadDates: Header or Comments are not reasonably defined, see DataIO::ReadLRN for further instructions")
  }
  )
  # print(rows)
  # print(cols)
  # print(Header)
  zwei=length(Header)
  
  if(zwei!=cols){
    warning(paste('ReadDates: Length of Header',zwei,'does not equal number of columns',cols))
    ColNameFlag=F
  }
  
  keyind=which(Header=='Key')
  if(length(keyind)==0){
    warning("ReadDates: Column with name 'Key' not found. Assuming that the key is stored in the first column.")
    keyind=1
  }
  TimeColumnName=Header[2]
  #deltes key name
  Header=Header[-1]
  tryCatch({
    Z= scan(FileName,skip = beginHeader+2,sep='\t',what=character(0), quiet=T)
    DataKey=matrix(Z,nrow=rows,ncol=cols,byrow=T)
    Key=as.vector(DataKey[,keyind])
    Data=DataKey[,-keyind]
  },warning=function(e){
    warning(e)
    warning('ReadDates: Please check row and column numbers, Trying to bypass error...')
    Z = read.table(FileName, comment.char = "%", header=FALSE,  fill=TRUE, stringsAsFactors = FALSE, na.strings=c('NA','NaN'),skip=beginHeader+2,sep = '\t')
    Key=as.vector(Z[,keyind])
    Data=Z[,-keyind]
  },stop=function(f){
    warning(f)
    stop("ReadDates: Header or Comments or Data are not reasonably defined, see Subversion/PUB/ZFileFormatDocuments for further instructions")
  }
  )
  if(!is.null(Key))
    rownames(Data)=Key
  else
    warning('ReadDates: Key is missing, Check data.')
  
  tind=which(Header=='Time')
  CheckTimename=F
  if(length(tind)==0){
    if(isFALSE(Silent))
      message("ReadDates: Column with name 'Time' not found. Assuming that the time is stored in the second column.")
    
    tind=1
    CheckTimename=T
  } 
  DF=tibble::as_tibble(Data[,-tind])
  DF=lapply(DF,type.convert, as.is=TRUE)
  DF=tibble::as_tibble(DF) 
  Time=as.Date(Data[,tind])
  
  #print(str(Time))
  colnames(DF)=Header[-tind]
  if(sum(!is.finite(Time))==length(Time)) 
    stop('Time has a format which as.Date does not recognize.')
  else
    if(sum(!is.finite(Time))) warning(paste("ReadDates:",sum(!is.finite(Time)),"lines of the Time feature are missing values.")) 
  
  if(length(Time)!=length(unique(Time))) warning('ReadDates: Time is not unique meaning that there are multiple days with same date.')
  
  
  Time=tibble::as_tibble(Time)
  TibbleDF=dplyr::bind_cols(Time,DF)
  colnames(TibbleDF)=Header
if(isFALSE(CheckTimename)){
  orderedtime=order(TibbleDF$Time,decreasing = FALSE,na.last = NA)
  if(length(TibbleDF$Time)!=length(orderedtime)) warning('ReadDates: "Time" has NA dates, they are not removed.')
  if(!identical(TibbleDF$Time,TibbleDF$Time[orderedtime])) warning('ReadDates: "Time" was not ordered from past to future. "Time" and Data is not reordered accordingly.')
}else{
  TibbleDF_temp=as.data.frame(TibbleDF)
 orderedtime=order(TibbleDF_temp[,1],decreasing = FALSE,na.last = NA)
 if(length(TibbleDF_temp[,1])!=length(orderedtime)) warning('ReadDates: "Time" has NA dates, they are not removed.')
if(!identical(TibbleDF_temp[,1],TibbleDF_temp[orderedtime,1])) warning('ReadDates: "Time" was not ordered from past to future. "Time" and Data is not reordered accordingly.')

}  
  if(isFALSE(Silent)){
    if(!is.null(Comments)){
      message('ReadDates: The follwing comments were in the file:')
      message(Comments)
    }
  }
  
  setwd(currentWD)
  try({
  Time=as.Date((as.matrix(TibbleDF[,1])),tz=tz)
  Data=as.matrix(TibbleDF[,2:ncol(TibbleDF)])
  #mode(Data)="numeric"
  #delete time name from header
  Header=Header[-1]
  V=list(Time=Time,Data=Data,Key=as.numeric(Key),Header=Header,Comments=Comments)
  if(isTRUE(CheckTimename))
  names(V)[1]=TimeColumnName
  
  return(V)
  
  })
  return(TibbleDF)
}