ReadDates=function(FileName=NULL,InDirectory=getwd(),SilentComments=TRUE){
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
    stop("Header or Comments are not reasonably defined, see DataIO::ReadLRN for further instructions")
  }
  )
  # print(rows)
  # print(cols)
  # print(Header)
  zwei=length(Header)
  
  if(zwei!=cols){
    warning(paste('Length of Header',zwei,'does not equal number of columns',cols))
    ColNameFlag=F
  }
  
  keyind=which(Header=='Key')
  if(length(keyind)==0){
    warning("Column with name 'Key' not found. Assuming that the key is stored in the first column.")
    keyind=1
  }
  Header=Header[-1]
  tryCatch({
    Z= scan(FileName,skip = beginHeader+2,sep='\t',what=character(0), quiet=T)
    DataKey=matrix(Z,nrow=rows,ncol=cols,byrow=T)
    Key=as.vector(DataKey[,keyind])
    Data=DataKey[,-keyind]
  },warning=function(e){
    warning(e)
    warning('Please check row and column numbers, Trying to bypass error...')
    Z = read.table(FileName, comment.char = "%", header=FALSE,  fill=TRUE, stringsAsFactors = FALSE, na.strings=c('NA','NaN'),skip=beginHeader+3)
    Key=as.vector(Z[,keyind])
    Data=Z[,-keyind]
  },stop=function(f){
    warning(f)
    stop("Header or Comments or Data are not reasonably defined, see Subversion/PUB/ZFileFormatDocuments for further instructions")
  }
  )
  if(!is.null(Key))
    rownames(Data)=Key
  else
    warning('Key is missing, Check data.')
  
  tind=which(Header=='Time')
  if(length(tind)==0){
    warning("Column with name 'Time' not found. Assuming that the time is stored in the second column.")
    tind=1
  } 
  DF=tibble::as.tibble(Data[,-tind])
  DF=lapply(DF,type.convert, as.is=TRUE)
  DF=tibble::as_tibble(DF) 
  Time=as.Date(Data[,tind])
  
  #print(str(Time))
  colnames(DF)=Header[-tind]
  if(sum(!is.finite(Time))==length(Time)) 
    stop('Time has a format which as.Date does not recognize.')
  else
    if(sum(!is.finite(Time))) warning(paste(sum(!is.finite(Time)),"lines of the Time feature are missing values.")) 
  
  Time=tibble::as.tibble(Time)
  TibbleDF=dplyr::bind_cols(Time,DF)
  colnames(TibbleDF)=Header
  
  if(SilentComments){
    if(!is.null(Comments)){
      print('The follwing comments were in the file:')
      print(Comments)
    }
  }
  
  setwd(currentWD)
  return(TibbleDF)
}