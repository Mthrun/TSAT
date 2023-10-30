# V=ReadTS(FileName,InDirectory)
# Series    = V$Series
# Rest     = V$FurtherTexts
# Comment  = V$Comments
#
# Description:
# Loads a TS file
#   
# INPUT
# FileName                        Filename of *.TS file
# OPTIONAL
# InDirectory                     InDirectory where *.TS file is, default: Current dir
# ForceNumeric                    TRUE: saves numeric format of series, FALSE: Accepts text
#
# OUTPUT
# Series                          [1:d,1:2] matrix of Time and SeriesVal, 1st column is Unix time or time in seconds, 
#                                 unique index from first column, Series contained in Column 2, without blanks
# Header                          [1:2+c] vector of column names for Time and SeriesVal plus c column names of further text 
#                                 rows that can be in the *.TS file. If no further texts are present c is 0.
# FurtherTexts                    [1:d,1:c] vector or matrix, containing the further text rows
#                                 Only returned if FurtherTexts are in the *.TS file
# Comments                        [1:k] vector, strings of all lines of Comments,  without the leading "#"

#author: MCT 08/2023

ReadTS = function(FileName,InDirectory=getwd(),ForceNumeric=FALSE){

  if(grepl('.lrn',FileName)){
    stop('Please use ReadLRN()')
  }
  
  ################################################################################################################
  ## Inputflow Kontrolle
  ################################################################################################################
  FileName  = addext(FileName,'ts')
  checkFilename(FileName,Directory=InDirectory,Extension='ts',ReadOrWrite=TRUE,NameOfFunctionCalled='ReadTS()')

  currentWD=getwd() #Aktuelles verzeichnis merken
  setwd(InDirectory) #Ins Verzeichnis wo sich Datei befindet wechseln

  ################################################################################################################
    #Beginn Daten einzulesen
    #Header einlesen mit HEADERSIZE
  ################################################################################################################
    
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
      # Wieso beginHeader+2 und nicht beginHeader+1?
      # ORA/inst/extdata/.*.NAMES Beispiele zeigen, dass man +1 braucht
      # da bei +2 die erste Datenzeile bereits mit ausgelesen wird
      #HeaderLines=readLines(FileName,n=beginHeader+1) # Edit funktioniert an anderer Stelle dann wieder nicht ...
      HeaderLines=readLines(FileName,n=beginHeader+2) # Original
      
      leng_size=sum(lengths(regmatches(HeaderLines, gregexpr("%", HeaderLines))))
      ZahlPattern = "[0-9]+"
      
      if(leng_size==2){ # Edit July 2023, ORA NAMES examples didnt retrieved correct ncols
        atomicVectorInd = regexpr(ZahlPattern, HeaderLines[beginHeader])
        #atomicVecIdxCol = regexpr(ZahlPattern, HeaderLines[beginHeader+1])
        tmpVar1 = unlist(strsplit(HeaderLines[beginHeader+1], "\t"))
        tmpVar2 = gsub(" ", "", gsub(pattern = "[[:punct:]]", "", tmpVar1))
        StartRow = atomicVectorInd[1]
        StartCol = atomicVectorInd[2]
        EndRow = StartRow+attributes(atomicVectorInd)$match.length[1]-1
        EndCol = StartCol+attributes(atomicVectorInd)$match.length[2]-1
        rows=as.numeric(substr(HeaderLines[beginHeader],StartRow,EndRow))
        cols=as.numeric(substr(HeaderLines[beginHeader+1],StartCol,EndCol))
        #cols=length(tmpVar2)
      }else{
        atomicVectorInd=regexpr(ZahlPattern, HeaderLines[beginHeader])
        StartRow = atomicVectorInd
        EndRow = StartRow+attributes(atomicVectorInd)$match.length-1 #
        rows=as.numeric(substr(HeaderLines[beginHeader],StartRow,EndRow)) # wie viele datenzeilen gibt 
        cols=NA
      }
      #Read Header
      Line=HeaderLines[beginHeader+2]#
        if(gregexpr('%',Line)[1]==1){#Prozentzeichen muss als erstes Zeichen einer Zeile kommen
          
        Line=sub('%','',Line)
        
        HeaderPatternStarts = "([[:alnum:]]|[[:punct:]])+"
        HeaderInd=gregexpr(HeaderPatternStarts,Line )
        Laengen = attributes(HeaderInd[[1]])$match.length
        Starts =   unlist(HeaderInd)
        Last = Starts+Laengen-1
        Header=substring(Line,Starts,Last)
        cols=length(Header)
        }else{
        #kein header da, legacy code
          Header=NULL #untern abgefangen
      }
   },
  error = function(c){
    warning(c)
    stop("Header or Comments are not reasonably defined, see Subversion/PUB/ZFileFormatDocuments for further instructions")
  }
  )

  
  
  ################################################################################################################
  ## Auslesen des Datensatzes
  ################################################################################################################
  #gibt fehler meldung aus wenn columns kleiner als 5 Datensätze sind.
  #Grund ist eine C-Funktion die die ersten 5 Zeilen einliest um die Daten zubestimmen
  #mehr infos: http://stackoverflow.com/questions/5990654/incomplete-final-line-warning-when-trying-to-read-a-csv-file-into-r
  tryCatch({
    Data = read.table(FileName,
                      sep='\t',
                      quote = "", #To disable quoting altogether
                      comment.char = "%",   #comments are already read in, catch legacy names that has only one % and new names with two %
                      header=FALSE,         #Wenn die erste Zeile der Header ist. zB. bei csv
                      fill=T, 
                      flush = T,
                      stringsAsFactors = FALSE, 
                      na.strings=c(''),
                      skip=beginHeader)#,    #Die ersten HEADERSIZE zeilen werden nicht nochmal gelesen
    ncols=ncol(Data)
  if(!is.null(rows))
    if(rows!=nrow(Data))
      warning(paste0('ReadNAMES(): Number of rows ',nrow(Data),' does not equal number of rows in header ',rows))
  
  },
  error = function(c){
    print("Time, Series or Descriuption are not reasonably defined, see Subversion/PUB/ZFileFormatDocuments for further instructions")
  }
  )
  
  ################################################################################################################
  ## Outputflow Kontrolle
  ################################################################################################################
  if(is.finite(cols)){
    if(ncol(Data)!=cols){
      warning(paste('rows of Time+Series+FurtherDescription(if exists)',ncol(Data),'does not equal number of columns stated in the header',cols))
    }
  }#otherwise old format, do nothin
  KeyColumn=1
  Time = Data[,KeyColumn]  #Umspeichern in Time 
  
  Data=Data[,-(KeyColumn)]  #Time als Spalte Loeschen
  Data=data.frame(Data) #Dataframe mit einer spalte == liste -> muss wieder data.Frame sein
  
  #zurück wechseln ins alte Verzeichnis
  setwd(currentWD)
  
  
  # Series & FurtherTexts
    NamesColumn = 2-KeyColumn
    Series=Data[,NamesColumn]
    if(isTRUE(ForceNumeric)){
      Series=gsub("\\,","\\.",Series)
      Series=gsub("[^0-9.]","",Series)
      mode(Series)="numeric"
    }else{
      #Series=as.character(Series)
    }
 
  if(sum(!is.finite(suppressWarnings(as.numeric(Time))))==0)
  	mode(Time)='numeric'
  
  if(ncols>2){
    FurtherTexts = Data[,-NamesColumn]
    FurtherTexts = as.matrix(FurtherTexts)# Edit July 2023: FurtherTexts kann Vektor sein
    if(is.null(Header)){
      Header=c("Series",paste0("C",1:ncol(FurtherTexts)))
    }else{
      Header=Header
    }
    HeaderFurtherText=Header[-1]#np neames
    
    if(length(HeaderFurtherText)==ncol(FurtherTexts))
      colnames(FurtherTexts)=HeaderFurtherText#tail(Header[-1],ncol(FurtherText))#no key -> "-1

    Series=cbind(Time,Series=Series)
    colnames(Series)=Header[1:2]
    colnames(FurtherTexts)=Header[3:length(Header)]
    result=list(Series=Series,FurtherTexts=FurtherTexts,Header=Header,Comments=Comments)
  }else{
    if(is.null(Header)){
      Header=c("Time","Series")
    }else{
    }
    Series=cbind(Time,Series=Series)
    colnames(Series)=Header[1:2]
    result=list(Series=Series,Comments=Comments,Header=Header)
  }

  return (result)
}
