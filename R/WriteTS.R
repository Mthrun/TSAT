# WriteTS(FileName, Time, SeriesVal)
#
# Description:
# Save series consisting of Time and SeriesVal and eventual further texts to a *.TS file.
#
# INPUT
# FileName                   String, name of the  file to be written
# Time                       [1:n] vector of row type, typically UnixTime or other time in seconds: unique key for each line, by default: [1:n]
# SeriesVal                  [1:n] vector with text without blanks or numerical data to be put in each line
# OPTIONAL
# FurtherTexts              [1:n,1:c] string matrix with row FurtherTexts to be put in third column
# Header                    1:(c+1) vector for the headers including for the names and FurtherTexts: 
#                           Array of chars, this line will be inserted at top of file with leading #
# OutDirectory              The OutDirectory where to write into; if not given: current dir
# ForceNumeric              TRUE: saves numeric format of SeriesVal, FALSE: accepts text
# Comments                  vector, Char Array or matrix, these lines will be inserted at top of file with leading #
#                           Note: Also allowed: Comments='first line \n# second line'
# 
#author: MCT 08/2023


WriteTS=function(FileName,Time,SeriesVal, FurtherTexts,Header, OutDirectory=getwd(), ForceNumeric=FALSE,
                         Comments){

  if(is.null(FileName)){
    stop('WriteTS: FileName ist missing')
  }
  checkFilename(FileName, Directory = OutDirectory,        # Check FileName if correct extension, its existing, ...
                Extension = 'ts', ReadOrWrite = FALSE,
                NameOfFunctionCalled='WriteTS()')
  
  if(isTRUE(ForceNumeric)){
    #if german notation
    SeriesVal=gsub("\\,","\\.",SeriesVal)
    SeriesVal=gsub("[^0-9.]","",SeriesVal)
    mode(SeriesVal)="numeric"
  }else{
    #SeriesVal=as.character(SeriesVal)
  }
  
  if(!is.null(SeriesVal)){      # Pruefung welche Inputvariablen angegeben wurden
    rows=length(SeriesVal)
    Data = data.frame(SeriesVal)   
  }else{
    stop('WriteTS: SeriesVal ist missing')
  }

  #----------------------------------------------------------------------------#
  ## Abfang Fehler bezueglich Time
  #----------------------------------------------------------------------------#
  if(missing(Time)|is.null(Time)){#MT: Korrektur zur Timeerstellung
    Time = 1:rows
    warning('WriteTS: Time missing, generating Time as 1:n.')
  }else{#Time set by user
    if(length(Time) != rows){
      stop('WriteTS():Length of Time isnt equal to the length of rows')
    }
    #convert to numeric so that format non scientific works
    Time=as.numeric(Time)
    if(sum(!is.finite(Time))>0){
      warning('WriteTS(): Time has element that are not finite.')
      Time[!is.finite(Time)]=NaN
    }
    #force no scientific format
    Time=format(Time,scientific=FALSE)
  }
  
  if(missing(FurtherTexts)) FurtherTexts=NULL
  
  if(missing(Header)){
    Header=NULL #wird weiter unten generiert
  }else{
    # remove all Blanks from header
    Header = gsub('  ','_',Header)
    Header = sub(' ', '_', Header)
    Header = gsub('\n','_',Header)
    Header = gsub('\t','_',Header)
  }       
  
  if(!is.null(FurtherTexts)){ 
    if(!is.character(FurtherTexts)) stop("FurtherTexts must consist of Strings!")
    #MT: Einfache Anfuerungsstriche sind ein sonderzeichen, welches eine korrekte
    #    checksummen erstellen und ueberpruefung verhindert
    FurtherTexts = sub("'","",sub("'","",FurtherTexts))
    Data = cbind(Data,FurtherTexts)
    if(is.null(Header)){
      if(!is.null(colnames(FurtherTexts))){
        Header=c("TimeInSeconds","Series",colnames(FurtherTexts))
      }else{#=NULL wird weiter unten abgefangen
        if(!is.null(ncol))
          Header=Header=c("TimeInSeconds","Series",paste0("C",1:ncol(FurtherTexts)))
      }
    }
  }else{
    if(is.null(Header)) Header=c("TimeInSeconds","Series")
  }
  if((ncol(Data)+1)<length(Header)){
    Header=Header[1:ncol(Data)]
    warning("WriteTS(): Header is longer than SeriesVal plus furthertext (if exist) columns. Taking first elements.")
  }
  if((ncol(Data)+1)>length(Header)){
    diff=ncol(Data)-length(Header)
    Header=c(Header,paste0("C",1:diff))
    warning("WriteTS(): Header is shorter than SeriesVal plus furthertext (if exist) columns. Adding elements.")
  }
  
  if(ncol(Data) == 0 || nrow(Data) == 0 || typeof(Data) == 'NULL'){    # Zeilen anzahl bestimmen
    stop('WriteTS(): No Data to write') #stop wenn Data NULL ist
  }else{
    rows    = nrow(Data)
    columns = ncol(Data)
  }
  
  FileName  = addext(FileName,'ts')    # richtige Datei endung
  suppressWarnings((OutDirectory = normalizePath(OutDirectory))) 
  currentWD = getwd()                     # Aktuelles verzeichnis merken
  setwd(OutDirectory)                     # Ins Verzeichnis wo sich Datei befindet wechseln
  
  if(!is.null(Time)){
    Timetmp = unique(Time)
    if(length(Timetmp)!=length(Time)){
      warning(paste0('WriteTS(): Time with length ',length(Time),' is not unique: ',length(Timetmp)))
    }else{
      Time=Timetmp
    }
  } #make Time unique
  
  Data = data.frame(Time,Data)    #Time hinzufuegen zu ColumnNames 
  columns = columns + 1 #Wenn Time Hinzugefuegt wurde, muss auch die Spalten Anzahl angepasst werden
  
  #ColumnNames zu Data
  #----------------------------------------------------------------------------#
  # letzter Schritt ist die Klassen mit class() anzupassen:
  # Problem war: das entweder nur string oder nur Zahl gefordert war
  # R aber keine explizite Typen definition vorraus setzt. 
  #----------------------------------------------------------------------------#
  for(i in 1:columns){
    if(is.integer(Data[,i])){
      Data[,i]= as.numeric(Data[,i])
    }
    if(is.factor(Data[,i])){
      Data[,i]=as.character(Data[,i])
    }
  }
  #----------------------------------------------------------------------------#
  ##---------------------Beginn mit schreiben in Datei--------------------------
  #----------------------------------------------------------------------------#
                                                                      # MCT way of WriteTS
  if(!missing(Comments)){                                                        # Write comments if existing (1st row)
    Comments = paste0('# ',Comments)                                             # One row starting with '#'
    write.table(x = Comments, file = FileName, quote = FALSE, sep='\t',
                row.names = FALSE, col.names = FALSE, na = 'NaN',append=FALSE)
    furtherappend=TRUE
  }else{
    furtherappend=FALSE
  }
  
  cols=ncol(Data)
  # write dimensions Number of lines & columns
  header = c(paste('%\t',rows),paste('%\t',cols))
  write.table(header, FileName, quote=FALSE, row.names=FALSE, col.names=FALSE, na='NaN',append = furtherappend)

  
  cat('% ', file=FileName, append=TRUE)
  cat(Header,'\n', file=FileName, append=TRUE, sep='\t')
  
  # Daten schreiben
  write.table(Data, file = FileName, append = TRUE, quote = FALSE, sep='\t',
              row.names = FALSE, col.names = FALSE, na = 'NaN')
  #}
  
  ##------------Daten schreiben abgeschlossen-----------  
  #zurueck wechseln ins alte Verzeichnis
  setwd(currentWD)
  #  }
}