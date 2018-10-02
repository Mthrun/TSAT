checkFilename=function(FileName,Directory=getwd(),Extension,ReadOrWrite,NameOfFunctionCalled){
# checkFilename(FileName,Directory,Extension,ReadOrWrite,NameOfFunctionCalled)
# Searching for Filename & Directory existence
#
# INPUT
# Filename  		  	string, name for file without extension
# Extension 			string, extension of filename (everything after the ".")
# ReadOrWrite			=TRUE: ReadX
#									=FALSE: WriteX
# NameOfFunctionCalled: Fuer Fehlermeldung: Welche Funktion hat checkFilename aufgerufen
#
# Optional
# Directory    			string, complete path of file, default is current path
#
# OUTPUT
# Bricht mit Fehlermeldung die Ein/AusleseProzedure ab, wenn mit Dateiname/Directory etwas nicht stimmt

# author: MT 06/2015
  FileName;
  if(is.null(FileName))
    stop('Filename is null/does not exist, please use a string as a filename')
  
    if(!is.character(FileName)){
      warning(paste('Filename is not of type character'))
    }
Filename=addext(FileName,Extension)
	if (!file.exists(Directory)){
		stop(paste0(NameOfFunctionCalled,' Directory: ',Directory ,' ,does not exist'))
	}
 
 if(ReadOrWrite){ #ReadX function, check for filename
 
  if(!file.exists(file.path(Directory,Filename))){
		stop(paste0(NameOfFunctionCalled,' FileName ',Filename,' does not exist'))
	}
 }else{ #In Case of WriteX:
   if(file.exists(file.path(Directory,Filename))){
     warning(paste0(NameOfFunctionCalled,' FileName ',Filename,' exists and will be overwritten'))
   }
 }	
}
