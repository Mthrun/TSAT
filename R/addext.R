#' add file extensions to file names
#' 
#' \code{addext} intern \code{DataIO} function, checks for the right extension
#' and adds it if necessary.
#' 
#' Files have special formats and therefor need to be saved with the correct
#' extensions. \code{addext} is simply an auxiliary function to guarantee
#' correct file name extensions
#' 
#' @param filename string, to be checked for correct extension
#' @param extension Correct extension for filename.
#' @return Function returns a string containing the filename and the correct
#' extension.
#' @author Michael Thrun
#' @keywords file
#' @examples
#' 
#' addext('hallo.data','data') #  'hallo.data'
#' addext('hallo','data')     #  'hallo.data'
#' 
#' 
`addext` <-
function(filename,extension){
extlen = nchar(extension)
filelen = nchar(filename)
if (filelen <= extlen || substr(filename, filelen-extlen,filelen) != paste('.',extension,sep=''))
                 filename = paste(filename,'.',extension,sep="")
return(filename)
}

