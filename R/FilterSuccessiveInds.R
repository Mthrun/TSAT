FilterSuccessiveInds= function(x) { 
# V=FilterSuccessiveInds(x)
# Filters consecutive Indizes
# Input
# x[1:n]            vector of Inds
# Output List V of 
#  V$length[1:m]      length of consecutive inds
#  V$values[1:m]      start ind of consecutive inds
# author: MT 11/17
#Zahlenproblem c(1,2,3,4,9,14,15,16) filtern zu c(1,9,14) und c(4,1,3) #Laenge  
#Hintergrund: Teilweise wird Zeitevent mehrmals aufgenommen
  
  incr=1
  if(!is.numeric(x)) x <- as.numeric(x) 
  x=sort(x,decreasing = F,na.last = T) #funktioniert nur wenn zahlen sortiert sind!
  n <- length(x)  
  y <- x[-1L] != x[-n] + incr 
  i <- c(which(y|is.na(y)),n) 
  list(lengths = diff(c(0L,i)),
       values = x[head(c(0L,i)+1L,-1L)]) 
}