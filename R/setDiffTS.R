# V=setDiffTS(Time1, Time2, Vector1 = NULL, Vector2 = NULL)
#
# Description:
# Symmetric setdiff between two time series (TS)
#
# INPUT
# Time1                   [1:n] POSIXct, or similar time object
# Time2                   [1:m] POSIXct, or similar time object
# Vector1                 Default: NULL, else [1:n] vector
# Vector2                 Default: NULL, else [1:n] vector
#
# OUTPUT
# TS1notinTS2             List with two elements: Time1 and Vector1 according to left side
# TS2notinTS1             List with two elements: Time2 and Vector2 according to right side
# 
# Details: Function works with all types of vectors, not only numerical
#
#
#author: MCT

setDiffTS=function(Time1,Time2,Vector1=NULL,Vector2=NULL){
  ind1=!Time1 %in% Time2
  ind2=!Time2 %in% Time1
  TS1notinTS2 =  list(Time1[ind1],Vector1[ind2])
  TS2notinTS1 =  list(Time2[ind2],Vector2[ind1])
  
  return(list(TS1notinTS2=TS1notinTS2,TS2notinTS1=TS2notinTS1))
}