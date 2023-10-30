# addedCounts = AddCounts(Count1,Names1,Count2,Names2)
#
# Description:
# Adds two counts by searching for same numerical key or character vector
#
# INPUT
# Count1             [1:n] numerical vector           
# Names1             [1:n] key or charactical vector         
# Count2             [1:m] numerical vector            
# Names2             [1:m] key or charactical vector 
#
# OUTPUT 
# CountsCombined      matrix[1:k,1:2] where k is the amount of unique column names of both vectors and the values
#                     the combined count values of both values.
#
# Author: MCT

AddCounts=function(Count1,Names1,Count2,Names2){
  NamesCombined=unique(c(Names1,Names2))
  n=length(NamesCombined)
  CountsCombined=rep(NaN,n)
  for(i in 1:n){
    ind=which(Names1==NamesCombined[i])
    ind2=which(Names2==NamesCombined[i])
    if(length(ind)>0)
      c1=Count1[ind]
    else
      c1=0
    if(length(ind2)>0)
      c2=Count2[ind2]
    else
      c2=0
    CountsCombined[i]=c1+c2
  }
  
  return(cbind(Names=NamesCombined,Counts=CountsCombined))
}