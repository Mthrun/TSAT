setDiffTS=function(Time1,Time2,Vector1=NULL,Vector2=NULL){
  ind1=!Time1 %in% Time2
  ind2=!Time2 %in% Time1
  TS1notinTS2 =  list(Time1[ind1],Vector1[ind])
  TS2notinTS1 =  list(Time2[ind2],Vector2[ind])
  
  return(list(TS1notinTS2=TS1notinTS2,TS2notinTS1=TS2notinTS1))
}