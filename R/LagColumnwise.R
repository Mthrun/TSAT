LagColumnwise=function(Data,LagRange=1:10){
  #Ext=LagColumnwise(Data,LagRange=1:10)
  #Generates Lagged features by not using information of futures in specific range
  #INPUT
  #Data         matrix of datsa
  #LagRange     numeric vecgtor of which lagges to use
  #OUTPUT
  #DataLagged   numeric marix of data with lagged features without original data
  #author MCT, 03/2024
  
  # vecR=c("t-3","t-2","t-1","t")
  # LagVector(vecR,1)#
  if(is.null(colnames(Data))){
  colnames(Data)=paste0("C",1:ncol(Data))
  }
  DataList=c()
  k=1
  for(lag in LagRange){
    DataL=apply(Data,2,LagVector,lag)
    colnames(DataL)=paste0("L",lag,"_",colnames(Data))
    DataList[[k]]=DataL
    k=k+1
  }
  DataLagged=do.call(cbind,DataList)
  return(DataLagged)
}