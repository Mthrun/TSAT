AggregateToUniqueTime=function(Time,DataVectorOrMatrix,Fun=sum,...){

  ind=order(Time,decreasing = F,na.last = T)
  Time=Time[ind]
  if(is.matrix(DataVectorOrMatrix)){
    DataVectorOrMatrix=DataVectorOrMatrix[ind,]
  }else{
    DataVectorOrMatrix=as.matrix(DataVectorOrMatrix[ind])
  }
Cls=Time2Classification(Time)

uniqueClasses <- sort(na.last = T, unique(Cls))
numberOfClasses <- length(uniqueClasses)
resultPerClass <- matrix(0, numberOfClasses, ncol(DataVectorOrMatrix))

for (i in 1:numberOfClasses) {
  inClassInd <- which(Cls == uniqueClasses[i])
  x = DataVectorOrMatrix[inClassInd, , drop = FALSE]
  if (is.vector(x)) {
    x = as.matrix(x)
  }
  resultPerClass[i, ] <- apply(X = x, FUN = Fun, MARGIN = 2, 
                               ...)
}

return(list(Aggregation=resultPerClass,UniqueTime=unique(Time,fromLast = F)))
}