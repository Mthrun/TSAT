# V = aggregateToUniqueTime(Time, DataVectorOrMatrix, Fun = sum, ...)
#
# Description:
# Aggregation to Unique Time by any function
#
# INPUT
# Time               [1:n] vector
# Data               [1:n,1:d] matrix or dataframe, d can be also 1, then vector
# FUN                aggregate by a function like sum
# ...                Further arguments passed on to FUN.
#
# OUTPUT 
# Aggregation	       [1:m,1:d] matrix, m<=n of DataVectorOrMatrix mapped to UniqueTime
# UniqueTime	       [1:m] vector, m<=n unique elements of Time
#
#
# Author: MCT

aggregateToUniqueTime=function(Time,DataVectorOrMatrix,Fun=sum,...){

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