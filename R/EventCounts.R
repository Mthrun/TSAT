EventCounts=function(Events,PlotIt=FALSE,main="Histogram of Events",xlab='Event Names'){
#EventCounts(Events,PlotIt=FALSE,main="Histogram of Events",xlab='Event names')
# INPUT
# Events [1:n] vector of events to be counted, character or numerical
# PlotIt TRUE (default): Plots histogram
# main #title of plot
# xlab  #xlabel of pöot
# Output
# Data[1:k,1:3] k breaks of (k different events) with counts as second column and event names as third column 
#author: MT 12/2017
z=Events
Counts=c()
if(is.numeric(z)){

  Breaks=1:max(unique(z),na.rm = T)
  Breaks2=Breaks
  for(i in 1:max(Breaks)){
    tempvr=sum(z==Breaks[i],na.rm = T)
     #if(tempvr>0)#{
      Counts=c(Counts,tempvr)
    # }else{
    #   Breaks2=Breaks2[-i]
    # }
    tempvr=c()
  } 
  # Breaks=Breaks2
  ind=which(Counts!=0)
  Counts=Counts[ind]
  names=as.character(Breaks[ind])
  Breaks=1:length(Breaks[ind])

  MiN=1#min(z, na.rm = TRUE)
  MaX=length(Breaks[ind])#max(z, na.rm = TRUE)

}else{
  MiN=1
  x=sort(unique(z))
  MaX=length(x)
  Breaks=1:length(x)
  
  for(i in 1:max(Breaks)) {
     tempvr=sum(z==x[i],na.rm = T)
    # if(tempvr>0){
       Counts=c(Counts,tempvr)
    # }else{
    #   x=x[-i]
    #   Breaks=Breaks[-i]
    # }

  }
  names=x
}

ind=order(Counts,decreasing = T,na.last = T)
Counts=Counts[ind]
names=names[ind]

if(PlotIt)
  PlotEventCounts(Counts,Names=names,main=main,xlab=xlab)

Data=cbind(Breaks,Counts,names)
colnames(Data)=c('Breaks','Counts','Names')
return(Data)
}