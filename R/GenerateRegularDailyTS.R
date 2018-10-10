GenerateRegularDailyTS=function(TimeChar, Datavec, na.rm = TRUE, format = '%Y-%m-%d', tz = 'UTC',option = 'stine', Start,End,PlotIt = FALSE){
  
  requireNamespace('tibble')
  requireNamespace('imputeTS')
  requireNamespace('tidyr')
  requireNamespace('lubridate')
  if(!lubridate::is.Date(TimeChar))
    Time=as.Date(strptime(TimeChar,format = format,tz = tz))
  else
    Time=TimeChar
  
  if(length(Time)!=length(unique(Time))) warning('"TimeChar" is not unique meaning that several days have the same date.')
  
  orderedtime=order(Time,decreasing = FALSE,na.last = NA)
  
  if(length(Time)!=length(orderedtime)) warning('"TimeChar" is has NA dates, they are removed from "Datavec" and "TimeChar".')
  
  if(!identical(Time,Time[orderedtime])) warning('"TimeChar" was not ordered from past to future. "TimeChar" and "Datavec" reordered accordingly.')
  
  Time=Time[orderedtime]
  Datavec=Datavec[orderedtime]
  
  if(is.double(na.rm)) stop('"na.rm" parameter wrongly chosen')
  if(is.na(min(Time))) stop('Wrong "format" chosen, please change.')
  
  if(missing(Start))
    Start=min(Time)
  if(missing(End))
    End=max(Time)
  
  FullTime=seq(from=Start,to=End,by='days')
  
  DF=data.frame(Time=FullTime,Data=NA)
  
  ind=match(Time,DF$Time)
  if(sum(!is.finite(ind))>0) warning('Either Start or End is before/after Minimum/Maximum of "TimeChar"')
  
  ind2=ind[is.finite(ind)]
  DF$Data[ind2]=Datavec[is.finite(ind)]
  
  switch(na.rm, 
         true={
           TS1=imputeTS::na.interpolation(DF$Data,option=option)
           DF$Data=TS1
         },
         false={
           TS1=DF$Data
           TS1[!is.finite(TS1)]=NaN #tidyr accepts only NA but NaN are here the correct notation...
           DF$Data=TS1
         },
         zero={
           TS1=DF$Data
           TS1[!is.finite(TS1)]=0
           DF$Data=TS1
         },
         mean={
           TS1=DF$Data
           TS1[!is.finite(TS1)]=mean(Datavec,na.rm=TRUE)
           DF$Data=TS1
         },
         min={
           TS1=DF$Data
           TS1[!is.finite(TS1)]=min(Datavec,na.rm=TRUE)
           DF$Data=TS1
         },
         max={
           TS1=DF$Data
           TS1[!is.finite(TS1)]=max(Datavec,na.rm=TRUE)
           DF$Data=TS1
         },
         ff={
           DF=tidyr::fill(DF,'Data',.direction="down")
         },
         bf={
           DF=tidyr::fill(DF,'Data',.direction="up")
         },
         weighted_bf={
           DF$WertNotmiert=NaN
           nonmissingind=which(is.finite(DF$Data))
           for(i in 2:length(nonmissingind)){
      
              ind=seq(from=nonmissingind[i-1]+1,to=nonmissingind[i],by=1)
              n=length(ind)

              DF$WertNotmiert[ind]=DF$Data[nonmissingind[i]]/n
           }
           if(nonmissingind[1]==1)
            DF$WertNotmiert[1]=DF$Data[1]

           DF2=DF[,c('Time','Data')]
           DF2$Data=DF$WertNotmiert
           DF=DF2
         },
         weighted_ff={
           DF$WertNotmiert=NaN
           nonmissingind=which(is.finite(DF$Data))
           for(i in 1:(length(nonmissingind)-1)){
             ind=seq(from=nonmissingind[i],to=nonmissingind[i+1]-1,by=1)
             n=length(ind)
             DF$WertNotmiert[ind]=DF$Data[nonmissingind[i]]/n
           }
           if(tail(nonmissingind,1)==nrow(DF))
             DF$WertNotmiert[nrow(DF)]=DF$Data[nrow(DF)]
           
           DF2=DF[,c('Time','Data')]
           DF2$Data=DF$WertNotmiert
           DF=DF2
         },
         {
           stop('"na.rm" parameter wrongly chosen')
         }
  )
  
  if(PlotIt){
    #plot(na.spline(full),col='red')
    m <-graphics::layout(matrix(c(1, 1, 2,2)))
    plot(Time,Datavec,col='blue',main='Irregular Time Series',type='l')
    
    plot(DF$Time,DF$Data,col='blue',pch=1, main='Regular Time Series',type='l',xlab='Time',ylab='Datavec')
    
  }

  return(tibble::as.tibble(DF))
  }