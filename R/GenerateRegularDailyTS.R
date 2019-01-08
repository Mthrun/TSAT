GenerateRegularDailyTS=function(TimeChar, Datavec, na.rm = TRUE, format = '%Y-%m-%d', tz = 'UTC',option = 'stine',Header=c('Time','Data'), Start,End,AggregateFun=sum,PlotIt = FALSE){
#GenerateRegularDailyTS(TimeChar, Datavec, na.rm = TRUE, format = '%Y-%m-%d', tz = 'UTC',option = 'stine',Header=c('Time','Data'), Start,End,AggregateFun=sum,PlotIt = FALSE)
  requireNamespace('tibble')
  requireNamespace('imputeTS')
  requireNamespace('tidyr')
  requireNamespace('lubridate')
  
  if(is.vector(Datavec)){

  if(!lubridate::is.Date(TimeChar))
    Time=as.Date(strptime(TimeChar,format = format,tz = tz))
  else
    Time=TimeChar
  
  orderedtime=order(Time,decreasing = FALSE,na.last = NA)
  
  if(length(Time)!=length(orderedtime)) warning('"TimeChar" is has NA dates, they are removed from "Datavec" and "TimeChar".')
  
  if(!identical(Time,Time[orderedtime])) warning('"TimeChar" was not ordered from past to future. "TimeChar" and "Datavec" reordered accordingly.')
  
  if(length(Time)!=length(Datavec)) stop('Length of "TimeChar" and "Datavec" has to be equal.')
  
  Time=Time[orderedtime]
  Datavec=Datavec[orderedtime]
  
  if(length(Time)!=length(unique(Time))){
    warning('"TimeChar" is not unique meaning that several days have the same date. Trying to use aggregate to solve this problem.')
    DF=aggregate(Datavec~Time,FUN = AggregateFun,na.rm=TRUE)
    colnames(DF)=c('Time','Datavec')
    Time=DF$Time
    Datavec=DF$Datavec
  } 
  
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
           
           indStart=seq(from=1,to=nonmissingind[1],by=1) #take start to first nonmissing
           nstart=length(indStart)
           DF$WertNotmiert[indStart]=DF$Data[nonmissingind[1]]/nstart
           
           for(i in 2:length(nonmissingind)){
              ind=seq(from=nonmissingind[i-1]+1,to=nonmissingind[i],by=1)
              n=length(ind)
              DF$WertNotmiert[ind]=DF$Data[nonmissingind[i]]/n
           }
           # if(nonmissingind[1]==1&Start==Time[1]) #Anfaenge ueberlappen
           #    DF$WertNotmiert[1]=DF$Data[1]
          
           # if(Start<Time[1]){#Regulaere ZR beginnt frueher 
           #    ind=seq(from=1,to=nonmissingind[1],by=1)
           #    n=length(ind)
           #    DF$WertNotmiert[ind]=DF$Data[nonmissingind[i]]/n
           # } 
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
           indEnde=seq(from=tail(nonmissingind,1),to=nrow(DF),by=1) #take last non missing to end
           nEnde=length(indEnde)
           DF$WertNotmiert[indEnde]=DF$Data[tail(nonmissingind,1)]/nEnde
           # if(tail(nonmissingind,1)==nrow(DF)) #Ende der regulaeren ZR ist ende der irregulaeren und mit wert
           #   DF$WertNotmiert[nrow(DF)]=DF$Data[nrow(DF)]
           # 
           # if(tail(nonmissingind,1)<nrow(DF)){ #Regulaere ZR ist groesser
           #   ind=seq(from=nonmissingind[i],to=nrow(DF),by=1)
           #   n=length(ind)
           #   DF$WertNotmiert[ind]=DF$Data[nonmissingind[i]]/n
           # } 
           DF2=DF[,c('Time','Data')]
           DF2$Data=DF$WertNotmiert
           DF=DF2
         },
         {
           stop('"na.rm" parameter wrongly chosen')
         }
  )
  colnames(DF)=Header
  if(PlotIt){
    #plot(na.spline(full),col='red')
    m <-graphics::layout(matrix(c(1, 1, 2,2)))
    plot(Time,Datavec,col='blue',main='Irregular Time Series',type='l')
    
    plot(DF[,1],DF[,2],col='blue',pch=1, main='Regular Time Series',type='l',xlab=Header[1],ylab=Header[2])
    
  }

  return(tibble::as.tibble(DF))
  }else{#datavec is matrix/df/tibble
    Data=Datavec
    d=ncol(Data)
    names=colnames(Data)
    for(i in 1:d){
      if(i==1){
      tibbledf=as.matrix(GenerateRegularDailyTS(TimeChar, Datavec=Data[,i], na.rm,
                                      format,tz,option,Header=c('Time',names[1]),
                                      Start,End,AggregateFun,PlotIt = FALSE))
      }else{
        tibbledftmp=as.matrix(GenerateRegularDailyTS(TimeChar, Datavec=Data[,i], na.rm,
                                        format,tz,option,Header,
                                        Start,End,AggregateFun,PlotIt = FALSE))
        tibbledf=cbind(tibbledf,tibbledftmp[,2])
        
        # y=as.vector(as.matrix(tibbledftmp)[,2])
        # tibbledf=tibble::add_column(tibbledf,y)
        colnames(tibbledf)[i+1]=names[i]
        # print(y)
        # print(tibbledf)
      }
      
    }
    #colnames(tibbledf)=names
    return(tibble::as.tibble(tibbledf))
  }
  }