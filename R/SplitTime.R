SplitTime=function(Time,Resolution='years',format=c('y-m-d','h:m:s'),split=" "){
  # SplitYears(Time,format=c('y-m-d','h:m:s'),split=" ")
  #Split times into years
  #Input
  #Time[1:n] character of time from years to seconds will be translated to chon object or chron object or chron object
  #format     format of Time as vector of years to days and hours to seconds
  #split      character vector (or object which can be coerced to such) containing regular expression(s) (unless fixed = TRUE) to use for splitting.
  #OUTPUT
  # list[1:k] splittet time
  #author: MT 2017  
  
  requireNamespace('chron')
  
  if(missing(Time)){
    stop('Time is missing')
  }
  if(is.list(Time)){
    warning('Time is a list, Trying to unlist.')
    Time=unlist(Time)
  }
  
  if(!inherits(Time,'chron')){
    if(inherits(Time, "POSIXlt")|inherits(Time, "POSIXct")){
      warning('Time is either POSIXlt or POSIXct object. Trying to transform to character')
      Time=as.character(Time)
    }
    if(!mode(Time)=="character"){
      warning('Time is neither character or chron object. Trying to transform to character')
      Time=as.character(Time)
    }
    indt1=which(Time=='NaN')
    if(length(indt1)>0)
      Time=Time[-indt1]
    indt2=which(Time=='NA')
    if(length(indt2)>0)
      Time=Time[-indt2]
    
  }
  dates=sapply(strsplit(Time, split), "[[", 1)
  daytimes=sapply(strsplit(Time, split), "[[", 2)
  y=chron::chron(dates=dates,times=daytimes,format=format)
  #print(chron::years(y))
  switch(Resolution,
         minutes={z=as.numeric(chron::minutes(y))
         names=sort(unique(z),na.last = T,decreasing = F)
         },
         seconds={z=as.numeric(chron::seconds(y))
         names=sort(unique(z),na.last = T,decreasing = F)
         },
         
         days={z=as.numeric(chron::days(y))
         names=sort(unique(z),na.last = T,decreasing = F)
         },
         hours={z=chron::hours(y)
         names=sort(unique(z),na.last = T,decreasing = F)
         },
         weekdays={ztemp=weekdays(y, abbreviate = TRUE)
         z=rep(NaN,length(ztemp))
         z[ztemp=='Mon']=1
         z[ztemp=='Tue']=2
         z[ztemp=='Wed']=3
         z[ztemp=='Thu']=4
         z[ztemp=='Fri']=5
         z[ztemp=='Sat']=6
         z[ztemp=='Sun']=7
         names=levels(weekdays(y, abbreviate = FALSE))
         names=names[c(2:7,1)]
         },
         months={z=as.numeric(months(y, abbreviate = TRUE))
         names=sort(unique(z),na.last = T,decreasing = F)
         },
         quarters={ztemp=quarters(y, abbreviate = FALSE)
         z=rep(NaN,length(ztemp))
         z[ztemp=='I']=1
         z[ztemp=='II']=2
         z[ztemp=='III']=3
         z[ztemp=='IV']=4
         names=levels(quarters(y, abbreviate = FALSE))
         },
         years={ztemp=chron::years(y)
         ind=as.numeric(levels(chron::years(y)))
         z=rep(NaN,length(ztemp))
         for(i in 1:length(ind)){
           z[ztemp==ind[i]]=i
         }
         names=ind  
         },
         stop("Wrong Resolution string entered")
         
  )
  
  vec=z
  ind=unique(vec)
  temp=list()
  for(i in min(ind):max(ind)){
    if(sum(vec==i)>0)
      temp=c(temp,list(Time[vec==i]))
  }
  return(temp)
}