EventCountsPerTime=function(BeginningTime,Resolution='months',PlotIt=TRUE,format=c('y-m-d','h:m:s'),split=" "){
# EventCountsPerTime(BeginningTime,Resolution='days',PlotIt=T,format=c('y-m-d','h:m:s'),split=" ")
#Counts the Events defined by the beginning (middle or end) time of the event in a resolution window of time
#Input
#BeginningTime[1:n] character of time from years to seconds will be translated to chon object or chron object or chron object
#Resolution string Option: seconds, minutes, days,hours,weekdays,months,quarters or years
#PlotIt     default(True) plots histogram
#format     format of Time as vector of years to days and hours to seconds
#split      character vector (or object which can be coerced to such) containing regular expression(s) (unless fixed = TRUE) to use for splitting.
#OUTPUT
# Data[1:n,2] Time of Resultation ind columns 1 and Counts in Columns 2
#author: MT 2017
  requireNamespace('chron')
  if(!is.character(Resolution)){
    stop('Resolution has to be character.')
  }
  if(missing(BeginningTime)){
    stop('BeginningTime is missing')
  }
  if(is.list(BeginningTime)){
    warning('BeginningTime is a list, Trying to unlist.')
    BeginningTime=unlist(BeginningTime)
  }
  
  if(!inherits(BeginningTime,'chron')){
    if(inherits(BeginningTime, "POSIXlt")|inherits(BeginningTime, "POSIXct")){
      warning('BeginningTime is either POSIXlt or POSIXct object. Trying to transform to character')
      BeginningTime=as.character(BeginningTime)
    }
    if(!mode(BeginningTime)=="character"){
      warning('BeginningTime is neither character or chron object. Trying to transform to character')
      BeginningTime=as.character(BeginningTime)
    }
    indt1=which(BeginningTime=='NaN')
    if(length(indt1)>0)
      BeginningTime=BeginningTime[-indt1]
    indt2=which(BeginningTime=='NA')
    if(length(indt2)>0)
      BeginningTime=BeginningTime[-indt2]
    
    dates=sapply(strsplit(BeginningTime, split), "[[", 1)
    daytimes=sapply(strsplit(BeginningTime, split), "[[", 2)
    y=chron::chron(dates=dates,times=daytimes,format=format)
  }else{
    y=BeginningTime
  }
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
  Data=EventCounts(z,PlotIt=PlotIt,main="Histogram of Time Events",xlab=Resolution)
  #Breaks <- sort(unique(z),na.last = T,decreasing = F)
  # Breaks=1:max(unique(z),na.rm = T)
  # Counts=c()
  # print(Breaks)
  # for(i in 1:max(Breaks)) Counts=c(Counts,sum(z==Breaks[i],na.rm = T))
  # Breaks=1:max(Breaks)
  # names=as.character(Breaks)
  # print(Counts)
  # nB <- length(Breaks)
  # y <- Counts
  # Title = "Histogram of Time of Data"
  # xlab=Resolution
  # ylab='Frequencies'
  # if (PlotIt){
  #   plot(x = c(min(z, na.rm = TRUE)-0.5, max(z, na.rm = TRUE)+0.5), 
  #        y = c(0, max(Counts, na.rm = TRUE) * 1.2), type = "n", 
  #        main = Title, xaxs = "i", yaxs = "i", axes = FALSE, 
  #        xlab = xlab, ylab = ylab, xlim = c(min(z, na.rm = TRUE)-0.5, 
  #                                           max(z, na.rm = TRUE)+0.5), ylim = c(0, max(Counts, 
  #                                                                                      na.rm = TRUE) * 1.2))
  #   par(mgp = c(2.2, 0.6, 0))
  #   rect(Breaks-0.5, 0, Breaks+0.5, y, col = "blue", border = "light blue", 
  #        xlab = "", ylab = ylab, xlim = c(min(z, na.rm = TRUE)-0.5, 
  #                                         max(z, na.rm = TRUE)+0.5), ylim = c(0, max(Counts, 
  #                                                                                    na.rm = TRUE) * 1.2))
  #   axis(1, col = "black", las = 1, xaxs = "i",at=Breaks,labels = names)
  #   axis(2, col = "black", las = 1, yaxs = "i")
  # }
  # Data=cbind(Breaks,y)
  # colnames(Data)=c(Resolution,'Counts')
  return(Data)
  ###
}