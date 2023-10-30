# df = aggregateWeeks2FiskalMonths(Time,Data,FUN,Header, ...)
#
# Description:
# Aggregation of many Days over several years to fiscal months is possible.
# A fiscal month is defined by the following. Every first month in a quarter has five weeks the following two months have
# exactly four weeks. The first month in the year starts with a monday (and every following month).
# A month has only full weeks included in it.
#
# INPUT
# Time               [1:n] vector of \code{as.Date} object. Beware, weekly data beginning with always monday is expected!
# Data               [1:n,1:d] matrix or dataframe, d can be also 1, then vector 
# FUN                aggregate by a function like sum or mean
# Header             colnames for data
# ...                Further arguments passed on to FUN.
#
# OUTPUT 
# dataframe[1:m,1:(d+1)] with m<n and first column being the time in as.Date format
#
# Details: The rownames of the data frame depict either the first week (usually) of the month the data is aggregated by or, in the case of the first time interval until a full month of data exists, the last week where data was existed (see example).
# The fiskal month is often used to compare weekly and monthly forecasts consistently.
#
# Author: MCT

aggregateWeeks2FiskalMonths=function(Time,Data,FUN,Header,...){
  requireNamespace('lubridate')
  if (!lubridate::is.Date(Time)) {
    warning("'Time' is not a date. Calling as.Date()")
    Time = as.Date(Time)
  }
  
  if (is.vector(Data)) {
    Boolean = TRUE
  } else if (!is.null(ncol(Data))) {
    if (ncol(Data) == 1) {
      Boolean = TRUE
    } else{
      Boolean = FALSE
    }
  } else{
    Boolean = FALSE
  }
  if (isTRUE(Boolean)) {
    requireNamespace('zoo')
    Weekly=data.frame(Time=Time,Data=Data)
    # Weekly = aggregateDays2Weeks(
    #   Time = Time,
    #   Data = Data,
    #   FUN = FUN,
    #   Header = c('Time', 'Data'),
    #   ...
    # )
    outage.zoo <-
      zoo::as.zoo(x = Weekly$Data, order.by = as.Date(Weekly$Time),frequency=7)
    
    fprior = zoo::zoo(, seq(
      from = as.Date(start(outage.zoo)),
      to = as.Date(end(outage.zoo)),
      by = "7 days"
    ))
    tempful = merge(outage.zoo, fprior, all = T)
    WeeklyNa = data.frame(Time = as.Date(zoo::index(tempful)), Data = tempful)
    WeeklyNa$Data[is.na(WeeklyNa$Data)] = 0
    Weekly = WeeklyNa
    
    yearlycls=c(1,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,4,5,5,5,5,6,6,6,6,7,7,7,7,7,8,8,8,8,9,9,9,9,10,10,10,10,10,11,11,11,11,12,12,12,12)
    years = lubridate::year(Weekly$Time)
    uy = unique(years)
    out = list()
    for (j in 1:length(uy)) {
      ind = which(years == uy[j])
      TimeCur = Weekly$Time[ind]
      DataCur = Weekly$Data[ind]
      yearcur = years[ind]
      weekno = lubridate::week(TimeCur) #number of complete seven day periods that have occurred between the date and January 1st
      indweekno = which(weekno > 0 & weekno < 53)
      if (length(indweekno) >= 1) {
        ycur = yearlycls[weekno[weekno > 0 &
                                  weekno < 53]] #here wie put in the real number below week 53 because we defined the cls
        TimeCur = TimeCur[indweekno] #here we require the indize because we did not define the input
        DataCur = DataCur[indweekno] #here we require the indize because we did not define the input
        yearcur = yearcur[indweekno] #here we require the indize because we did not define the input
        DFcur = data.frame(Data = DataCur, Cls = ycur)
        TimeM = TimeCur[!duplicated(ycur, fromLast = FALSE)]#first week of month will be rowname
        
        uniqueClasses <- sort(na.last = T, unique(DFcur$Cls))
        numberOfClasses <- length(uniqueClasses)
        #print(numberOfClasses)
        resultPerClass <- matrix(0, numberOfClasses, 1)
        for (i in 1:numberOfClasses) {
          inClassInd <- which(DFcur$Cls == uniqueClasses[i])
          x = DFcur$Data[inClassInd]
          margin = 1
          x = as.matrix(x)
          resultPerClass[i,] <- apply(X = x,
                                      FUN = FUN,
                                      MARGIN = 2,
                                      ...)
        }#end for number of classes
        y = data.frame(
          Time = TimeM,
          Data = resultPerClass,
          MonthNo = ycur[!duplicated(ycur, fromLast = FALSE)],
          YearNo = yearcur[!duplicated(ycur, fromLast = FALSE)]
        )
        
        rownames(y) = ycur[!duplicated(ycur, fromLast = FALSE)]#first week of month will be rowname
        out = c(out, list(MonthlyCur = y))
      } #end if weekno given
    }#end for uy
    Monthly = do.call(rbind, out)
    #Monthly$Time2=paste0(Monthly$YearNo,'-',gsub(pattern = ' ',replacement = 0,format(Monthly$MonthNo,digits = 2)),'-01')
    chars = paste0(Monthly$YearNo,
                   '-',
                   gsub(
                     pattern = ' ',
                     replacement = 0,
                     format(Monthly$MonthNo, digits = 2)
                   ),
                   '-01')
    rownames(Monthly) = Monthly$Time
    Monthly$Time = as.Date(chars)
    #first approach
    # Dt=tibble::as.tibble(data.frame(TimeTmp=Time,Data=Data))
    #
    # Monthly =dplyr::group_by(Dt,Time=lubridate::floor_date(TimeTmp,Period))
    # Monthly = dplyr::summarise(Monthly,Data=FUN(Data,...))
    # ind=min(which(lubridate::wday(x = Time,week_start=1)==1))
    
    #second approach
    # if(ind!=1){
    #   n=length(Time)
    #   FirstMonday=Time[ind]
    #   Mondaybeforefirst=tail(seq(from=FirstMonday,length.out = 7*4,by='-1 days'),1)
    #   fiskalmonths=as.character(cut (Time[ind:length(Time)], breaks="28 day"))
    #   print(str(fiskalmonths))
    #   fiskalmonths=as.factor(as.Date(c(rep(as.character(Mondaybeforefirst),ind-1),fiskalmonths)))
    #   Monthly=aggregate(Data,by=list(fiskalmonths),FUN)
    #
    # }else{
    #   Monthly=aggregate(Data,by=list(cut (Time, breaks="28 day")),FUN)
    # }
    #
    Monthly = Monthly[, -c(3, 4)]
    if (!missing(Header)) {
      if (length(Header) == 1)
        colnames(Monthly) = c('Time', Header)
      else{
        if (length(Header) == length(colnames(Monthly))) {
          colnames(Monthly) = Header
        } else if (length(Header) == (-1 + length(colnames(Monthly)))) {
          colnames(Monthly) = c('Time', Header)
        } else{
          warning('Length of Header is not matched by number of columns')
          colnames(Monthly) = c('Time', Header)
        }
      }
    }
    return(as.data.frame(Monthly))
  } else{
    MonthlyL = c()
    DateTemp = Data
    for (i in 1:ncol(DateTemp)) {
      if (i == 1)
        MonthlyL = list(as.data.frame(
          aggregateWeeks2FiskalMonths(
            Time = Time,
            Data = DateTemp[, i],
            FUN = FUN,
            Header = Header[c(1,i+1)],
            ...
          )
        ))
      else
        MonthlyL = c(MonthlyL, list(as.data.frame(
          aggregateWeeks2FiskalMonths(
            Time = Time,
            Data = DateTemp[, i],
            FUN = FUN,
            Header = Header[c(1,i+1)],
            ...
          )
        )))
    }
    Monthly = MonthlyL[[1]]
    for (i in 2:length(MonthlyL)) {
      Monthly = merge(Monthly,
                      MonthlyL[[i]],
                      by.x = "Time",
                      by.y = "Time",
                      all = T)#bigger f the two sets defines the time frame of data
    }
    nn = lapply(MonthlyL, nrow)
    TT = rownames(MonthlyL[[which.max(nn)]])
    rownames(Monthly) = TT #as the biggest df defines the time, it can also define the rownames
    #der folgende code sortier fehlende werte am anfang ans ende der matrix
    # print(str(MonthlyL))
    # nn=unlist(lapply(MonthlyL,length))
    # print(nn)
    # TimeOut=as.data.frame(aggregateWeeks2FiskalMonths(Time=Time,Data = DateTemp[,which.max(nn)],FUN=FUN,Header=Header,...)$Time)
    # print(TimeOut)
    #
    # addcol=function(...){
    #   DataVisualizations::CombineCols(...)
    # }
    # Monthly=do.call(addcol,MonthlyL)
    # Monthly=cbind(TimeOut,Monthly)
    if (!is.null(colnames(DateTemp))) {
      colnames(Monthly) = c('Time', colnames(DateTemp))
    } else{
      colnames(Monthly) = c('Time', paste0('C', 1:ncol(DateTemp)))
    }
    return(Monthly)
  }# end isTRUE(Boolean)
}#end function