aggregateDays2FiskalMonths=function(Time,Data,FUN,Header,...){
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
    Weekly = aggregateDays2Weeks(
      Time = Time,
      Data = Data,
      FUN = FUN,
      Header = c('Time', 'Data'),
      ...
    )
    outage.zoo <-
      zoo::as.zoo(x = Weekly$Data, order.by = as.Date(Weekly$Time))
    
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
          aggregateDays2FiskalMonths(
            Time = Time,
            Data = DateTemp[, i],
            FUN = FUN,
            Header = Header,
            ...
          )
        ))
      else
        MonthlyL = c(MonthlyL, list(as.data.frame(
          aggregateDays2FiskalMonths(
            Time = Time,
            Data = DateTemp[, i],
            FUN = FUN,
            Header = Header,
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
    # TimeOut=as.data.frame(aggregateDays2FiskalMonths(Time=Time,Data = DateTemp[,which.max(nn)],FUN=FUN,Header=Header,...)$Time)
    # print(TimeOut)
    #
    # addcol=function(...){
    #   rowr::cbind.fill(...,fill=NaN)
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