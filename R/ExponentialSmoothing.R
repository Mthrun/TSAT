ExponentialSmoothing=function(Time,Data,Type='StateSpaceModel',PlotIt,...){
  
  # require(smooth)
  # require(greybox)
  # require(Mcomp)
  requireNamespace('lubridate')
    requireNamespace('forecast')
  warning('UnderDevelopment')
  
  switch(Type)(
  
  SSOE={dat=ts(Weekly$Data, freq=365.25/7, start=lubridate::decimal_date(lubridate::ymd(min(Weekly$Time))))
  a=smooth::es(dat, h=18, holdout=TRUE, silent=FALSE, model=c("CCC","ANN","AAN","AAdN","ANA","AAA","AAdA"))
  },
  
  exponentialSmoothing={model=forecast::ses(Weekly$Data, h=12, alpha=NULL)#,initial="optimal",exponential=F,seasonal="additive")
  plot(model)
  },
  
  HoltWinters={mod1 <- HoltWinters(Weekly$Data, ...)#alpha=0.1, beta=FALSE, gamma=FALSE)
  predict(mod1, n.ahead=12)
  },
  #http://uc-r.github.io/ts_exp_smoothing
  StateSpaceModel={qcement.hw <- ets(dat, model = "MAM",...)#model = "AAA")
  autoplot(forecast(qcement.hw))
  },
  
  StructTS={
    StructTS(dat,...)
  }
  )
}