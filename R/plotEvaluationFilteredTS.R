plotEvaluationFilteredTS=function(Time,DataBefore,DataAfter,Short=FALSE,MarkedPoints=NULL,main=''){
  #plotEvaluationFilteredTS(Time,DataBefore,DataAfter,FALSE)
  def.par <-
    par(no.readonly = TRUE) # save default, for resetting...
  if(Short){
    m <-
      graphics::layout(matrix(c(1, 2, 1, 2), 2, 2))
    plot(Time,DataBefore,type='l')
    if(!is.null(MarkedPoints))
      points(Time[MarkedPoints],DataBefore[MarkedPoints],pch=2,col='red')
    plot(Time,DataAfter,type='l')
  }else{
    
  m <-
    graphics::layout(matrix(c(1, 2, 3, 1, 2, 4, 1, 2, 5), 3, 3))
  title(main)
  # par(oma = c(0, 0, 1, 0))#c(u,li,o,re) in
  plot(Time,DataBefore,type='l')
  if(!is.null(MarkedPoints))
    points(Time[MarkedPoints],DataBefore[MarkedPoints],pch=2,col='red')
  plot(Time,DataAfter,type='l')
  requireNamespace('AdaptGauss')
  D2 = DataBefore-DataAfter
  D2=D2[D2!=0]
  pdeVal        = AdaptGauss::ParetoDensityEstimation(D2)
  plot(
    pdeVal$kernels,
    pdeVal$paretoDensity,
    type = 'l',
    xaxs = 'i',
    yaxs = 'i',
    xlab = 'DataBefore-DataAfter',
    ylab = 'PDE',
    col = 'blue',
    main = 'Residuum'
  )
  MinD = min(D2, na.rm = TRUE)
  MaxD = max(D2, na.rm = TRUE)
  par(pty = "s")
  qqnorm(
    D2,
    pch = 20,
    col = "blue",
    axes = TRUE,
    xlim = c(-4.5, 4.5),
    ylim = c(MinD, MaxD),
    main = '',
    xlab = "Normal Distribution",
    ylab = 'Residuum: DataBefore-DataAfter'
  )
  axis(4, col = "black", las = 3) #y-Achse
  grid(lty = 'dashed', col = 'black')
  mtext(
    'Normal QQ-Plot',
    side = 3,
    line = 0,
    cex = 1,
    col = "black"
  )
  plot(D2, type = 'l', ylab = 'DataBefore-DataAfter')

  }
  par(def.par)
}