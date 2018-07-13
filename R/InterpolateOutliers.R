InterpolateOutliers=function(Time,Datavector,OutliersTime,option = 'stine',PlotIt=TRUE){
  
  DataBefore=Datavector
  ind=Time %in% OutliersTime
  Datavector[ind]=NaN
  
  # DataAfter=imputeTS::na.interpolation(Datavector,option = 'spline')
  # DataAfter[ind]=NaN
  DataAfter=imputeTS::na.interpolation(Datavector,option = option)
  
  if(PlotIt){
  def.par <-
    par(no.readonly = TRUE) # save default, for resetting...
  m <-
    graphics::layout(matrix(c(1, 2, 3, 1, 2, 4, 1, 2, 5), 3, 3))
  # par(oma = c(0, 0, 1, 0))#c(u,li,o,re) in
  plot(Time,DataBefore,type='l')
  points(Time[ind],DataBefore[ind],pch=2,col='red')
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
  par(def.par)
  }
  return(invisible(DataAfter))
}