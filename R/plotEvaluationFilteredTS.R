# V=plotEvaluationFilteredTS=function(Time,DataBefore,DataAfter,Short=FALSE,MarkedPoints=NULL,main='')
#
# DESCRIPTION
# A dashboard for TS filters and forecasting
#
# INPUT
# Time                  [1:n] vector of time, as.Date or POSIXct objects are accepted
# DataBefore            [1:n] numerical vector of data before filtering
# DataAfter             [1:n] numerical vector of data after filtering, where filtered values are not finite
# OPTIONAL
# Short                 If TRUE: Only times series plots of original data and filtered data are plotted.
#                       If FALSE, include further plots, including Boxplot, Normal-QQ-Plot, Residual-Plots. 
#                       Default is FALSE
# MarkedPoints          [1:k] vector of indices of points to be highlighted. Default is NULL 
#                       Default are german holidays, see TSAT::GermanHolidays$Time
# main                  String/character for title of plot. Default is no title
#
#
# author MCT

plotEvaluationFilteredTS=function(Time,DataBefore,DataAfter,Short=FALSE,MarkedPoints=NULL,main=''){

  tryCatch({
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
      
    #m <- graphics::layout(matrix(c(1, 2, 3, 1, 2, 4, 1, 2, 5), 3, 3))
     m <- graphics::layout(matrix(c(1, 2, 3, 1, 2, 3, 4, 5, 6), 3, 3))
      
    #title(main)
    # par(oma = c(0, 0, 1, 0))#c(u,li,o,re) in
    plot(Time,DataBefore,type='l',main = main)
    if(!is.null(MarkedPoints))
      points(Time[MarkedPoints],DataBefore[MarkedPoints],pch=2,col='red')
    plot(Time,DataAfter,type='l')
    requireNamespace('DataVisualizations')
    #[Tukey, 1977]  Tukey, J. W.: Exploratory data analysis, United States Addison-Wesley Publishing Company, ISBN: 0-201-07616-0, 1977.
    
    #page 113
    Residuals = DataBefore-DataAfter
    #indRes = which(Residuals != 0) # added
    out=WhiteNoiseTest(Residuals,PlotIt=FALSE)
    if(!is.null(out))
      pval=out@test$p.value[1]
    else
      pval=1
    
    if(pval>0.001){
      pval=round(pval,4)
      string=paste("Residuals are white noise: p.val",pval[1])
    }else{
      string=paste("Residuals are white noise: p.val<0.001")
    }
    Residuals=Residuals[Residuals!=0]
    #plot(Time[indRes],Residuals, type = 'l', ylab = 'DataBefore-DataAfter') # added
    plot(Time,Residuals, type = 'l', ylab = 'DataBefore-DataAfter')
    pdeVal        = DataVisualizations::ParetoDensityEstimation(Residuals)
    plot(
      pdeVal$kernels,
      pdeVal$paretoDensity,
      type = 'l',
      xaxs = 'i',
      yaxs = 'i',
      xlab = 'Residuals (blue), Gaussian Distribution (magenta)',
      ylab = 'PDE',
      col = 'blue',
      main = string
    )
    Normaldist <- dnorm(pdeVal$kernels,mean(Residuals,na.rm=TRUE),sd(Residuals,na.rm=TRUE))
    points(pdeVal$kernels,Normaldist,type='l',col='magenta')
    abline(v =0,col='red')
    MinD = min(Residuals, na.rm = TRUE)
    MaxD = max(Residuals, na.rm = TRUE)
    par(pty = "s")
    qqnorm(
      Residuals,
      pch = 20,
      col = "blue",
      axes = TRUE,
      xlim = c(-4.5, 4.5),
      ylim = c(MinD, MaxD),
      main = '',
      xlab = "Normal Distribution",
      ylab = 'Residuals: DataBefore-DataAfter'
    )
    qqline(Residuals, col = 2)
    axis(4, col = "black", las = 3) #y-Achse
    grid(lty = 'dashed', col = 'black')
    mtext(
      'Normal QQ-Plot',
      side = 3,
      line = 0,
      cex = 1,
      col = "black"
    )
   boxplot(Residuals)

  }
  par(def.par)
  },error=function(e) warning(e))
}