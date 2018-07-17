PlotTimeSeries=function(X,Y,xlab='X',ylab='Y',main='Timeseries by Plotly',col='black',SaveIt=FALSE){
  requireNamespace('plotly')
  
  p <- plotly::plot_ly()
  p <- plotly::add_lines(p,x = ~X, y = ~Y, name = ylab, line = list(color=col))
      p<-plotly::layout(p,
        title = main, 
        xaxis = list(title=xlab,
                     showgrid=FALSE),
        yaxis= list(
          title=ylab,
          showgrid=FALSE
        )
      )
  p
  if(SaveIt){
    requireNamespace('htmlwidgets')
    htmlwidgets::saveWidget(p, file = "TimeSeriesPlot.html")
  }
  return(p)
}