TimeSeriesLineChart=function(X,Y,xlab='Time',y1lab='Values of Time Series',y2lab='Approximated Curve',main='Time Series',cols=c('black','red'),SaveIt=FALSE){
  requireNamespace('plotly')

  
  p <- plotly::plot_ly() 
    p <- plotly::add_lines(p,x = ~X, y = ~Y, name = y2lab, line = list(color=cols[1])) 
    p <-  plotly::add_trace(p,x = ~X,y = ~Y, name= y1lab, marker = list(color=cols[2]))
    p <-  plotly::layout(p,
      title = main, 
      xaxis = list(title=xlab,
                   showgrid=T),
      yaxis= list(
        title=paste('Values of Time Series'),
        showgrid=T
        ),
      legend = list(x = 0.7, y = 0.98)
      
    )
	p
  if(SaveIt){
    requireNamespace('htmlwidgets')
    htmlwidgets::saveWidget(p, file = "ForecastingLineChart.html")
  }
  return(p)
}