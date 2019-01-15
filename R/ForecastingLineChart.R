ForecastingLineChart=function(X,Y,F_y,xlab='Time',y1lab='Test data Y',y2lab='Forecast F',main='Forecasting Line Chart',cols=c('black','red'),SaveIt=FALSE){
  requireNamespace('plotly')

  
  p <- plotly::plot_ly() 
    p <- plotly::add_lines(p,x = ~X, y = ~Y, name = y1lab, line = list(color=cols[1])) 
    p <-  plotly::add_lines(p,x = ~X, y = ~F_y, name = y2lab, line = list(color=cols[2])) 
    p <-  plotly::layout(p,
      title = main, 
      xaxis = list(title=xlab,
                   showgrid=FALSE),
      yaxis= list(
        title=paste(y1lab,'versus',y2lab),
        showgrid=FALSE
        )
    )
	p
  if(SaveIt){
    requireNamespace('htmlwidgets')
    htmlwidgets::saveWidget(p, file = "DualAxisLineChart.html")
  }
  return(p)
}