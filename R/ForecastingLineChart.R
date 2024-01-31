# ForecastingLineChart = ForecastingLineChart(X, Y, F_y,
#                                             xlab='Time',
#                                             y1lab='Test data Y', 
#                                             y2lab='Forecast F',
#                                             main='Forecasting Line Chart',
#                                             cols=c('black','red'),
#                                             SaveIt=FALSE)
#
# Description:
# Plots a line chart comparing test data to forecasted data.
#
# INPUT
# X         Time.
# Y         Test data.
# F_y       Forecasted data.
# 
# OPTIONAL:
# xlab      X  axis label. Default is 'Time'.
# y1lab     Y1 axis label. Default is 'Test data Y'.
# y2lab     Y2 axis label. Default is 'Forecast F'.
# main      Main label. Default is 'Forecasting Line Chart'.
# cols      Colours. Default is c('black','red').
# SaveIt    Boolean, TRUE if plot should be saved as html widget, FALSE else. Default is FALSE.  
#
# OUTPUT
# A plot, optionally saved as an html widget.
#
# Author: 

ForecastingLineChart = function(X,Y,F_y,xlab='Time',y1lab='Test data Y',y2lab='Forecast F',main='Forecasting Line Chart',cols=c('black','red'),SaveIt=FALSE){
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
    htmlwidgets::saveWidget(p, file = "ForecastingLineChart.html")
  }
  return(p)
}