# p = PlotTimeSeries(X, Y, xlab="X", ylab="Y", main="Timeseries by Plotly", col="black", SaveIt=FALSE)
#
# DESCRIPTION:
# Plots time series as a ploty object for interactive purposes like zooming or investigating specific points of time with mouse
#
# INPUT
# X               [1:n] vector, both lines require the same xvalues, e.g. the time of the time series, POSIXlt or POSIXct are accepted     
# Y               [1:n] vector of first line
# OPTIONAL
# xlab            String for xlabel. Default is "X"
# ylab            String for ylabel. Default is "Y"
# Title           Title of plot. Default is "Timeseries by Plotly"
# col             Color of two lines. Default is "black"
# SaveIt          TRUE if you want to save plot as html in getwd() directory. Default is FALSE
#
# OUTPUT 
# p               A plotly object
#
# DETAILS:
# Enables to visualize the time series interactively, using plotly
#
# NOTE:
# Wrapper in order to unify Input and Output
#
#
# Author: MCT

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