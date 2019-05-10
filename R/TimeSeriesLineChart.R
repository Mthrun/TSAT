TimeSeriesLineChart=function(Time,Values,xlab='Time',Resolution='AsIs',y1lab='Values of Time Series',y2lab='Approximated Curve',main='Time Series',cols=c('black','red'),LegendPos='topright',Save=FALSE,...){
  requireNamespace('plotly')

  switch(Resolution,
    AsIs={
      #do nothing
    },
    Weekly={
      V=aggregateDays2Weeks(Time,Values,...)
      Time=V$Time
      Values=V$Data
    },
    Monthly={
      V=aggregateDays2Months(Time,Values,...)
      Time=V$Time
      Values=V$Data
    },
    {#defualt do nothing
    }
  )
  switch(LegendPos,
    topleft={
      x = 0.05
      y = 0.98
    },
    topright={
      x = 0.7
      y = 0.98
    },
    bottomleft={
      x = 0.05
      y = 0.1
    },
    bottomright={
      x = 0.7
      y = 0.1
    },
    {
      warning('Please select legend position.')
      x=0.5
      y=0.5
    }
  )
  p <- plotly::plot_ly() 
    p <- plotly::add_lines(p,x = ~Time, y = ~Values, name = y2lab, type='scatter', line = list(color=cols[1])) 
    if(cols[2]!='white')
      p <-  plotly::add_trace(p,x = ~Time,y = ~Values, name= y1lab, type='scatter',mode='marker',marker = list(size = 10,color=cols[2]))
    p <-  plotly::layout(p,
      title = main, 
      xaxis = list(title=xlab,
                   showgrid=T),
      yaxis= list(
        title=paste('Values of Time Series'),
        showgrid=T
        ),
      legend = list(x = x, y = y)
      
    )
	p
  if(isTRUE(Save)){
    requireNamespace('htmlwidgets')
    htmlwidgets::saveWidget(p, file = paste0(Resolution,"_",main,".html"))
  }
  return(p)
}