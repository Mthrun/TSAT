PlotEventCounts=function(Counts,Names=NULL,main='Event Counts',xlab='Events',  ylab='Frequencies',order=F){
  #PlotEventCounts(Counts,Names=NULL,main='Event Counts',xlab='Events',ylab='Frequencies')
  nB <- length(Counts)
  Breaks=1:nB
  if(is.null(Names))
    Names=Breaks
  MiN=1
  MaX=nB
  Counts=as.numeric(Counts)
  if(order){
    tmp=order(Counts,decreasing = T,na.last = T)
    Names=Names[tmp]
    y <- Counts[tmp]
  }else{
    y <- Counts
  }




    plot(x = c(MiN-0.5, MaX+0.5), 
         y = c(0, max(Counts, na.rm = TRUE) * 1.2), type = "n", 
         main = main, xaxs = "i", yaxs = "i", axes = FALSE, 
         xlab = xlab, ylab = ylab, xlim = c(MiN-0.5, 
                                            MaX+0.5), ylim = c(0, max(Counts, 
                                                                      na.rm = TRUE) * 1.2))
    par(mgp = c(2.2, 0.6, 0))
    rect(Breaks-0.5, 0, Breaks+0.5, y, col = "blue", border = "light blue", 
         xlab = "", ylab = ylab, xlim = c(MiN-0.5, 
                                          MaX+0.5), ylim = c(0, max(Counts, 
                                                                    na.rm = TRUE) * 1.2))
    axis(1, col = "black", las = 1, xaxs = "i",at=Breaks,labels = Names)
    axis(2, col = "black", las = 1, yaxs = "i")
  
}