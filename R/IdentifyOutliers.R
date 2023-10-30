IdentifyOutliers <- function(Datavec,Time, n_sigmas=3,p=0.1,lowInnerPercentile = 25,PlotIt=FALSE) {
  #IdentifyOutliers(ElectricityBRD$Mrd_KWh,ElectricityBRD$Time,n_sigmas = 1,PlotIt = T)
  #Datavec              [1:n] numerical vector of timeseries
  #Optional
  #Time                 vector of time, default 1:n
  #n_sigmas             number of standard deviation outside which its an outlier
  #p                    Optional, percent of the top- and bottomcut from x
  #lowInnerPercentile   Optional, standard deviation aproximated by percentilinterval.
  #PlotIt               TRUE: plots data
  #OUTPUT
  #IsOutlier            [1:n] boloean vector: TRUE is outlier, FALSE is not
  #author               2023, MCT
  label=deparse1(substitute(Datavec))
  x <- Datavec
  
  if(missing(Time)) Time=1:length(x)

  mu <- Meanrobust(x,p = p)
  sigma <- Stdrobust(x,lowInnerPercentile = lowInnerPercentile)
  up=as.vector(mu + n_sigmas*sigma)
  down= as.vector(mu - n_sigmas * sigma)
  IsOutlier=x > up | x <down
  
  if(isTRUE(PlotIt)){
    def.par <-
      par(no.readonly = TRUE) # save default, for resetting...
    on.exit(par(def.par))
                
    m <-graphics::layout(matrix(c(1, 2, 1,2), 2, 2))
    
    PDEnormrobust(Datavec,xlim=c(down*0.95,up*1.05),xlab=label)
    abline(col="red",v=up)
    abline(col="red",v=down)
    # 
    plot(Time,Datavec,type="l",col="black",ylab=label,xlab="Time")
    points(Time[IsOutlier],Datavec[IsOutlier],col="red",pch=17)
  }
  return(IsOutlier)
}