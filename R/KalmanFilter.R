KalmanFilter=function(Datavector,DLMobject,PlotIt=TRUE,Short=FALSE){
  #Kalman filter program to predict, filter, and smooth related to the material in Section 10.6 4 in Applied Time Series Analysis with R, second edition by Woodward, Gray, and Elliott
  # Input
  # Datavector  [1:n], the univariate data set to be analyzed
  # DLMobject   Opotional, basically a list with scalar Parameters as defined by dlm::dlm, FF, V,  GG, W,  m0, C0 can be set, 
  #             details for Parameter estimation in Petris, Giovanni: An R Package for Dynamic Linear Models, Journal of Statistical Software, 2010
  #             or in the dlmMLE function of dlm package
  # OUTPUT
  # FilteredData [1:n] Data without noise, where noise is defined by model
  # FULL        Output of dlmFilter function of dlm package
  # ParametersDLM DLMobject used for KalmanFiltering
  
  # info: https://magesblog.com/post/2015-01-06-kalman-filter-example-visualised-with-r/
  
  requireNamespace('dlm')
  
  stdrobust=function (x, lowInnerPercentile = 25/100) 
  {
    if (is.vector(x) || (is.matrix(x) && dim(x)[1] == 1)) 
      dim(x) <- c(length(x), 1)
    lowInnerPercentile <- max(1, min(lowInnerPercentile, 49))
    hiInnerPercentile <- 100 - lowInnerPercentile
    faktor <- sum(abs(qnorm(t(c(lowInnerPercentile, hiInnerPercentile)/100), 
                            0, 1)))
    std <- sd(x, na.rm = TRUE)
    p <- c(lowInnerPercentile, hiInnerPercentile)/100
    quartile <- quantile(x, p, type = 5, na.rm = TRUE)
    if (ncol(x) > 1) 
      iqr <- quartile[2, ] - quartile[1, ]
    else iqr <- quartile[2] - quartile[1]
    shat <- c()
    for (i in 1:ncol(x)) {
      shat[i] <- min(std[i], iqr[i]/faktor, na.rm = TRUE)
    }
    dim(shat) <- c(1, ncol(x))
    colnames(shat) <- colnames(x)
    return(shat)
  }
  
  if(missing(DLMobject)){
    m0=mean(Datavector, trim = 0.1, na.rm = TRUE) #roboust mean
    s0=stdrobust(Datavector) #robust std
    DLMobject=dlm::dlm(FF = 1, V = 1, GG = 1, W = 1, m0 = m0, C0 = s0)
    # return(DLMobject)
  }
  out=dlm::dlmFilter(Datavector,DLMobject)
  FilteredData=out$m[2:length(out$m)]
  if(PlotIt){
    plotEvaluationFilteredTS(1:length(Datavector),Datavector,FilteredData,Short = Short,MarkedPoints = NULL)
  }
  return(invisible(list(FilteredData=FilteredData,FULL=out,ParametersDLM=DLMobject)))
}

## OLD Code, works, but parameters are unclearly documented by tswge packages
# function(Datavector,start=0,gam0=1,Fscalar=0.9,gamV=1,G=1,gamW=0.75,PlotIt=TRUE,Short=FALSE)
# output=tswge::kalman.wge(y=Datavector,start=start, gam0=gam0, F=Fscalar, gamV=gamV, G=G, gamW=gamW)
# start	      the scalar version of Xo in item (c) following the state equation (10.47) of the text
# gam0	      the scalar version of Gamma(0) discussed in item (c) following the state equation
# Fscalar	    scalar version of the matrix F in the state equation
# gamV	      the value Gamma(v) specified in item (b) following the state equation
# G	          the scalar observation matrix specified in the observation equation as G(t)
# gamW	      the variance of the (univariate) white noise denoted by Gamma(w) in item (c) following (10.48)
# OUTPUT
# pfs	
# a table giving results such as those in Table 10.1 in Woodward, Gray, and Elliott book
# MT: still only a working solution, to be expented
#plotEvaluationFilteredTS(1:length(Datavector),output[,"Prediction"],output[,"Data"],Short = Short,MarkedPoints = NULL)