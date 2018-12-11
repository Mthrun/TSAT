PlotOptimalAlignment=function(FirstVector,SecondVector,Type='threeway',FirstTime,SecondTime,Frequency,...){

 #INPUT
#FirstVector
  #SecondVector
  #OPTIONAL
#Type alignment, twoway, threeway, ensity
  #FirstTime
  #SecondTime
  #Frequency
#Output
# dtw object in invisible mode
  #author: MT, 2018
  #[Giorgino, 2009]  Giorgino, T.: Computing and visualizing dynamic time warping alignments in R: the dtw package, Journal of statistical Software, Vol. 31(7), pp. 1-24. 2009.
  
   if(missing(Frequency)) Frequency='days'
  
  if(missing(FirstTime)){
    TS1=ts(FirstVector,frequency = 1)
  }else{
    TS1=ConvertNumerical2TSobject(FirstVector,FirstTime,Frequency=Frequency) 
  }
  if(missing(FirstTime)){
    TS2=ts(SecondVector,frequency = 1)
  }else{
    TS2=ConvertNumerical2TSobject(SecondVector,SecondTime,Frequency=Frequency)
  }
 
  out=dtw::dtw(FirstVector,SecondVector,keep.internals = TRUE)
  dtw::dtwPlot(out,type=Type,...)
  return(invisible(out))
  
}