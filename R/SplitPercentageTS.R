SplitPercentageTS=function(Datavector,percentage=80){
  if(percentage>1) percentage=percentage/100
  
  N=length(Datavector)
  if(percentage*N%%2!=0){
    Datavector=head(Datavector,N-1)
  }
  N=length(Datavector)
  
  FactorA=1/(1/percentage-1)
  
  # FactorB=N-(N/FactorA)%%2
  # 
  z=1
  for(i in 1:N){
    if(z*FactorA<N)
      z=z+1
  }
  
  # Split=round(N*percentage)
  
  # ltrain=N-Split
  # ltest=Split
  # 
  
  
  # if(N<100)
  #   Split=Split-Split%%2
  # else
  #   Split=Split-Split%%16
  # 
  # Split/(N-Split) ->wholenumber
  # 
  # #The batch size must be evenly divisible into both the training an testing lengths (e.g. training length / batch size and testing 
  # batch=
    
    
  # Train=head(Datavector,N-Split)
  # Test=tail(Datavector,Split)
}