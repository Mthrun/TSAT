InterestRate=function(EingangsKapital,Zins, Tage){
  Endkaptial=EingangsKapital*(1+Zins/100)^(Tage/365.25)
  return(Endkaptial)
}