# endcapital = InterestRate=function(EingangsKapital,Zins,Tage)
#
# DESCRIPTION
# Calculates the final capital through the accumulation factor (Aufzinsungsfaktor) initialCapital*((1+interest/100) to the power of time)
# where
#
# INPUT
# initialCapital        Numeric value of initial capital
# interest              Numeric value of the interest rate
# days                  Numeric value of days, for time of accumulation
#
# OUTPUT
# finalCapital          Final capital value
#
# author MCT

InterestRate=function(initialCapital,interest,days) {
  finalCapital=initialCapital*(1+interest/100)^(days/365.25)
  return(finalCapital)
}