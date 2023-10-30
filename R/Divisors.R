# divisors=Divisors(x)
#
# Description:
# Compute a vector containing all divisors of a non-zero whole number in natural order.
#
# INPUT
# x                   A non-zero whole number.
#
# OUTPUT
# Sorted vector with all divisors of x.
# 
#
#author: JM


Divisors = function(x) {
  if(x == 0) stop("x has to be atleast 1.")
  if(x%%1 != 0) stop("x has to be an integer.")
  if(x < 0) return(Divisors(-x))
  y = seq_len(x)
  return(y[x%%y == 0])
}
