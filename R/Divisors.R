Divisors = function(x) {
  if(x == 0) stop("x has to be atleast 1.")
  if(x%%1 != 0) stop("x has to be an integer.")
  if(x < 0) return(Divisors(-x))
  y = seq_len(x)
  return(y[x%%y == 0])
}
