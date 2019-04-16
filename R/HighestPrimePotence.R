HighestPrimePotence = function(x) {
  if(x == 0) return(0)
  i = 2
  curr = 1
  max = 1
  while(abs(x) != 1) {
    if(x%%i == 0) {
      curr = curr * i
      x = x / i
    } else {
      if(curr > max) max = curr
      curr = 1
      i = i + 1
    }
  }
  return(max(max, curr))
}