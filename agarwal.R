#Laplace transform function, skin effect, wellbore storage*/
#Cd - storage well, W - skin effect, p - parametr
  
  Agarwal <- function(p, Cd, W){
    
    div = p * ( sqrt(p) * besselK(sqrt(p),1) + Cd * p * (besselK(sqrt(p),0) + W * sqrt(p) * besselK(sqrt(p),1)) )
    
    sd = ( besselK(sqrt(p), 0) + (W * sqrt(p) * besselK(sqrt(p), 1))) / div
    
    return (sd)
  }
  
  