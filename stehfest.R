# FUNCTION return bezrozmerny cas a bezrozmenrne snizeni
stehfest <- function (tdPar, Cd, W) {

  #STEHFEST TERMS 
  N = 10
  #Vi presented by Walton(1996)
  Vi <- c(0.08333333333333333, -32.08333333333334, 1279.0, -15623.66666666667, 84244.1666666666, -236957.5, 375911.66666666667, - 340071.6666666667, 164062.5, -32812.5);
  
  fsd <- numeric()
  ftd <- numeric()
  
  for(td in tdPar){
    
    sum = 0
    i = 1
    
    while (i <= N) {
      
      k = (i + i) / 2
      p = i * (log(2) / td)
      sum = sum + (Vi[i] * Agarwal(p, Cd, W))
      i = i + 1
      
    }
    # groundwater function - dimensionless drawdowns vector
    ftd <- c(ftd, td )
    fsd <- c(fsd, (log(2) / td) * sum )
  }
  
  fres <- matrix(c(ftd,fsd),nrow=length(ftd))
  colnames(fres) <-c("td","sd")
  return (fres)
}


