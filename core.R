
getResult <- function(field, sk, cd){ 

  modelSd <- stehfest(field[1], cd, sk)
  #MSE    
  total = 0
  account = 0
  
  for (i in 1:dim(field)[1]) 
  {
    total = total + ( (field[i,2] - modelSd[i,2]) * (field[i,2] - modelSd[i,2]) )
    account = account + 1
  }
  
  #MSE result
  MSE = total / account
  
  return (total)

}