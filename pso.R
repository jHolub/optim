source("agarwal.R")
source("stehfest.R")
source("core.R")

field<- read.table("./field.txt", header=T, sep="\t")
colnames(field) <-c("td","sd")
# SKIN EFFECT
skMax = 100
skMin = 0.1
gaz <- runif(10, skMin, skMax)
vsk = runif(10, 0.0, 1)


logspace <- function( d1, d2, n) exp(log(10)*seq(d1, d2, length.out=n))
#WELLBORE STORAGE
cdMax = 10000000000
cdMin = 1
baz <- logspace( 1, 10, 10)
vcd = runif(10, 0.0, 100.0)
#cd = 4000000

csk1 = 2
csk2 = 2

ccd1 = 2
ccd2 = 2

gTotal = 100000

for(i in 1:500){

  pTotal = 100000;
  
  for(sk in gaz){
    
    for(cd in baz){
      
      total = getResult(field, cd, sk)
      
      if(total < pTotal){
        pTotal = total
        pbest['sk'] = sk 
        pbest['cd'] = cd
      }
      
      if(total < gTotal){
        gTotal = total
        gbest['sk'] = sk 
        gbest['cd'] = cd 
      }
      
    }
  }
  
  print(gbest['sk'])
  print(gbest['cd'])
  print(gTotal)
  print(gaz)
  print(baz)
  print('')

  j = 1
  for(present in gaz){
    
    if(pTotal > gTotal){
      vsk[j] = vsk[j] + (csk1 * runif(1) * ( pbest['sk'] - present)) + (csk2 * runif(1) * (gbest['sk'] - present))
    }else{
      vsk[j] = vsk[j] + (csk1 * runif(1) * ( pbest['sk'] - present))  
    }
    
    gaz[j] = present + vsk[j]
    
    if(gaz[j] < skMin){
      gaz[j] = skMin
      vsk[j] = 0
    }
    
    if(gaz[j] > skMax){
      gaz[j] = skMax
      vsk[j] = 0
    }

    j = j + 1
  }
  
  
  
  j = 1
  for(present in baz){
    
    if(pTotal > gTotal){
      vcd[j] = vcd[j] + (ccd1 * runif(1) * ( pbest['cd'] - present)) + (ccd2 * runif(1) * (gbest['cd'] - present))
    }else{
      vcd[j] = vcd[j] + (ccd1 * runif(1) * ( pbest['cd'] - present)) 
    }
    
    baz[j] = present + vcd[j]
    
    if(baz[j] < cdMin){
      baz[j] = cdMin
      vcd[j] = 0
    }
    
    if(baz[j] > cdMax){
      baz[j] = cdMax
      vcd[j] = 0
    }
    j = j + 1
  }
  
}
