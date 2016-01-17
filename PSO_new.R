source("agarwal.R")
source("stehfest.R")
source("core.R")

#field data
field<- read.table("./field.txt", header=T, sep="\t")
colnames(field) <-c("td","sd")

#*** PSO Kennedy – Eberhart, 1995 ***

# inicializace hejna

Skmin = 0
Skmax = 100

Cdmin = 1
Cdmax = 10000000000

#velikost populace
npop = 20
dim = 2
X = matrix(nrow=(dim+1), ncol=npop)

logspace <- function(arg){return(10**arg)}

for( i in 1:npop){
  X[1, i] = runif(1,min=Skmin,max=Skmax)
  X[2, i] = logspace(runif(1,min=log10(Cdmin),max=log10(Cdmax)))
  X[3, i] = getResult(field, X[1,i], X[2,i])
}

V = matrix(0,nrow=(dim), ncol=npop)

bestind=which.min(X[(dim+1),])
gbest=X[,bestind]
pbest = X

#pocet iteraci
ngen = 200

out_k <- numeric()
out_gbest <- numeric()

for(k in 1:ngen){
  
  bestind=which.min(X[(dim+1),])
  if (gbest[dim+1]>X[(dim+1),bestind]) gbest=X[,bestind]
  
  for(i in 1:npop){
    if (pbest[(dim+1),i] > X[(dim+1),i])  pbest[,i]=X[,i]
  }
  

  for(i in 1:npop){
    for(j in 1:dim){
      Vhelp = V[j,i] + 2*runif(1)*(gbest[j]-X[j,i])+2*runif(1)*(pbest[j,i]-X[j,i])
      if(j == 1){
        Vmax = Skmax / 2
        if( abs(Vhelp) > Vmax) {
          if (Vhelp > Vmax) V[j,i] = Vmax 
          else V[j,i] = -Vmax
        }
        else V[j,i]=Vhelp
      }else{
        Vmax = Cdmax / 10000
        if( abs(Vhelp) > Vmax) {
          if (Vhelp > Vmax) V[j,i] = Vmax 
          else V[j,i] = -Vmax
        }
        else V[j,i]=Vhelp       
      }
      
      X[j,i]=X[j,i]+V[j,i]
    }
  }
  
  for(i in 1:npop){
      X[dim+1,i] = getResult(field, X[1,i], X[2,i])
  }
#print(X)
    print(gbest)
    print(k)
    out_k <- c(out_k, k)
    out_gbest <- c(out_gbest, gbest[3])
}


plot(out_k,out_gbest,type="l",col="red", ylim=c(0,10), xlim=c(0,200))



