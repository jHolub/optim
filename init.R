
source("agarwal.R")
source("stehfest.R")
source("core.R")

field<- read.table("./field.txt", header=T, sep="\t")
colnames(field) <-c("td","sd")

# log seq
logspace <- function( d1, d2, n) exp(log(10)*seq(d1, d2, length.out=n))

gaz <- seq(from=1, to=100, by=5)

baz <- logspace( 1, 10, 20) 
#baz <- seq(from=1, to=100000000, by=5000000)

res = 10000

ctotal <- numeric()
ccd <- numeric()
csk <- numeric()

for(sk in gaz){
  
  for(cd in baz){
    
    total = getResult(field, cd, sk)
    
    ctotal <- c(total,ctotal )
    ccd <- c(ccd, cd )
    csk <- c(csk, sk )
    
    if(res > total){
      print(cd)
      print(sk)
      print(total) 
      res = total
    }
    
  }
}


bazz <- numeric()
for(cd in baz){
  bazz <- c(bazz, log10(cd) ) 
}


B = matrix(ctotal,nrow=20,ncol=20) 
z <- B


persp(
  gaz, bazz, z, theta = 250, phi = 30, expand = 0.5, col = "lightblue",
  ltheta = 120, shade = 0.75, ticktype = "detailed",
  col=rainbow(50),
  xlab = "X", ylab = "Y", zlab = "z"
)


persp3d(
  gaz, bazz, z, theta = 90, phi = 150, expand = 0.5, col = "lightblue",
  ltheta = 120, shade = 0.75, ticktype = "detailed",
  #ylim = range(1,1000000000),
  xlog = TRUE, xlab = "X", ylab = "Y", zlab = "z"
)



#scatterplot3d( csk, ccd, ctotal ,main="3D Scatterplot", log="x", type='h')


