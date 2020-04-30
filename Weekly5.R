lambda_a <- 6
lambda_d <- 1
r <- 1.3
cost <- function(y){20 + y}
s <- 180
S <- 350
L <- 24
h <- 0.01


simF=function(hours){
  i=1
  t <- 0
  profit <- 0
  stock <- S
  order <- 0
  t_arrival <- rexp(1,6)
  t_delivery <- Inf
  totalOrderMissed=0
  timetoHour=1
  mat <- matrix(c(t,t_arrival,t_delivery,profit,stock,order,totalOrderMissed,timetoHour),nrow=1,ncol=8)
  colnames(mat)=c("t","t_arrival","t_delivery","profit","stock","order", "Orders Missed", "Time to Hour")
  
  while(mat[i,1] <hours){
    if ((mat[i,8]<mat[i,3]) && (mat[i,8]<mat[i,2])){
      mat=updateHoldings(i,mat)
    }
    else if(mat[i,3] < mat[i,2]){
      mat=updateDelivery(i,mat)
    }
    else{
      mat =updateArrival(i,mat)
    }
    i=i+1
  }
  return(mat)
}

updateDelivery=function(i,mat){
  t=(mat[i,1] + mat[i,3])
  profit=mat[i,4]-cost(mat[i,6])
  stock=mat[i,5]+mat[i,6]
  order=0
  t_arrival=mat[i,2]-mat[i,3]
  timetoHour=mat[i,8]-mat[i,3]
  t_delivery= Inf
  totalOrderMissed = mat[i,7]
  mat <- rbind( mat, c(t,t_arrival,t_delivery,profit,stock,order,totalOrderMissed,timetoHour) )
  return (mat)
}



updateArrival=function(i,mat){
  t=( mat[i,1] ) + ( mat[i,2] )
  t_delivery=(mat[i,3] - mat[i,2])
  timetoHour=mat[i,8]-mat[i,2]
  t_arrival <- rexp(1,6)
  demand=rpois(1,lambda_d)
  if(demand > (mat[i,5])){
    totalOrderMissed = mat[i,7]+ (demand - mat[i,5])
    demand=mat[i,5]
  }
  else{
    totalOrderMissed = mat[i,7]
  }
  profit=(mat[i,4])+(r*demand)
  stock=(mat[i,5])-demand
  if(stock <= s && mat[i,6] == 0){
    order=(S-stock)
    t_delivery=L
  }
  else{
    order=mat[i,6]
  }
  mat <- rbind( mat, c(t,t_arrival,t_delivery,profit,stock,order,totalOrderMissed,timetoHour) )
  return (mat)
}


updateHoldings=function(i, mat){
  t=( mat[i,1] ) + ( mat[i,8] )
  t_delivery=(mat[i,3] - mat[i,8])
  t_arrival=mat[i,2]-mat[i,8]
  timetoHour=1
  stock=mat[i,5]
  order=mat[i,6]
  totalOrderMissed=mat[i,7]
  profit=mat[i,4]-h*mat[i,5]
  mat <- rbind( mat, c(t,t_arrival,t_delivery,profit,stock,order,totalOrderMissed,timetoHour) )
  return (mat)
}


