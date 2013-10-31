
rm(list=ls())
#Challenger Data Set
x = c(53,57,58,63,66,67,67,67,68,69,70,70,70,70,72,73,75,75,76,76,78,79,81)
y = c( 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0)

fr <- function(beta) {
  ll = 0
  for(i in 1:23){
    z = beta[1] + beta[2]*x[i]
    ll = ll - ( y[i]*z - log(exp(z) + 1))
  }
  return(ll);
}

gr <- function (beta) {
  g1 = 0
  g2 = 0
  for(i in 1:23){
    z = beta[1] + beta[2]*x[i]
    temp = (y[i] - 1 / (exp(-z) + 1))
    g1 = g1 - temp
    g2 = g2 - temp*x[i]
  }
  return(c(g1,g2))
}

print(optim(c(0,1),fr,gr,method = "L-BFGS-B"))
