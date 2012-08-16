rm(list=ls())
#logistics data
dat = array(dim = 81)
dat[1]=81
x = seq(-3,3,.151)
dat[2:41]  = (sign(x+rnorm(40,0,.5))+1)/2
dat[42:81] = x
n=2
fr <- function(beta) {
  ll=0
  for(i in 1:40){
   z = beta[1] + beta[2]*dat[i+41]
   #ll=ll - ( dat[i+1]*z - log( exp ( z ) + 1))
   ll = ll - (dat[i+1] * log (1/(exp(-z) +1)) + (1-dat[i+1])*log (1 - 1/(exp(-z) +1)))
  }
  return(ll);
}
beta=array(dim=2)
beta[1]=0
beta.seq = seq(0,4,.1)
ll = array(dim=length(beta.seq))
for(i in 1:length(beta.seq)){
  ll[i]=fr(c(beta[1],beta.seq[i]))
}
