rm(list=ls())
#Challenger Data Set
x = c(53,57,58,63,66,67,67,67,68,69,70,70,70,70,72,73,75,75,76,76,78,79,81)
y = c( 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0)

lr.solve = function(x,y=NULL){
	x.mat=as.matrix(x)
	n=dim(x.mat)
	m=n[1] ; n=n[2]
	if(!is.null(y)){
	  X=matrix(c(rep(1,m),x.mat),ncol=n+1)
		Y = matrix(y,m,1)
		beta=matrix(rep(0,n),n,1)
		b=matrix(rep(10,n),n,1)
	}
	else{
	  X=matrix(c(rep(1,m),x.mat[,-n]),ncol=n)
		Y = x.mat[,n]
		beta=matrix(rep(0,n),n,1)
		b=matrix(rep(10,n),n,1)
	}
	while(norm(matrix(beta-b))>1e-5){
		b=beta
		p=1/(1+exp(-X%*%b))
		G=t(X)%*%diag(-c(p*(1-p)))%*%X
		beta=b-solve(G,t(X)%*%(Y-p))
	}
	return(beta)
}
iris.y=iris
iris.y$Species = as.numeric((iris$Species=='virginica'))
lr.solve(iris.y)
