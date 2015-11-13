require(tseries)
require(quantmod)
require(corpcor)
require(Rglpk)

#if you don't have one of the above packages, use install.packages("packagename")
CVaR <- function(mylist, alpha, rmin, wmin, wmax, weight.sum)
{
  require(Rglpk)
  for(i in 1:length(mylist)){
    mylist[[i]] <- mylist[[i]][index(mylist[[1]])]
  }
  
  r<-na.remove(do.call(merge,lapply(lapply(mylist,Ad),annualReturn))) #extract adjusted close daily returns
  
  n = ncol(r) # number of assets
  s = nrow(r) # number of scenarios i.e. periods
  averet = vector()
  #calculate geometric mean and store in averet
  for (i in 1:length(ticker)){
    averet[i] <- prod((1+r[1:length(r[,1]),i]))^(1/length(r[,1]))-1
  }
  r <- matrix(r, nrow = s, ncol = n)
  # creat objective vector, constraint matrix, constraint rhs
  Amat = rbind(cbind(rbind(1,averet),matrix(data=0,nrow=2,ncol=s+1)),cbind(r,diag(s),1))
  objL = c(rep(0,n), rep(-1/(alpha*s), s), -1)
  bvec = c(weight.sum,rmin,rep(0,s))
  # direction vector
  dir.vec = c("==",">=",rep(">=",s))
  # bounds on weights
  bounds = list(lower = list(ind = 1:n, val = rep(wmin,n)),
                upper = list(ind = 1:n, val = rep(wmax,n)))
  res = Rglpk_solve_LP(obj=objL, mat=Amat, dir=dir.vec, rhs=bvec,
                       types=rep("C",length(objL)), max=T, bounds=bounds)
  w = as.numeric(res$solution[1:n])
  return(list(w=w,status=res$status))
  
  }