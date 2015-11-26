#load library fPortfolio
require(fPortfolio)
require(Rglpk)
require(tseries)
require(quantmod)
#install.packages("fPortfolio")
#library(fPortfolio)


MVO <- function(mylist, ticker, shorts, nports, rf, wmax){
  
  for(i in 1:length(mylist)){
    mylist[[i]] <- mylist[[i]][index(mylist[[1]])]
  }
  
  r<-na.remove(do.call(merge,lapply(lapply(mylist,Ad),monthlyReturn))) #extract adjusted close annual returns
  names(r) <- ticker
  n.assets <- length(mylist)

  #create portfolio specification
  frontierSpec  <- portfolioSpec();
  
  #optimization criteria - MVO
  setType(frontierSpec)  <- "MVO"
  
  #set optimization algorithm
  setSolver(frontierSpec)  <- "solveRquadprog"
  
  #set risk free rate
  setRiskFreeRate(frontierSpec) <- rf
  
  #number of portfolios in efficient frontier
  setNFrontierPoints(frontierSpec)  <- nports
  
  #convert constraints to be handled properly by porfolioFrontier()
  ifelse(shorts, wmin <- -wmax, wmin <- 0)
  constraints <- c(paste("minW[1:",n.assets,"]=",wmin),paste("maxW[1:",n.assets,"]=",wmax))
  
  #optimize, without shortselling
  frontier <- portfolioFrontier(data = as.timeSeries(r), spec = frontierSpec, constraints=constraints);
  
  vol <- getTargetRisk(frontier)[,2]
  ret <- getTargetReturn(frontier)[,2]
  wts <- getWeights(frontier)
  return(list(vol = vol, ret = ret, weights = wts))
  
}