#load library fPortfolio
require(fPortfolio)
install.packages("fPortfolio")
#library(fPortfolio)


MAD <- function(mylist, ticker){
#use indicies LPP2005, see http://www.pictet.com/en/home/lpp_indices.html
  for(i in 1:length(mylist)){
    mylist[[i]] <- mylist[[i]][index(mylist[[1]])]
  }
  
  r<-na.remove(do.call(merge,lapply(lapply(mylist,Ad),monthlyReturn))) #extract adjusted close annual returns
  names(r) <- ticker

  #create portfolio specification
  frontierSpec  <- portfolioSpec();
  
  #optimization criteria - CVaR
  setType(frontierSpec)  <- "MAD"
  
  #set optimization algorithm
  setSolver(frontierSpec)  <- "solveRglpk.MAD"
  
  #set confidence level CVaR
  #setAlpha(frontierSpec)  <- 0.05
  
  #number of portfolios in efficient frontier
  setNFrontierPoints(frontierSpec)  <- 25
  
  #optimize, without shortselling
  frontier <- portfolioFrontier(data = as.timeSeries(r), spec = frontierSpec, constraints="LongOnly");
  
  vol <- getTargetRisk(frontier)[,2]
  ret <- getTargetReturn(frontier)[,2]
  wts <- getWeights(frontier)
  return(list(vol = vol, ret = ret, weights = wts))
  
  #build efficient frontier graph
  #tailoredFrontierPlot(object=frontier,mText="MAD Frontier (Long only)",risk="CVaR");
  #weightedReturnsPlot(frontier)
  
}