require(tseries)
require(quantmod)
require(corpcor)
require(Matrix)

#if you don't have one of the above packages, use install.packages("packagename")
MVO <- function(ticker, mylist, wmax, nports, shorts, rf){
  
  for(i in 1:length(mylist)){
    mylist[[i]] <- mylist[[i]][index(mylist[[1]])]
  }
  
  r<-na.remove(do.call(merge,lapply(lapply(mylist,Ad),annualReturn))) #extract adjusted close daily returns

  averet = vector()
  #calculate geometric mean and store in averet
  for (i in 1:length(ticker)){
    averet[i] <- prod((1+r[1:length(r[,1]),i]))^(1/length(r[,1]))-1
  }
  
  averet <- matrix(averet, nrow=1)
  rcov <- as.matrix(nearPD(cov(r))$mat)
  
  mxret = max(abs(averet))
  mnret = -mxret
  n.assets = ncol(averet)
  reshigh = rep(wmax,n.assets)
  if( shorts )
  {
    reslow = rep(-wmax,n.assets)
  } else {
    reslow = rep(0,n.assets)
  }
  min.rets = seq(mnret, mxret, len = nports)
  vol = rep(NA, nports)
  ret = rep(NA, nports)
  wts = list()
  for (k in 1:nports)
  {
    port.sol = NULL
    try(port.sol <- portfolio.optim(x=averet, pm=min.rets[k], covmat=rcov,
                                    reshigh=reshigh, reslow=reslow,shorts=shorts,riskless = TRUE, rf = rf),silent=FALSE)
    if ( !is.null(port.sol) )
    {
      vol[k] = sqrt(as.vector(port.sol$pw %*% rcov %*% port.sol$pw)) #caclulate volatility
      ret[k] = averet %*% port.sol$pw #calculate returns
      names(port.sol$pw)<-ticker
      port.sol$pw <- round(100*port.sol$pw)/100
      wts[[k]] = port.sol$pw #store weights
    }
  }
  
  return(list(vol = vol, ret = ret, weights = wts))

}