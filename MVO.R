require(tseries)
require(quantmod)
require(corpcor)

#if you don't have one of the above packages, use install.packages("packagename")

#read stock symbols
ticker <- read.csv(file="XIU.csv",header=FALSE,sep=",",colClasses = c(V1="character"))$V1

from = as.Date('2013-10-01')
to = Sys.Date()

#download data from yahoo finance
mylist <- lapply(ticker, function(x){
  try(getSymbols(x, src = 'yahoo', from = from, to = to, auto.assign = FALSE))
})

#ensure all stocks are on the same time index
for(i in 1:length(mylist)){
  mylist[[i]] <- mylist[[i]][index(mylist[[1]])]
}

r<-na.remove(do.call(merge,lapply(lapply(mylist,Ad),annualReturn))) #extract adjusted close daily returns


averet = vector()
#calculate geometric mean and store in averet
for (i in 1:length(ticker)){
  averet[i] <- prod((1+r[1:length(r[,1]),i]))^(1/length(r[,1]))-1
}
names(averet)<-ticker
rcov <- cov(r) #calculate covariance matrix
tgtret <- 0.05 #target return

folio <- portfolio.optim(x = r, pm = tgtret, covmat = make.positive.definite(rcov), shorts = FALSE, 
                           reslow = rep(0,length(ticker)), reshigh = rep(0.5,length(ticker)), riskless = TRUE, 
                           rf = 0.01)
names(folio$pw)<-ticker
folio$pw <- round(100*folio$pw)/100
print(folio$pw[folio$pw!=0])

#calculate effecient frontiers
wmax <- 1
nports <- 50
shorts <- TRUE

eff<-effFrontier(matrix(averet,nrow=1),make.positive.definite(rcov),nports,shorts,wmax)
plot(eff$vol,eff$ret,type='l',xlim = as.vector(c(min(eff$vol),max(eff$vol))),ylim = as.vector(c(min(eff$ret),max(eff$ret))))

effFrontier = function (averet, rcov, nports, shorts, wmax)
{
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
  for (k in 1:nports)
  {
    port.sol = NULL
    try(port.sol <- portfolio.optim(x=averet, pm=min.rets[k], covmat=rcov,
                                    reshigh=reshigh, reslow=reslow,shorts=shorts),silent=T)
    if ( !is.null(port.sol) )
    {
      vol[k] = sqrt(as.vector(port.sol$pw %*% rcov %*% port.sol$pw))
      ret[k] = averet %*% port.sol$pw
    }
  }
  return(list(vol = vol, ret = ret))
}