source("CVaR.R")
ticker <- c("XIU.TO","RY.TO","F")
from = as.Date('2013-10-01')
to = Sys.Date()

#download data from yahoo finance
mylist <- lapply(ticker, function(x){
  try(getSymbols(x, src = 'yahoo', from = from, to = to, auto.assign = FALSE))
})

#The below says "I will have an rmin with a probability of 1-alpha"
#wmin/wmax/weight.sum are porfolio constraints. wmin = -1 will allow shorts
#weight.sum affects leverage
folio <- CVaR(mylist = mylist, ticker = ticker, alpha = 0.05, shorts = TRUE, nports = 25, rf = 0.02, wmax = 0.75)
