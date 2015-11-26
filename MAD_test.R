source("MAD.R")
ticker <- c("XIU.TO","RY.TO","F","AAPL","GOOGL", "T", "SPY", "BUD", "MMM", "ATVI", "ABT", "ALTR", "CAT", "CMG")
from = as.Date('2013-10-01')
to = Sys.Date()

#download data from yahoo finance
mylist <- lapply(ticker, function(x){
  try(getSymbols(x, src = 'yahoo', from = from, to = to, auto.assign = FALSE))
})

#The below says "I will have an rmin with a probability of 1-alpha"
#wmin/wmax/weight.sum are porfolio constraints. wmin = -1 will allow shorts
#weight.sum affects leverage
MAD(mylist = mylist, ticker = ticker, rf = 0.02, shorts = TRUE, nports = 25, wmax = 0.5)
