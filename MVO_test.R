source("MVO.R")
ticker <- c("XIU.TO","RY.TO","F")
from = as.Date('2013-10-01')
to = Sys.Date()

#download data from yahoo finance
mylist <- lapply(ticker, function(x){
  try(getSymbols(x, src = 'yahoo', from = from, to = to, auto.assign = FALSE))
})

folio <- MVO(ticker = ticker, mylist = mylist, wmax = 1, nports = 20, shorts = TRUE, rf = 0.01)
