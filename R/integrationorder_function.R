intorder <- function(x){
  listoutput <- adf.test(x)
  c=0
  p.val <- listoutput[['p.value']]
  while (p.val > 0.05 ) {
    c=c+1
    x <- diff(x)
    listoutput <- adf.test(x)
    p.val <- listoutput[['p.value']]
    if (p.val < 0.05){
      return(c) #order of integration
    }
  }
}
intorder(x=DATA$GDP_PERCAPITA)
intorder(x=DATA$INFLATION)
