plot_I0serie <- function(x,t){
  listoutput <- tseries::adf.test(x)
  c=0
  p.val <- listoutput[['p.value']]
  while (p.val > 0.05 ) {
    c=c+1
    x <- diff(x)
    listoutput <- tseries::adf.test(x)
    p.val <- listoutput[['p.value']]
    if (p.val < 0.05){
      x <- zoo(diff(x, k=c),order.by = t)
      par(mfcol=c(1,1))
      return(plot(x,type = 'l', col='red',xlab = 'Years'))
    }
  }
}
plot_I0serie(x=DATA$GDP_PERCAPITA,t=DATA$DATE)
plot_I0serie(x=DATA$INFLATION,t=DATA$DATE)

I0_serie <- function(x,t){
  listoutput <- tseries::adf.test(x)
  c=0
  p.val <- listoutput[['p.value']]
  while (p.val > 0.05 ) {
    c=c+1
    x <- diff(x)
    listoutput <- tseries::adf.test(x)
    p.val <- listoutput[['p.value']]
    if (p.val < 0.05 ){
      x <- zoo(diff(x, k=c),order.by = t)
      return(x)
    }
  }
}
I0_GDP <- I0_serie(x=DATA$GDP_PERCAPITA, t=DATA$DATE)
I0_INFL <- I0_serie(x=DATA$INFLATION, t=DATA$DATE)

