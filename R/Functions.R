#load the dataset from github
loaddata <- function(){
  DATA <- read.csv(file=system.file("extdata", "DATA.csv", package="TS1"), sep = ',')
  return(DATA)
}

#print 3 graphs of a zoo obj with raw data, acf and pacf
sumplts <- function(x){
  par(mfcol=c(3,1))
  plot(x, xlab = 'Years', col = 'red', type = 'l')
  acf(coredata(x), type=c('correlation'), main='ACF', ylab='value')
  acf(coredata(x), type=c('partial'), main='PACF', ylab='value')
}

#does the same as sumplts, but with two zoo obj
doublesumplts <- function(x,y){
  par(mfcol=c(3,2))
  plot.zoo(x, xlab = 'Years', col = 'red', type = 'l')
  acf(coredata(x), type=c('correlation'), main='ACF', ylab='value')
  acf(coredata(x), type=c('partial'), main='PACF', ylab='value')
  plot(y, xlab = 'Years', col = 'blue',type='l')
  acf(coredata(y), type=c('correlation'), main='ACF', ylab='value')
  acf(coredata(y), type=c('partial'), main='PACF', ylab='value')
}

#print the plots of INFL and GDP from DATA.csv with ggplot
ggdoubleplt <- function(){
  DATA <- read.csv('https://raw.githubusercontent.com/unimi-dse/a45b803b/master/DATA.csv')
  DATA$DATE <- as.Date.factor(DATA$DATE)
  p1 <- ggplot(DATA, aes(x=as.Date.factor(DATA$DATE), y=DATA$GDP_PERCAPITA))+
    geom_line(color='red', size=0.75) +
    scale_x_date(date_breaks = "8 years", date_labels = "%Y")+
    geom_smooth(size=0.5, method = 'loess', formula = 'y ~ x')+
    labs(title = 'USA GDP PER CAPITA', x='Years', y='Level')
  p2 <- ggplot(DATA, aes(x=as.Date.factor(DATA$DATE), y=DATA$INFLATION))+
    geom_line(color='darkgreen', size=0.75) +
    scale_x_date(date_breaks = "8 years", date_labels = "%Y")+
    geom_smooth(size=0.5, method = 'loess', formula = 'y ~ x', color='purple')+
    labs(title = 'USA INFLATION', x='Years', y='% change from y ago')
  grid.arrange(p1,p2,ncol=1,nrow=2)
}

#this function returns the diff I0 series.
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

#this function prints the plot of the diff I0 series starting from a numeric vector.
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

#returns the integration order of your series
intorder <- function(x){
  listoutput <- tseries::adf.test(x)
  c=0
  p.val <- listoutput[['p.value']]
  while (p.val > 0.05 ) {
    c=c+1
    x <- diff(x)
    listoutput <- tseries::adf.test(x)
    p.val <- listoutput[['p.value']]
    if (p.val < 0.05){
      return(c) #order of integration
    }
  }
}

#random ar, ma, arma integrated of order 1 with a default sample from 100 to 1000 (the unspecified coefficient could be +- 1)
randar11 <- function(x=c(1,1,0), t=sample(100:1000,1), phi=runif(1, min = -1,max=1)){
  arima.sim(list(order=x,ar=phi),n=t)
}

randma11 <- function(x=c(0,1,1), t=sample(100:1000,1), teta=runif(1, min = -1,max=1)){
  arima.sim(list(order=x,ma=teta),n=t)
}

randarma111 <- function(x=c(1,1,1), t=sample(100:1000,1), phi=runif(1, min = -1,max=1), teta=runif(1, min = -1,max=1)){
  arima.sim(list(order=x,ar=phi,ma=teta),n=t)
}
