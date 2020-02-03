#random ar,ma,arma integrated of order 1 with a default sample from 100 to 1000 (the unspecified coefficient could be +- 1)
randar11 <- function(x=c(1,1,0), t=sample(100:1000,1), phi=runif(1, min = -1,max=1)){
  arima.sim(list(order=x,ar=phi),n=t)
}

randma11 <- function(x=c(0,1,1), t=sample(100:1000,1), teta=runif(1, min = -1,max=1)){
  arima.sim(list(order=x,ma=teta),n=t)
}

randarma111 <- function(x=c(1,1,1), t=sample(100:1000,1), phi=runif(1, min = -1,max=1), teta=runif(1, min = -1,max=1)){
  arima.sim(list(order=x,ar=phi,ma=teta),n=t)
}

