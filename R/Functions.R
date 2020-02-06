#' Load the dataset
#'
#' @description The function is used to download a Data Frame from the file 'DATA.csv' which contains two series of data downloaded by FRED. In particular they are the quarterly observations of US seasonally adjusted GDP per capita and Inflation, observed from 1947 to 2019.
#'
#' @return The function returns the Data Frame of the file 'DATA.csv'
#'
#' @export
#'
#' @examples DATA <- loaddata()
#'
loaddata <- function(){
  DATA <- read.csv(system.file("extdata", "DATA.csv", package = "TS1"))
  DATA$DATE <- as.Date.factor(DATA$DATE)
  return(DATA)
}


#' GDP's summary plots
#'
#' @description This function is useful to visualize a summary - raw data, ACF and PACF - of your data in one window. WARNING: to visualize it click the icon above the 'refresh topic' one.
#' @param x The numeric vector of the observations you want to visualize.
#' @param t The vector of the dates used to index the data.
#'
#' @return The function returns a series of three plots: The plot of the raw data, the ACF plot and the PACF plot. As default it returns the plot of US GDP data.
#' @export
#'
#' @examples sumPLOTS <- sumpltsGDP(x=data, t=time)
sumpltsGDP <- function(x=DATA$GDP_PERCAPITA, t=DATA$DATE){
  par(mfcol=c(3,1))
  plot(t, x, main = 'US GDP per capita', xlab = 'Years', col = 'red', type = 'l')
  acf(x, type=c('correlation'), main='ACF', ylab='value')
  acf(x, type=c('partial'), main='PACF', ylab='value')
}

#' Inflation's summary plots
#'
#' @description This function is useful to visualize a summary - raw data, ACF and PACF - of your data in one window. WARNING: to visualize it click the icon above the 'refresh topic' one.
#' @param x The numeric vector of the observations you want to visualize.
#' @param t The vector of the dates used to index the data.
#'
#' @return The function returns a series of three plots: The plot of the raw data, the ACF plot and the PACF plot. As default it returns the plot of US inflation data.
#' @export
#'
#' @examples sumPLOTS <- sumpltsINFL(x=data, t=time)
sumpltsINFL <- function(x=DATA$INFLATION, t=DATA$DATE){
  par(mfcol=c(3,1))
  plot(t, x, main = 'US Inflation', xlab = 'Years', col = 'green', type = 'l')
  acf(x, type=c('correlation'), main='ACF', ylab='value')
  acf(x, type=c('partial'), main='PACF', ylab='value')
}


#' Double column summary plots
#'
#' @description This function is useful to visualize a summary - raw data, ACF and PACF - of your two series in one window. WARNING: to visualize it click the icon above the 'refresh topic' one.
#' @param x The numeric vector of the observations you want to visualize.
#' @param z The numeric vector of the other series you want to visualize.
#' @param t The vector of the dates used to index the data.
#'
#' @return The function works creating two columns of graphs by which you can compare two series.
#' @export
#'
#' @examples twoseriesplot <- doublesumplts(x=GDP, z=INFL, t=time)
doublesumplts <- function(x=DATA$GDP_PERCAPITA, z=DATA$INFLATION, t=DATA$DATE){
  par(mfcol=c(3,2))
  plot(t, x, main = 'GDPpercapita',ylab = 'level', xlab = 'Years', col = 'red', type = 'l')
  acf(x, type=c('correlation'), main='ACF', ylab='value')
  acf(x, type=c('partial'), main='PACF', ylab='value')
  plot(t, z,main = 'Inflation', ylab = '%change', xlab = 'Years', col = 'blue',type='l')
  acf(z, type=c('correlation'), main='ACF', ylab='value')
  acf(z, type=c('partial'), main='PACF', ylab='value')
}

#' ggplot2 data visualization
#'
#' @description This is a default function that is useful to visualize a better graph of the raw data using the package ggplot2.
#' @param DATA The default parameter in this function is the Data Frame containing US GDP per capita and US Inflation.
#'
#' @return This function gives as output a single window with the plot of the two series of the df, also adding them a tendency line.
#' @export
#' @import ggplot2 gridExtra
#'
#' @examples seriesplot <- ggdoubleplot(DATA)
ggdoubleplt <- function(DATA){
  DATA <- read.csv(system.file("extdata", "DATA.csv", package = "TS1"))
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

#' Differentiate time series - GDP
#'
#' @description With this function is possible to differentiate a series to obtain the I(0) version. By default uses the GDP data from the default dataframe of the package 'DATA.CSV'.
#' @param x The numeric vector of the observations you want to visualize.
#' @param t The vector of the dates used to index the data.
#'
#' @return This function calculates the order of integration of your time series and gives you as output the differentiated one as a zoo object. By default this function uses the GDP data from 'DATA.csv'.
#' @export
#' @import tseries zoo
#' @examples I0GDP <- I0_seriesGDP(x=GDP, t=time)
I0_seriesGDP <- function(x=DATA$GDP_PERCAPITA,t=DATA$DATE){
  listoutput <- adf.test(x)
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

#' Differentiate time series - INFLATION
#'
#' @description With this function is possible to differentiate a series to obtain the I(0) version. By default uses the Inflation data from the default dataframe of the package 'DATA.CSV'.
#' @param x The numeric vector of the observations you want to visualize.
#' @param t The vector of the dates used to index the data.
#'
#' @return This function calculates the order of integration of your time series and gives you as output the differentiated one as a zoo object. By default this function uses the Inflation data from 'DATA.csv'.
#' @export
#' @import tseries zoo
#' @examples I0INFL <- I0_seriesINFL(x=infl, t=time)
I0_seriesINFL <- function(x=DATA$INFLATION,t=DATA$DATE){
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

#' Plot I(0) series - GDP
#'
#' @description With this function is possible to visualize the plot of a differentiate I(0) series. By default uses the GDP data from the default dataframe of the package 'DATA.CSV'.
#' @param x The numeric vector of the observations you want to visualize.
#' @param t The vector of the dates used to index the data.
#'
#' @return This function gives as output the plot of the differentiated I(0) series. By default this function uses the GDP data from 'DATA.csv'.
#' @export
#' @import tseries zoo
#'
#' @examples pI0GDP <- plot_I0GDP(x=GDP, t=time)
plot_I0GDP <- function(x=DATA$GDP_PERCAPITA,t=DATA$DATE){
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
      return(plot(x, main='I(0) GDP per capita', type = 'l', col='red',xlab = 'Years'))
    }
  }
}

#' Plot I(0) series - Inflation
#'
#' @description With this function is possible to visualize the plot of a differentiate I(0) series. By default uses the Inflation data from the default dataframe of the package 'DATA.CSV'.
#' @param x The numeric vector of the observations you want to visualize.
#' @param t The vector of the dates used to index the data.
#'
#' @return This function gives as output the plot of the differentiated I(0) series. By default this function uses the Inflation data from 'DATA.csv'.
#' @export
#' @import tseries zoo
#'
#' @examples pI0INFL <- plot_I0INFL(x=inflation, t=time)
plot_I0INFL <- function(x=DATA$INFLATION,t=DATA$DATE){
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
      return(plot(x, main='I(0) inflation', type = 'l', col='red',xlab = 'Years'))
    }
  }
}

#' Integration order - GDP
#'
#' @description This function is used to calculate the order of integration of the series. By default uses the GDP data from the default dataframe of the package 'DATA.CSV'.
#' @param x The numeric vector of the observations you want to visualize.
#'
#' @return This function returns the integration order of the series. By default this function uses the GDP data from 'DATA.csv'.
#' @export
#' @import tseries zoo
#'
#' @examples IO_GDP <- intorderGDP(x=GDP)
intorderGDP <- function(x=DATA$GDP_PERCAPITA){
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

#' Integration order - Inflation
#'
#' @description This function is used to calculate the order of integration of the series. By default uses the Inflation data from the default dataframe of the package 'DATA.CSV'.
#' @param x The numeric vector of the observations you want to visualize.
#'
#' @return This function returns the integration order of the series. By default this function uses the Inflation data from 'DATA.csv'.
#' @export
#' @import tseries zoo
#'
#' @examples IO_INFL <- intorderINFL(x=inlflation)
intorderINFL <- function(x=DATA$INFLATION){
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

#' Random ARIMA(1,1,0)
#'
#' @description With this function - by default - it is possible to create a random AR(1) with a random set between 100 and 1000 obs, and a random coefficient.
#' @param x The vector which indicates the kind of ARMA model.
#' @param t The sample of the AR.
#' @param phi The coefficient of the AR.
#'
#' @return By default this function returns a random sample between 100 and 1000 observations of an ARMA(1,1,0) with a random coefficient between -1 and 1.
#' @export
#'
#' @examples AR1 <- randar11(x=c(1,1,0), t=500, phi=-0.75)
randar11 <- function(x=c(1,1,0), t=sample(100:1000,1), phi=runif(1, min = -1,max=1)){
  arima.sim(list(order=x,ar=phi),n=t)
}

#' Random ARIMA(0,1,1)
#'
#' @description With this function - by default - it is possible to create a random MA(1) with a random set between 100 and 1000 obs, and a random coefficient.
#' @param x The vector which indicates the kind of ARMA model.
#' @param t The sample of the MA.
#' @param teta The coefficient of the MA.
#'
#' @return By default this function returns a random sample between 100 and 1000 observations of an ARMA(0,1,1) with a random coefficient between -1 and 1.
#' @export
#'
#' @examples MA1 <- randar11(x=c(0,1,1), t=500, teta=-0.75)
randma11 <- function(x=c(0,1,1), t=sample(100:1000,1), teta=runif(1, min = -1,max=1)){
  arima.sim(list(order=x,ma=teta),n=t)
}

#' Random ARIMA(1,1,1)
#'
#' @description With this function - by default - it is possible to create a random ARMA(1,1) with a random set between 100 and 1000 obs, and random coefficients.
#' @param x The vector which indicates the kind of ARMA model.
#' @param t The sample of the ARMA.
#' @param phi The coefficient of the AR.
#' @param teta The coefficient of the MA.
#'
#' @return By default this function returns a random sample between 100 and 1000 observations of an ARMA(1,1,1) with random coefficients between -1 and 1.
#' @export
#'
#' @examples ARMA11 <- randar11(x=c(0,1,1), t=500, phi=0.5, teta=-0.75)
randarma111 <- function(x=c(1,1,1), t=sample(100:1000,1), phi=runif(1, min = -1,max=1), teta=runif(1, min = -1,max=1)){
  arima.sim(list(order=x,ar=phi,ma=teta),n=t)
}
