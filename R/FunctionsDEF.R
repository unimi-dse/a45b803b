#' Load the dataset
#'
#' @description The function is used to print a Data Frame from the file 'DATA.csv' which contains two series of observations downloaded on FRED. In particular they are the quarterly observations of US seasonally adjusted GDP per capita and Inflation, observed from 1947 to 2019.
#'
#' @return The function assigns the Data Frame of the file 'DATA.csv'
#'
#' @export
#'
loaddata <- function(){
  df <- read.csv(system.file("extdata", "DATA.csv", package = "TS1"))
  df$DATE <- as.Date.factor(df$DATE)
  print(df)
}

#' Return the main plots of the package.
#'
#' @description This function can be used to print the dataset and visualize the main plots of the package.
#' @return The function returns the main plots of TS1.
#' @export
#'
MAINPLOTS <- function(){
  ggdoublesumplts()
  ggsumplts(main = 'GDP per capita')
  ggsumplts(r=DATA,s=DATA$INFLATION,t=DATA$DATE, main='Inflation')
  plot_I0()
  plot_I0(x=DATA$INFLATION, t=DATA$DATE, title = 'Diff inflation')
}

#' Plot raw data + tendency line
#'
#' @description This default function is used to visualize the two time series of the 'DATA' dataframe
#' @return This function returns the plots of the two time series with a tendency line.
#'
#' @export
#'
ggdoubleplt <- function () {
  p1 <- ggplot2::ggplot(DATA, ggplot2::aes(x = as.Date.factor(DATA$DATE),y = DATA$GDP_PERCAPITA)) +
    ggplot2::geom_line(color = "red",size = 0.75) +
    ggplot2::scale_x_date(date_breaks = "8 years",date_labels = "%Y") +
    ggplot2::geom_smooth(size = 0.5,method = "loess", formula = "y ~ x") + ggplot2::labs(title = "USA GDP PER CAPITA", x = "Years", y = "Level")
  p2 <- ggplot2::ggplot(DATA, ggplot2::aes(x = as.Date.factor(DATA$DATE), y = DATA$INFLATION)) +
    ggplot2::geom_line(color = "darkgreen",size = 0.75) +
    ggplot2::scale_x_date(date_breaks = "8 years", date_labels = "%Y") +
    ggplot2::geom_smooth(size = 0.5, method = "loess", formula = "y ~ x", color = "purple") +
    ggplot2::labs(title = "USA INFLATION", x = "Years", y = "% change from y ago")
  gridExtra::grid.arrange(p1, p2, ncol = 1, nrow = 2)
}

#' Summary plot
#'
#' @description With this function you can visualize the raw data, the ACF and the PACF. By default the function uses the GDP data of the package.
#' @param r The dataframe used.
#' @param s The series you want to visualize in x-axe
#' @param t The dates you want to use to index your series.
#' @param main The title you want to give to your plot.
#'
#' @return This gunction returns the plot of the raw data, the ACF and the PACF of the desired data. By default it returns the GDP per capita of the default dataset of the package.
#' @export
#' @import ggfortify
#' @examples ## You can try this    pltsINFL <- ggsumplts(r=DATA, s=DATA$INFLATION, t=DATA$DATE, main='INFLATION')
#'
ggsumplts <- function(r=DATA,s=DATA$GDP_PERCAPITA,t=DATA$DATE, main='GDP per capita'){
  p1 <- ggplot2::ggplot(r, ggplot2::aes(x=as.Date.factor(t), y=s))+
    ggplot2::geom_line(color='red', size=0.75) +
    ggplot2::scale_x_date(date_breaks = "8 years", date_labels = "%Y")+
    ggplot2::labs(title = main, x='Years', y='Level')
  acf1 <- ggplot2::autoplot(acf(s, plot = FALSE))
  pacf1 <- ggplot2::autoplot(pacf(s, plot = FALSE))
  gridExtra::grid.arrange(p1, acf1, pacf1 , ncol=1,nrow=3)
}

#' Double summary plots
#'
#' @description This function works like 'ggsumplts' but permits to visualize two time series at the same time. By default it uses the two time series of the standard dataset
#' @param r The dataframe used.
#' @param s The series you want to visualize in x-axe
#' @param t1 The dates you want to use to index your series.
#' @param main1 The title you want to give to your plot.
#' @param u The other dataframe used.
#' @param v The other series you want to visualize in x-axe
#' @param t2 The other dates you want to use to index your series.
#' @param main2 The other title you want to give to your plot.
#'
#' @return This function returns the summary plots - raw data, ACF, PCF - of two time series.
#' @export
#' @import ggfortify
#'
ggdoublesumplts <- function(r=DATA,s=DATA$GDP_PERCAPITA,t1=DATA$DATE, main1='Ts1', u=DATA ,v=DATA$INFLATION,t2=DATA$DATE, main2='Ts2'){
  p1 <- ggplot2::ggplot(r, ggplot2::aes(x=as.Date.factor(t1), y=s))+
    ggplot2::geom_line(color='red', size=0.75) +
    ggplot2::scale_x_date(date_breaks = "8 years", date_labels = "%Y")+
    ggplot2::labs(title = main1, x='Years', y='Level')
  acf1 <- ggplot2::autoplot(acf(s, plot = FALSE))
  pacf1 <- ggplot2::autoplot(pacf(s, plot = FALSE))
  p2 <- ggplot2::ggplot(u, ggplot2::aes(x=as.Date.factor(t2), y=v))+
    ggplot2::geom_line(color='blue', size=0.75) +
    ggplot2::scale_x_date(date_breaks = "8 years", date_labels = "%Y")+
    ggplot2::labs(title = main2, x='Years', y='Level')
  acf2 <- ggplot2::autoplot(acf(v, plot = FALSE))
  pacf2 <- ggplot2::autoplot(pacf(v, plot = FALSE))
  gridExtra::grid.arrange(p1,p2,acf1,acf2,pacf1,pacf2, ncol=2,nrow=3)
}

#' Differentiate time series
#'
#' @description With this function is possible to differentiate a series to obtain the I(0) version. By default it uses the GDP observation from the default dataset.
#' @param x The numeric vector of the observations you want to visualize.
#' @param t The vector of the dates used to index the data.
#'
#' @return This function calculates the order of integration of your time series and gives you as output the differentiated one as a zoo object.
#' @export
#'
#' @examples ##You can try this    I0INFL <- I0_series(x=DATA$INFLATION, t=DATA$DATE)
I0_series <- function(x=DATA$GDP_PERCAPITA,t=DATA$DATE){
  listoutput <- tseries::adf.test(x)
  c=0
  p.val <- listoutput[['p.value']]
  while (p.val > 0.05 ) {
    c=c+1
    x <- diff(x)
    listoutput <- tseries::adf.test(x)
    p.val <- listoutput[['p.value']]
    if (p.val < 0.05 ){
      x <- zoo::zoo(diff(x, k=c),order.by = t)
      return(x)
    }
  }
}

#' Plot I(0) series
#'
#' @description With this function is possible to visualize the plot of a differentiate I(0) series. By default it uses the GDP data from the default dataset of the package.
#' @param x The numeric vector of the observations you want to visualize.
#' @param t The vector of the dates used to index the data.
#'
#' @return This function gives as output the plot of the differentiated I(0) series.
#' @export
#'
#' @examples ## Do not run this    pltI0INFL <- plot_I0(x=DATA$INFLATION, t=DATA$DATE, title='Diff inflation')
plot_I0 <- function(x=DATA$GDP_PERCAPITA,t=DATA$DATE, title='Diff GDP per capita'){
  listoutput <- tseries::adf.test(x)
  c=0
  p.val <- listoutput[['p.value']]
  while (p.val > 0.05 ) {
    c=c+1
    x <- diff(x)
    listoutput <- tseries::adf.test(x)
    p.val <- listoutput[['p.value']]
    if (p.val < 0.05){
      x <- zoo::zoo(diff(x, k=c),order.by = t)
      par(mfcol=c(1,1))
      return(zoo::plot.zoo(x, main= title, type = 'l', col='red',xlab = 'Years'))
    }
  }
}

#' Integration order
#'
#' @description This function is used to calculate the order of integration of the series. By default this function uses the US GDP per capita of the default dataset.
#' @param x The numeric vector of the observations you want to visualize.
#'
#' @return This function returns the integration order of the series.
#' @export
#' @examples ##You can try this    IO_INFL <- intorder(x=DATA$INFLATION)
intorder <- function(x=DATA$GDP_PERCAPITA){
  listoutput <- tseries::adf.test(x)
  c=0
  p.val <- listoutput[['p.value']]
  while (p.val > 0.05 ) {
    c=c+1
    x <- diff(x)
    listoutput <- tseries::adf.test(x)
    p.val <- listoutput[['p.value']]
    if (p.val < 0.05){
      return(c)
    }
  }
}
