loaddata <- function(){
  DATA <- read.csv('https://raw.githubusercontent.com/unimi-dse/a45b803b/master/DATA.csv')
}
DATA$DATE <- as.Date.factor(DATA$DATE)

GDP <- zoo(DATA$GDP_PERCAPITA, order.by = DATA$DATE)
INFL <- zoo(DATA$INFLATION, order.by = DATA$DATE)

sumplts <- function(x){
  par(mfcol=c(3,1))
  plot(x, xlab = 'Years', col = 'red', type = 'l')
  acf(coredata(x), type=c('correlation'), main='ACF', ylab='value')
  acf(coredata(x), type=c('partial'), main='PACF', ylab='value')
}
sumplts(x=GDP)
sumplts(x=INFL)

doublesumplts <- function(x,y){
  par(mfcol=c(3,2))
  plot.zoo(x, xlab = 'Years', col = 'red', type = 'l')
  acf(coredata(x), type=c('correlation'), main='ACF', ylab='value')
  acf(coredata(x), type=c('partial'), main='PACF', ylab='value')
  plot(y, xlab = 'Years', col = 'blue',type='l')
  acf(coredata(y), type=c('correlation'), main='ACF', ylab='value')
  acf(coredata(y), type=c('partial'), main='PACF', ylab='value')
}
doublesumplts(x=GDP,y=INFL)

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
