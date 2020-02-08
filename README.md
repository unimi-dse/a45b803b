<h1>TS1 Package in R</h1>

<h4>Author: Filippo Menegatti</h4>

<h3>Description of the package</h3>

With the TS1 package it is possible to run a preliminar analysis and visualization of your time series data. It is possible to make graphs of the raw data, ACF and PACF - for one or two series - at the same time. Then you can obtain the order of integration, differentiate the series or plot directly the I(0) series. In addiction with this package it is possible to simulate three kinds of ARMA processes integrated of order 1. Finally, the package contains a default dataset of the US GDP per capita and the US inflation between 1948 and 2019.

<h3>Functions description</h3>

```{r}
loaddata()
```
This particular function is used to load and save the dataframe containing GDP and Inflation data from our default dataset with the column DATE converted in class Date.

```{r}
MAINPLOTS()
```
With this function you can upload the dataset and visualize all the main plots of the package.

```{r}
ggdoubleplt()
```
This function can be used to visualize the plot of the two series using the package `ggplot2`. It also combines the graphs putting them in a single window with `gridExtra`.

```{r}
ggsumpltsGDP()
ggsumpltsINFL()
```
With these two default functions is possible to visualize the raw data, acf, pacf of the two time series in the default dataset with `ggplot2`, `gridExtra` and `ggfortify`.

```{r}
sumplts()
```
<p>This function is used to display three different graphs in one single window: raw data, ACF and PACF. <br>

***IMPORTANT*** To use it without errors is important to enlarge the plot window by clicking the button above the 'Refresh topic' arrow.</p>

```{r}
doublesumplts()
```
<p>This function is used like the previous one, but permits to visualize the two series in the same window to easily compare them.<br>
  
***IMPORTANT*** To use it without errors is important to enlarge the plot window by clicking the button above the 'Refresh topic' arrow.</p>

```{r}
intorder()
intorderGDP()
intorderINFL()
```
These functions can be used to return the order of integration of the two time series with the `adf.test()` from `tseries` package.

```{r}
I0_series()
I0_seriesGDP()
I0_seriesINFL()
```
These functions are used to calculate the order of integration with the `adf.test()` from `tseries` package and then differentiate to obtain the I(0) series.

```{r}
plot_I0()
plot_I0GDP()
plot_I0INFL()
```
These functions return the plot of the differentiated series.

```{r}
randar11()
randma11()
randarma111()
```
These three function are used to obtain respectively AR(1), MA(1), ARMA(1) integrated of order 1 series randomly generated with a set which goes from 100 to 1000 observations (also the coefficient/s is/are random).

<h4>Packages needed</h4>
ggplot2, zoo, tseries, gridExtra, ggfortify
