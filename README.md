<h1>TS1 Package in R</h1>

<h4>Author: Filippo Menegatti</h4>

____

<h3>Description of the package</h3>

With the TS1 package it is possible to run a preliminar analysis and visualization of your time series data. It is possible to make graphs of the raw data, ACF and PACF - for one or two series - at the same time. Then you can obtain the order of integration, differentiate the series or plot directly the I(0) series. Finally, the package contains a default dataset of the US GDP per capita and the US inflation between 1948 and 2019.

____

<h3>Functions description</h3>

```{r}
loaddata()
```
This particular function is used to print the dataframe containing GDP and Inflation data from our default dataset with the column DATE converted in class Date.

____

```{r}
MAINPLOTS()
```
With this function you can return the dataset and visualize the main plots of the package with the data given by default.

____

```{r}
ggdoubleplt()
```
This function can be used to visualize the plot of the two series using the package `ggplot2`. It also combines the graphs putting them in a single window with `gridExtra`.

____

```{r}
ggsumplts()
```
With this function it is possible to visualize the raw data, acf, pacf of a time series with `ggplot2`, `gridExtra` and `ggfortify`.

____

```{r}
ggdoublesumplts()
```
This function can be used like the previous one, but permits to visualize the two series in the same window to easily compare them.

____

```{r}
intorder()
```
This functios can be used to return the order of integration of the two time series with the `adf.test()` from `tseries` package.

____

```{r}
I0_series()
```
This function is used to calculate the order of integration of a series with the `adf.test()` from `tseries` package and then differentiate it to obtain the I(0) series.

____

```{r}
plot_I0()
```
This function return the plot of the differentiated series.

____

<h4>Packages needed</h4>

>    ggplot2, zoo, tseries, gridExtra, ggfortify
