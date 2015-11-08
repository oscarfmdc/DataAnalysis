library(TSA)
library(forecast)

## package TSA, forecast
# time series bubbly, ggb, a.ts, co2, electricity, lynx, NAO.ts
bubbly=ts(tsdata$bubbly,start=c(1999,1), end=c(2005,12),frequency=12)
str(bubbly)
ggb=ts(tsdata$ggb,start=c(1995,1),frequency=12)
str(ggb)
data(AirPassengers) 
str(AirPassengers)
a.ts=AirPassengers
plot(a.ts)
plot(ggb)
plot(bubbly)
data(co2)
str(co2)
plot(co2)
data

## package forecast, fancy representation
tsdisplay(ggb, plot.type="partial")
tsdisplay(ggb, plot.type="scatter")
tsdisplay(ggb, plot.type="spectrum")

### Box Cox transformation, two datasets where logarithmic transformation is useful: a.ts and electricity
BoxCox.lambda(a.ts,lower=0, upper=2)

## lambda=0 is a logarithmic transformation
tsdisplay(a.ts, plot.type="scatter")
tsdisplay(log(a.ts), plot.type="scatter")

## with electricity
data(electricity)
str(electricity)
BoxCox.lambda(electricity,lower=0, upper=2) #package forecast
tsdisplay(electricity, plot.type="scatter")
tsdisplay(log(electricity), plot.type="scatter")
la.ts=log(a.ts)
lelectr=log(electricity)

#### Exponential Smoothing
## Simple exponential smoothing is used with series without trend and seasonality
## declare data set NAO as a ts
NAO.ts=ts(data=NAO$NAO, start=min(NAO$year), end=max(NAO$year))
alisim1=HoltWinters(NAO.ts,gamma=FALSE,beta=FALSE)
alisim1
forecast=predict(alisim1, n.ahead=12, prediction.interval=TRUE, level=0.95)
plot(alisim1, forecast)

## Holt smoothing is used with series withd trend and no seasonal component
alisim2=HoltWinters(NAO.ts,gamma=FALSE)
alisim2
forecast2=predict(alisim2, n.ahead=12, prediction.interval=TRUE, level=0.95)
plot(alisim2, forecast2)

## Holt-Winters smoothing with series with linear trend and seasonal variation
alisim3=HoltWinters(a.ts)
forecast3=predict(alisim3, n.ahead=6, prediction.interval=TRUE, level=0.95)
plot(alisim3, forecast3)

## Holt-Winters with multipicative structure: if the variability of each cycle increases with time
alisim3prim=HoltWinters(a.ts, seasonal="mult")
forecast3prim=predict(alisim3prim, n.ahead=6, prediction.interval=TRUE, level=0.95)
plot(alisim3prim,forecast3prim)

#### Decomposition of time series, signals extraction, additive decomposition
decomp1=decompose(a.ts)
plot(decomp1)

### Decomposition of time series, multiplicative decomposition
decomp1prim=decompose(a.ts,type="multiplicative")
plot(decomp1prim)

## The random part of the decomposition should show no pattern, should look like a white noise. 
## Let's see what a white noise looks like, doing some simulation
whiteuniform=runif(100) # uniform white noise
whitegaussian=rnorm(100) #gaussian white noise
tsdisplay(whiteuniform, plot.type="scatter")
tsdisplay(whitegaussian, plot.type="scatter")

## An example where decomposition works well
decomp2=decompose(co2)
str(decomp2)
plot(decomp2) 

## Method stl() and forecast
stl1=stl(co2, s.window="periodic", robust=TRUE)
stl2=stl(la.ts, s.window="periodic", robust=TRUE)
plot(stl1)
plot(stl2)

## In method for forecasting, we can choose c("naive", "ets", "arima", "rwdrift"). 
# This is a model for the random ## part: ets=exponential smoothing, rwdrift=random walk 
# with drift. In this particular example the results are ## almost the same.
fcst=forecast(stl1, method="naive")
plot(fcst)
fcst1=forecast(stl1, method="arima")
plot(fcst1)
fcst2=forecast(stl1, method="rwdrift")
plot(fcst2)
fcst2$mean
fcst3=forecast(stl2, method="arima")

## in the logarithmic scale
plot(fcst3)

## in the original scale
fcst4=stlf(a.ts,method="arima",lambda=BoxCox.lambda(a.ts))
plot(fcst4)

## If I want to extract the remainder part for, say stl1:
stl1_remainder=stl1$time.series[,3]
plot(stl1_remainder)
acf2(stl1_remainder)

### Exploring seasonality (declared frequency must be >1), seasonplot in package forecast
monthplot(a.ts)
seasonplot(a.ts)
monthplot(bubbly)
monthplot(ggb)

# Spectrum of the series, package astsa: it shows the lowest frequency having the highest 
# contribution to the variation of the process.
# It may help us in determining the period of the series, if any.
mvspec(a.ts)

# it shows clearly the maximum at 1, as we have declared the frequency=12, the period is 1/1=1 year.
## other data set, annual data
data(lynx)
mvspec(lynx)
 
## the maximum is at 0.1, the period is 1/0.1=10 years (frequency=1 declared in the command ts)

## Exploring the autocorrelation structure of time series
## clear nonstationarity probably due to seasonality (among other causes)
par(mfrow=c(3,1))
plot(log(lynx))
acf(log(lynx))
pacf(log(lynx))

## ggb stochastic trend+seasonality
plot(ggb)
acf(ggb)
pacf(ggb)

## Differenciation to achieve stationarity: lynx (log, d=1, D=1), ggb (no need for log, d=1, D=1)
## We plot them, log(lynx)
par(mfrow=c(3,1))
plot(log(lynx))
plot(diff(log(lynx),10))
plot(diff(diff(log(lynx),10)))

## ggb
plot(ggb)
plot(diff(ggb,12))
plot(diff(diff(ggb,12)))

# exploring ggb
tsdisplay(ggb, plot.type="scatter")
tsdisplay(diff(ggb), plot.type="scatter")
tsdisplay(diff(diff(ggb),12), plot.type="scatter")

# bubbly
par(mfrow=c(2,1))
plot(bubbly)
plot(diff(bubbly,12))

## Stationary tests
## Augmented Dickey-Fuller test (package tseries) is applied on the seasonal adjusted series to
# see if regular differenciation is needed to achieve stationarity.

## log(lynx)
adf.test(diff(log(lynx),10))
adf.test(diff(diff(log(lynx),10)))

## ggb
adf.test(diff(ggb,12))
adf.test(diff(diff(ggb,12)))

## bubbly, D=1
adf.test(diff(bubbly,12))