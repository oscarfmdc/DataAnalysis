## package TSA, forecast, astsa, tseries
# time series bubbly, lynx and ggb 
### With series lynx----------------------------
## First, log transforming the series, is very IMPORTANT!!
tsdisplay(lynx, plot.type="scatter")
tsdisplay(log(lynx), plot.type="scatter")
#with the standard deviation of the series and its differenciations, we find d=1, D=1
sd(log(lynx))
sd(diff(log(lynx))) #d=1
sd(diff(diff(log(lynx),10))) #D=1
lynd=diff(diff(log(lynx),10))
adf.test(lynd) #it is stationary
acf2(lynd, max.lag=30) #we decide to include only a seasonal structure MA(1)
lynx.1=Arima(log(lynx),order=c(0,1,0),include.mean=1,seasonal=list(order=c(0,1,1), period=10))
tsdiag(lynx.1)
# We decide to include a regular structure
 acf2(lynx.1$residuals)
tsdiag(lynx.1) # we can include an MA(1) or an AR(1) in the regular part. We decide an AR(1)
lynx.2=Arima(log(lynx),order=c(1,1,0),include.mean=1,seasonal=list(order=c(0,1,1), period=10))
acf2(lynx.2$residuals)
 tsdiag(lynx.2)
# We decide to try the MA(1) instead the AR(1)  in the regular part
lynx.3=Arima(log(lynx),order=c(0,1,1),include.mean=1,seasonal=list(order=c(0,1,1), period=10))
# we get some improvement
tsdiag(lynx.3)
acf2(lynx.3$residuals)
## We try a fourth model
lynx.4=Arima(log(lynx),order=c(0,1,2),include.mean=1,seasonal=list(order=c(0,1,1), period=10))
## it is not better
## And a fifth
lynx.5=Arima(log(lynx),order=c(1,1,1),include.mean=1,seasonal=list(order=c(0,1,1), period=10))
## It doesn't improve either. Compare AIC's of these models
AIC(lynx.1)
AIC(lynx.2)
AIC(lynx.3)
AIC(lynx.4)
AIC(lynx.5)
##Using auto arima
auto.arima(log(lynx))
## full form of auto.arima function
## auto.arima(log(lynx), max.d=1, max.D=1, max.p=1,max.q=1,max.P=1, max.Q=1,trace=TRUE, stepwise=TRUE, test="adf", seasonal.test="ocsb")
##We get ARIMA(2,0,1), no seasonal part, AIC=184.55
## But we have already got a better one in terms of AIC, lynx.3
#correlations between model coefficients
cov2cor(lynx.3$var.coef)
#testing independence of the residuals
LB.test(lynx.3)
#or 
LB.test(lynx.3, lag=20)
#normality of the residuals
jarque.bera.test(residuals(lynx.3))

## Forecasting with the model lynx.3
predict(lynx.3,n.ahead=20)
plot.Arima(lynx.3) #TSA package


#----------------------------------------------------------------
#series bubbly
#wether or not to take logarithm is not clear. We choose not to.
sd(bubbly)
sd(diff(bubbly,12))
sd(diff(diff(bubbly,12)))
##sd confirms D=1
acf2(diff(bubbly,12)) #pretty decent
bubbly.1=Arima(bubbly,order=c(0,0,0),include.mean=1,seasonal=list(order=c(0,1,0), period=12))
## good validation results, testing independence of the residuals
LB.test(bubbly.1) ##TSA
Box.test(residuals(bubbly.1),lag=12)
tsdiag(bubbly.1)
acf2(bubbly.1$residuals)
## we try another models
bubbly.2=Arima(bubbly,order=c(1,0,0),include.mean=1,seasonal=list(order=c(0,1,0), period=12))
acf2(bubbly.2$residuals)
AIC(bubbly.1)
AIC(bubbly.2)
## We have improved a bit
bubbly.3=Arima(bubbly,order=c(1,0,1),include.mean=1,seasonal=list(order=c(0,1,0), period=12))
AIC(bubbly.3)
## We have improved a little more
tsdiag(bubbly.3)
## model 4th is worse. We keep model 3
 bubbly.4=Arima(bubbly,order=c(1,0,1),include.mean=1,seasonal=list(order=c(0,1,1), period=12))
AIC(bubbly.4)
tsdiag(bubbly.4)
plot(forecast(bubbly.3,h=12))
## to see the numbers
 forecast(bubbly.3,h=12)
## or
 plot.Arima(bubbly.3)  #predictions are the same
## watch out !! with bubbly.3
cov2cor(bubbly.3$var.coef)
## correlations are near 0.8
##We correct this in the Fifth Model
 bubbly.5=Arima(bubbly,order=c(1,1,1),include.mean=1,seasonal=list(order=c(0,1,0), period=12))
cov2cor(bubbly.5$var.coef)
AIC(bubbly.5)
LB.test(bubbly.5,12)
## even reducing AIC !!
forecast(bubbly.5)
 plot(forecast(bubbly.5))

#-------------------------------------------------------------
# with series ggb
sd(ggb)
sd(diff(ggb,12))
sd(diff(diff(ggb,12)))
sd(diff(diff(ggb,12),2))
## d=1, D=1
acf2(diff(diff(ggb,12)))
## We choose MA(1) for the regular part and MA(1) for the seasonal part. Slowly exponential decreasing pattern in PACF and one spike significant
## in ACF
ggb.1 <- Arima(ggb,order=c(0,1,1),include.mean=1,seasonal=list(order=c(0,1,1), period=12))
tsdiag(ggb.1)
LB.test(ggb.1)
Box.test(residuals(ggb.1),lag=12)
forecast(ggb.1,h=12)
plot(forecast(ggb.1,h=12))
cov2cor(ggb.1$var.coef)

# We try an ARIMA(0,1,1)(2,1,0)_12
ggb.2 <- Arima(ggb,order=c(0,1,1),include.mean=1,seasonal=list(order=c(2,1,0), period=12))	
tsdiag(ggb.2)
LB.test(ggb.2) #TSA package
Box.test(residuals(ggb.2),lag=12)

## We compare AIC for both models
 AIC(ggb.1)
 AIC(ggb.2)
## 

## We try a third model
ggb.3 <- Arima(ggb,order=c(0,1,2),include.mean=1,seasonal=list(order=c(0,1,1), period=12))
tsdiag(ggb.3)
LB.test(ggb.3,12)
Box.test(residuals(ggb.3),lag=12) #the same, in package stats
## Its AIC is better
cov2cor(ggb.3$var.coef)

## A fourth one
myggb=Arima(ggb,order=c(1,1,1),include.mean=1,seasonal=list(order=c(0,1,1), period=12))
tsdiag(myggb)
LB.test(myggb)
AIC(myggb)

forecast(myggb,h=12)
plot(forecast(myggb,h=12))

#------------------------------------------------------------------------------------------------
## Using function sarima from package astsa
myggb.sarima=sarima(ggb,1,1,1,0,1,1,12)
#when you fit the model, diagnostics are plotted
#plotting the residuals
plot(resid(myggb.sarima$fit))
LB.test(resid(myggb.sarima$fit))
#forecasting
sarima.for(ggb,1,1,1,0,1,1,12)
sarima.for(ggb,n.ahead=12,1,1,1,0,1,1,12)
