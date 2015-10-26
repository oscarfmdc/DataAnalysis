library(car)
library(lmtest)
library(nortest)
library(tseries)
library(moments)
library(gvlma)
library(perturb)
library(leaps)
library(MASS)

load("hbat.RData")
# remember, X19 is overall customer satisfaction with the company and indpendent variables measure different perceptions on the service
# provided
Full<-lm(X19~X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18, data=hbat)
summary(Full)
attributes(Full)

# We implement a do-it-yourself method for variable selection as a first approximation to see what is going
# on when fitting a multiple linear regression  model
#We are dropping out non significant variables, one by one, as the model changes with every adjustment we make
# We first drop out the one with the highest p-value, X15, and so on
back1=update(Full,.~.-X15, hbat)
summary(back1)
back1=update(back1,.~.-X18, hbat)
summary(back1)
back1=update(back1,.~.-X10, hbat)
summary(back1)
back1=update(back1,.~.-X8, hbat)
summary(back1)
back1=update(back1,.~.-X14, hbat)
summary(back1)
back1=update(back1,.~.-X13, hbat)
summary(back1)
back1=update(back1,.~.-X17, hbat)
summary(back1)
back1=update(back1,.~.-X16, hbat)
summary(back1)
#So, deletion sequence has been X15, X18, X10, X8, X14, X13, X17 and X16, using the function  update()
#Our final model is now stored at object back1, we rename it to lm.1
lm.1=back1

# Assume now that we want to test if parameters \beta_X6 (point estimation = 0.37) and \beta_X9 ( point estimation =0.31) 
# can be assumed to  be equal. We fit a model with this hypothesis:
lm.3=lm(X19~I(X6+X9)+X7+X11+X12, hbat) #restricted model
#and compare both models
anova(lm.3,lm.1)
# p-value allows us to make the simplification, but this is for illustration purposes only, so I continue with model lm.1
summary(lm.1)
#confidence intervals for beta parameters
confint(lm.1)

#Diagnostics plots
par(mfrow=c(3,2))
plot(lm.1, which=c(1:6), ask=F)
par(mfrow=c(1,1))

#More residual plots versus each term and versus fitted values. Also computes a 
#curvature test for each of the plots by adding a quadratic term and testing the 
#quadratic to be zero. This is Tukey's test for nonadditivity when plotting against 
#fitted values. We are interested in high p-values. Package car.
residualPlots(lm.1)
# For instance, if we want to see if there is a quadratic dependence on, say, X9 (added
# to the model)
residualPlots(lm.1, terms=~X9^2)
#it doesn't seem so

#Influential Observations
# built-in function, several measures of influence: cov ratio, cook's distance, hat values
InF=influence.measures(lm.1)
summary(InF)
# and residuals that weren't print out before
# load package "car"
influencePlot(lm.1, labels=NULL,id.method="identify")
# or
influencePlot(lm.1, labels=NULL,id.method="noteworthy")
# From InF we get the numbers of the potential influence observations
obsi=which(apply(InF$is.inf,1,any))
# and we put together all the influence measures we are interested in
cbind(res=rstandard(lm.1)[obsi], InF$infmat[obsi,8:10])

#more Influence plots
infIndexPlot(lm.1)

# Plots available for multiple linear regression (that are not available in simple linear regression)
#Partial residual plots (or added variable plots): one for each independent variable. It provides information
# about the nature of the marginal relationship for each regressor under consideration. They can also be used
# to explore for influence observations (package car) 
avPlots(lm.1)

#Component + residual plots: they are an extension of partial residual plots, and a good way to 
# see if the predictors have a linear #relationship to the dependent variable. They may suggest a 
# transformation of the predictor, typically, sqrt(x), 1/x, log(x).
crPlots(lm.1)

#Residual Analysis
boxplot(lm.1$residuals, outline=T)
# package "nortest" and "tseries"
cvm.test(lm.1$residuals)
jarque.bera.test(lm.1$residuals)
# Lilliefors test
lillie.test(lm.1$residuals)
#kurtosis and skewness for residuals
anscombe.test(lm.1$residuals)
agostino.test(hbat[,6])
#It doesn't seem lack of normality is a problem

#Outlier test (Bonferroni, package car)
#defaullt parameters cutoff=0.05, n.max=10
outlierTest(lm.1, cutoff=0.05)

#Constant/Non constant variance (homocedasticity), package "lmtest"
# Breusch-Pagan test
bptest(lm.1)
# Goldfeld-Quandt Test 
gqtest(lm.1)
#Score test for non-constant error variance (package car). Null hypothesis
#of constant error variance. Alternative: the error variance changes with the fitted values.
ncvTest(lm.1)
ncvTest(lm.1, var.formula=~hbat$X12)
#Spread-level plot: suggests a power transformation if there is a problem with non contst variance. What we want is
#the lowess fit showed to be flat, not sloped. 
slp(lm.1)

#Durbin Watson  test, to test for residuals independence (autocorrelation of residuals):
#package lmtest
dwtest(lm.1, alternative="two.sided")
#Box-Ljung test
Box.test(residuals(lm.1))
# The-values are actually low, we may have a problem with independence of residuals.

#General test for errors specification in the model, here we are interested in high p-values.
# tests if powers of the fitted values should be included in the model (that includes powers of the explanatory variables and interaction #terms)
resettest(lm.1,type="fitted")
## tests if powers of the explanatory variables should be included in the model, low p-values is a "yes" to this question
resettest(lm.1, power=2:3,type="regressor")
## it seems we need to add some powers of explanatory variables, we will see this later on.

# Harvey-Collier test for linearity. Small p-values suggest that the true relationship is not linear but concave or convexe.
harvtest(lm.1)

## Global Diagnostoc measure, package gvlma
gvlma(lm.1)    
 
#it compares the Global Stat value with
qchisq(0.95,4)
# As it is less, although one of the hypothesis is not satisfied, it is globally accepted
plot(gvlma(lm.1))

# We could transform variable X12 to log(X12) and check out the gvlma test
lm.1log12=lm(X19~X6+X9+X7+X11+log(X12), data=hbat)
gvlma(lm.1log12)

##Including polynomial terms
### It can happen when including polynomial terms that we introduce correlations between 
### the variables (that could cause multicollinearity issues.) As a precautionary 
### measure, when including polynomial terms we will prefer to center the data: 
hbats=data.frame(scale(hbat,scale=F)) 
lm.2s=lm(X19~X6+X7+X12+X9+X11+I(X7^2), data=hbats)
lm.1s=lm(X19~X6+X7+X12+X9+X11, data=hbats)
anova(lm.1s,lm.2s)
# we prefer model lm.1s
lm.3s=lm(X19~X6+X7+X12+X9+X11+I(X7^2)+I(X12^2), data=hbats)
anova(lm.1s,lm.3s)
#we prefer model lm.3s, there are no outliers, normality is not rejected
outlierTest(lm.3s)
lillie.test(residuals(lm.3s))
## p-value in Durbin-Watson test is a little bit improved
dwtest(lm.3s,alternative="two.sided")
#some influential observations
influencePlot(lm.3s, id.method="noteworthy")

#Global diagnosis
gvlma(lm.3s)

#predictions 
#first, we create new data frame with three new data points (made up "on the fly")
newlm.1=data.frame(X6=c(5.3,6,9.1),X7=c(3,4,5), X9=c(3,6,7), X11=c(3,4,8), X12=c(3,6,8))

# prediction with model lm.1
# confidence interval of the mean response (robust to non normality of the errors)
predict(lm.1, newlm.1, interval="confidence")
#prediction intervals for new observations (much less robust to non normality of errors)
predict(lm.1, newlm.1, interval="prediction")

#Predictions with model lm.3s
newlm.3s=data.frame(scale(newlm.1,center=c(7.81,3.6,6.918, 5.805, 5.123)))
predict(lm.3s, newlm.3s, interval="prediction")
predict(lm.3s, newlm.3s, interval="confidence")
