#We start with an automatic procedure to find a logistic model for X23(=extent to which the respondent preceives his firm would engage in partnership with HBAT 0=not, 1=yes) in terms of variables X6 to X21
log.F=glm(X23~X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+X16+X17+X18+X19+X20+X21,data=hbat,family=binomial)
log.B=glm(X23~1,data=hbat,family=binomial)
Step.for=step(log.B, scope=list(upper=log.F),direction="forward")
#We fit that model log.m
log.m=glm(X23~X19+X20+X21+X9+X15, data=hbat, family=binomial)
summary(log.m)
# To assess the fit of the model we compute a  p-value for the overall fit of the model
pchisq(50.906,94, lower=F)
#We  have no evidence against this model, pvalue >0.05. the model will be rejected for pvalue<0.05
#The next part of the output shows the coefficients, their standard errors, the z-statistic (sometimes called a Wald z-statistic), and #the  #associated p-values. The logistic regression coefficients give the change in the log odds of the outcome for a one unit increase #in the #predictor variable.

## another way to test the overall model, through the likelihood ratio test 
## reduction in deviance 
with(log.m, null.deviance - deviance)
## reduction in degrees of freedom
with(log.m, df.null - df.residual)
## p-value
with(log.m, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
## p-value of less than 0.001 tells us that our model as a whole fits significantly better than an empty model. This is sometimes ## called a 
## likelihood ratio test (the deviance residual is -2*log likelihood). 

#Analysis of deviance table
 anova(log.m, test="Chisq")

#Odds Ratios 
 exp(cbind(OR = coef(log.m), confint(log.m)))

#Hosmer and Lemeshow GoF test, library(ResourceSelection)
hoslem.test(hbat$X23, fitted(log.m))

## Classification table with model log.m
myprob=log.m$fitted
myprob=ifelse(myprob>0.5,1,0)
table(myprob, hbat$X23)
## Our model achieves an 86% of correct classification


## This was an automatic found model, try this one
 log.m2=glm(X23~X19+X20+X21+X9, data=hbat, family=binomial)
## Comparing nested models
anova(log.m2,log.m, test="Chisq")
## p-valor < 0.05 supporting Model 2 (log.m), but perhaps model 1 has a clearer interpretation

### Predicting probabilities with model log.m
newdata=with(hbat, data.frame(X19=mean(X19), X20=mean(X20), X21=c(mean(X21), mean(X21)+1), X9=mean(X9), X15=mean(X15)))
newdata
#predict the probability of a yes for the new data
predict(log.m, newdata = newdata, type = "response")
## when I increase one unit in X21, the probabilty of success increases from 0.28 to 0.64. The odds ratio for X21 #change in 
exp(log.m$coefficients[4])
## when increasing X21 in one unit.

#plotting the probability of would recommend as a function of X21
newdata2=with(hbat, data.frame(X19=mean(X19), X20=mean(X20), X21=seq(from=5.5, to=9.9,length.out=50), X9=mean(X9), X15=mean(X15)))
newdata3 <- cbind(newdata2, prob=predict(log.m, newdata = newdata2, type = "response"))
ggplot(newdata3, aes(x=X21, y=prob))+geom_line()

#plotting the probability of would recommend as a function of X15= extent to which HBAT develops and sells new products
newdata4=with(hbat, data.frame(X19=mean(X19), X20=mean(X20), X21=mean(X21), X9=mean(X9), X15=seq(from=1.7, to=9.5,length.out=50)))
newdata5 <- cbind(newdata4, prob=predict(log.m, newdata = newdata4, type = "response"))
ggplot(newdata5, aes(x=X15, y=prob))+geom_line()


###multicollinearity, import functions r2multv, myvif
# function r2multv
r2multv<-function(x){
r2s=1-1/(diag(solve(cov(x)))*diag(cov(x)))
r2s
}

# function myvif
myvif<-function(x){
 myvif=1/(1-r2multv(x))
 myvif
sort(myvif, decreasing=TRUE)
  }

myvif(cbind(hbat$X19,hbat$X20,hbat$X21,hbat$X9,hbat$X15))
# we don't have multicollinearity issues

## What is the predicted probability of a "yes" in question 23, if a company answered X19=6, X20=5, X21=5.5, X9=3 and X15=3? 

## gam models

#Gam (Generalized additive models with integrated smoothness estimation)
#Paquete mgcv
gam1=gam(X19~X6+X7+X9+X11+s(X12), data=hbat)
summary(gam1)
plot(gam1)
plot(hbat$X19, predict(gam1))
abline(0,1)
# model for some previous session 
lm.1=lm(X19~X6+X7+X9+X11+X12, data=hbat)
dev.new()
plot(hbat$X19, predict(lm.1))
abline(0,1)
anova(lm.1,gam1)
cor(hbat$X19, predict(lm.1))
cor(hbat$X19, predict(gam1))


## Library mclust: Model-Based Clustering
## Clustering univariate data
mclust1=Mclust(hbat$X13)
summary(mclust1,parameters=TRUE)
par(mfrow=c(2,2))
plot(mclust1)
# Clustering bivariate data
mclust2=Mclust(cbind(hbat$X13, hbat$X19))
summary(mclust2)
summary(mclust2, parameters=TRUE)
par(mfrow=c(2,2))
plot(mclust2)
