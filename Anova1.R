library(FactoMineR)
library(perturb)
library(dplyr)
library(lmtest)
library(multcomp)

## Multicollinearity
# we use a data set known for its high multicollinearity
data(longley)

# we create a subset with the regressor variables only
long=as.matrix(longley[,-7])

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

# we use it, remember vif's > 10 indicate high multicollinearity
myvif(long)

## visualizing collinearity directly, package FactoMineR

## which variables have strong linear correlations?
long_pca=PCA(long,scale.unit=TRUE, ncp=6, axes=c(1,2))

# eigenvalues, notice the almost 0 value of the last 3 ones.
long_pca$eig

# eigenvectors associated
vectors=data.frame(long_pca$svd$V)
rownames(vectors)=colnames(long)
vectors

## which variables contribute the most to last dimensions? We can see nothing, we are trying to project 3-dimensional data set on
# a fifth and sixth dimensions
plot(long_pca, shadow=TRUE,choix="var", select="contrib 4" , axes=c(5,6))
rbind(long_pca$var$contrib, TOTAL=colSums(long_pca$var$contrib))
barplot(long_pca$var$contrib, beside=TRUE, legend.text=TRUE,args.legend=list(x="topleft", bty='n', ncol=1))

## using default function for PC analysis
princomp(long, cor=TRUE)$loadings

## OPTIONAL: Another way: Multicollinearity diagnostic with package perturb.
# The variance decomposition proportions associated with each condition index. If a large condition index is associated
# two or more variables #with large variance decomposition proportions, these variables may be causing collinearity problems.
# Belsley et al suggest that a large proportion is 50 percent or more.
colldiag(long, center=T)

### OPTIONAL finishes ##

## Principal Component Regression

## variable selection procedures
## we create a set for the running variables in the model
run=hbat[,6:19]

# We are using function step, by default it compares models based in AIC. We have to define Base model and Full model.
Base1=lm(X19~1, data=run)
Full1=lm(X19~., data=run)
Step.for=step(Base1, scope=list(upper=Full1, lower=~1),direction="forward",trace=F)

# if we remove trace=F we get the sequential process printed out in our screen
summary(Step.for)
Step.for$anova

## backward procedure
Step.back=step(Full1,direction="backward",trace=F)

## direction both
Step.both=step(Base1, scope=list(upper=Full1, lower=~1),direction="both",trace=F)

## If we want to use BIC, use the option k=log(n), n is the sample size
step(Base1, scope=list(upper=Full1, lower=~1),direction="both",trace=F,k=log(100))

# If we want to expand the search to models including two order interaction terms
Full2=lm(X19~.^2, data=exis)
Step2.for=step(Base, scope=list(upper=Full2, lower=~1),direction="forward",trace=F)
summary(Step2.for)

# package leaps to use Mallow's Cp, it does an exhaustive search
Cp=leaps(x=hbat[,6:18],y=hbat[,19],names=names(hbat)[6:18],method="Cp")
plot(Cp$size,Cp$Cp, ylim=c(2,14), xlim=c(2,14))

# Adding the line Cp=p (x=y) 
abline(0,1)
identify(Cp$size,Cp$Cp)

# We identify the model in row 42, which has variables
Cp$which[42,]

## To try to identify the model with Cp=p
head(cbind(Cp$size,Cp$which,Cp$Cp))
which(Cp$size<=Cp$Cp+0.06 & Cp$size>=Cp$Cp-0.06)
cbind(Cp$size[c(42,117,121)], Cp$Cp[c(42,117,121)])

# package leaps using adjr2 as the criiterion
Adjr2=leaps(x=hbat[,6:18],y=hbat[,19],names=names(hbat)[6:18],method="adjr2")

# the model with maximum adjr2 is
Adjr2$which[which(Adjr2$adjr2==max(Adjr2$adjr2)),] 

# regsubsets function from package leaps, parameter nvmax controls the size of the subsets to consider
subsets=regsubsets(X19~X6+X7+X8+X9+X10+X11+X12+X13+X14+X16+X15+X17+X18, data=hbat,nbest=3)
plot(subsets,scale="adjr2")
plot(subsets,scale="bic")
plot(subsets,scale="Cp")
summary(subsets)
coef(subsets,5)

#### Variable selection procedures finish

#Transformations
powerTransform(hbat[,6:18])
powerTransform(lm.1)

## OPTIONAL
# restricting our attention to that variables in lm.1, looking for the best model including two order 
# interactions between the variables X's
dependent=hbat[,c(6,7,9,11,12,19)]
Base=lm(X19~1, data=dependent)
FullB=lm(X19~.^2, data=dependent)
summary(FullB)
StepB.for=step(Base, scope=list(upper=FullB, lower=~1),direction="forward",trace=F)
summary(StepB.for)
gvlma(StepB.for)

X=hbat[,c(9,6,12,7,11)]

# We have important problems with multicollinearity
myvif(X)
Xs=data.frame(scale(X,scale=F))
myvif(Xs)

# multicollinearity problems dissapear, the model is the same
summary(lm(dependent$X19~Xs$X9+Xs$X6+Xs$X12+Xs$X7+Xs$X11+Xs$X12:Xs$X11+Xs$X6:Xs$X12+Xs$X7:Xs$X12))
plot(lm(dependent$X19~Xs$X9+Xs$X6+Xs$X12+Xs$X7+Xs$X11+Xs$X12:Xs$X11+Xs$X6:Xs$X12+Xs$X7:Xs$X12))
gvlma(lm((dependent$X19)~Xs$X9+Xs$X6+Xs$X12+Xs$X7+Xs$X11+Xs$X12:Xs$X11+Xs$X6:Xs$X12+Xs$X7:Xs$X12))
## OPTIONAL finishes