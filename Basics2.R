# Importing data sets
medifis=read.table("medifis.txt")
colnames(medifis)=c("gr","hg","wg","foot","arm","back","skull","knee")
hbat=read.csv("hbat.csv",header=TRUE,sep=",")
# if we have imported the id column as first column, we delete it
head(hbat)
hbat=hbat[-1]

# Covariance and Correlation matrix
cov(medifis[,-1])
r=cor(medifis[,-1])
r# which variables correlate the most?
diag(r)=0
which(r==max(abs(r)), arr.ind=TRUE)

# Pairwise correlations with p-values (package Rcmdr)
rcorr.adjust(medifis[,-1])

#Partial correlations
partial.cor(medifis[,-1])

#or with package corpcor, function cor2pcor is applied on a correlation matrix:
cor2pcor(cor(medifis[,-1]))
# plotting correlations, we need package ellipse
plotcorr(cor2pcor(cor(medifis[,-1])))

# Package ppcor provides more information in the output
pcor(medifis[,-1])

#Define a function r2multv for squared multiple correlation coeficients (r-squared)

r2multv<-function(x){
r2s=1-1/(diag(solve(cov(x)))*diag(cov(x)))
r2s
}

#use it on data set "measures"
r2multv(medifis)
# What is the variable "more linearly explained" by the others
#use the function on hbat dataset
r2multv(hbat[,6:22])

#In this data set, important linear relationships are present. Let's caclculate the determinant of S and R
det(cor(hbat[,6:22]))
det(cov(hbat[,6:22]))

#Can you find the variables involved in the overall linear dependence?
eigen(cov(hbat[,6:22]))
# However, linear pairwise correlations between those variables are not very strong
cor(hbat[,c(11,17,18)])
#But R^2's are
r2multv(hbat[,c(11,17,18)])
    
# Effective dependence coefficient
1-det(cor(medifis[,-1]))^{1/6}
#Variables from 6 to 19 from hbat
1-det(cor(hbat[,6:19]))^{1/13}
#Variables from 6 to 22 from hbat
1-det(cor(hbat[,6:22]))^{1/16}

# Testing normality assumptions
# We can use Shapiro-Wilk's test for small samples $n<50$. Lilliefors' test (package nortest) for bigger samples.
lillie.test(hbat[,8])
shapiro.test(medifis[,3])
# Any conclusion?
#Other tests include Anderson-Darling test, Cramer Von-Misses and Jarque-Bera (based on a joint statistics of skewness and kurtosis)
ad.test(hbat[,6])
cvm.test(hbat[,8])
jarque.bera.test(hbat[,8])

#### Test for skewness and kurtosis (if normality is failing, for instance)
# package moments
anscombe.test(hbat[,6])  
#kurtosis
agostino.test(hbat[,6])
# Why is failing normality in variable hbat[,6]?


#We can try a transformation on hbat[,6]
lillie.test(hbat[,6]^2)

# Variable transformation	
rdoorcl=read.table("rdoorcl.txt")
rdoorop=read.table("rdoorop.txt")
qqPlot(rdoorcl$V1, dist="norm")
shapiro.test(rdoorcl$V1)
#boxcox function, package MASS
boxcox(rdoorcl$V1~1)
#values of previous function
boxcox(rdoorcl$V1~1,plotit=FALSE)
#finding the maximum of the likelihood function
max1=boxcox(rdoorcl$V1~1,plotit=FALSE)
max1[which.max(max1$y),]
max1=data.frame(max1)

#Another more direct way to find that value along with some statistical tests using function powerTransform (package "car")
powerTransform(rdoorcl$V1)
# and more information
summary(powerTransform(rdoorcl$V1))
#We make a variable transformation using lambda=0.27
rdoorclt=bcPower(rdoorcl, lambda=0.27)
#and check if it improves normality
shapiro.test(rdoorclt$V1)
#Comparing both qqplots
par(mfrow=c(1,2))
qqPlot(rdoorcl$V1, dist="norm")
qqPlot(rdoorclt$V1, dist="norm")
#Cheking improvement of normality
lillie.test(rdoorcl$V1)
lillie.test(rdoorclt$V1)

#To test a particular value for \lambda, we can use the function testTransform acting on a powerTransform object

testTransform(powerTransform(rdoorcl$V1~1), lambda=1)
testTransform(powerTransform(rdoorcl$V1~1), lambda=0.3)

# Repeat the whole analysis with variable rdoorop$V1 to check if the transformation improves normality

#Bivariant Normality for the joint variable (rdoorcl$V1,rdoorop$V1)
#Estimating bivariate parameter (\lambda_1,\lambda_2)
powerTransform(cbind(rdoorcl$V1,rdoorop$V1))
plot(powerTransform(cbind(rdoorcl$V1,rdoorop$V1)~1))
summary(powerTransform(cbind(rdoorcl$V1,rdoorop$V1)~1))
#bcPower Transformations to Multinormality 
# Although We can accept the logarithmic transformation for both variables, we are transforming them with (\lambda_1,\lambda_2) values =c(0.16, 0.15).
# Defining the transformed variable with those lambdas
rdoorT=bcPower(cbind(rdoorcl$V1,rdoorop$V1), c(0.16,0.15))

# Redefining some graphical parameters to combine multiple plots into one overall graph
par(mfrow=c(1,2))

#Comparing fitting to a normal distribution before and after the transformation with Mardia test (package MVN)
#Before
mardiaTest(cbind(rdoorcl$V1,rdoorop$V1), qqplot=T)
# We reject normality given p values equal to 0 for skewness and kurtosis
#After
mardiaTest(rdoorT, qqplot=T)
#We have improved normality

## Outliers

# Univariate identification with boxplots. Package car:
Boxplot(hbat[,7], id.method="identify")
Boxplot(hbat[,7], id.method="y")

# with default built-in functions
boxplot(hbat[,7])
boxplot(hbat[,7])$out

#Bivariate outliers: scatterplot + confidence ellipsoide
#With variables hbat[,6:7]
#correlation matrix and mean vector for both variables
corr=cor(hbat[,6:7])[1,2]
mean=colMeans(hbat[,6:7])
# with package "ellipse" plot 95% confidence region for a bivariate normal distribution
plot(ellipse(-0.13,centre=c(7.8,3.6)),type='l',col=2)
# Add points (hbat[,6], hbat[,7])
points(hbat[,6],hbat[,7])
# Identifying points outside the ellipsoide
identify(hbat[,6],hbat[,7])

#Multivariate outliers, "by hand" with Mahalanobis distance, non-robust
#variables hbat[,6:18]
hbat618=hbat[,6:18]
# mean vector, cov matrix and mahalanobis distance
meanhbat618=sapply(hbat618, mean)
covhbat618=cov(hbat618)
mahalanobis618=mahalanobis(hbat618,meanhbat618,covhbat618)
mahalanobis618
# 95th percentile of a chi-squared distribution with 13 degrees of freedom (we are using 13 variables)
#Position of outliers
which(mahalanobis618 > qchisq(0.95,df=13))
#We got 6 outliers, their rows in the data set
pos=which(mahalanobis618 > 22.36)
pos
mahalanobis618[pos]

## To plot outliers in a different color
x=rep(1, 100)
x[pos]=0
# We plot them on a scatterplot of variables 6 and 7.
plot(hbat[,6],hbat[,7],col=x+2,pch=16)
# Visual identification, function qqPlot package car
qqPlot(mahalanobis618,dist="chisq",df=13, line="robust", id.method="identify")

#Package mvoutlier

##### pcout function, robust method based on principal components
hb618.out=pcout(hbat[,6:18], makeplot=TRUE)
# which potential outliers does it find?
which(hb618.out$wfinal01==0)

# plotting each point with each final combined weighed. Small values indicate potential multivariate outliers.
plot(seq(1,100),hb618.out$wfinal)

# Bivariate graphic highlighting outliers in red
plot(hbat[,6],hbat[,7],pch=16,col=hb618.out$wfinal01+2) 

# Interactive chi-squared plot
chisq.plot(hbat[,6:18])

#Four graphs
aq.plot(hbat[,6:18])

# changing the default quantile
which(aq.plot(hbat[,6:18],delta=qchisq(0.95,df=13))$outliers=="TRUE")
which(aq.plot(hbat[,6:18])$outliers=="TRUE")

par(mfrow=c(1,1))
# Symbol plot for two variables
symbol.plot(cbind(hbat[,16], hbat[,17]))

#correlation plot, for two variables
corr.plot(hbat[,16], hbat[,17])
# Other plot for no more than 10 variables
uni.plot(hbat[,6:12])


## Function spm, package car. Very complete, see parameter "transform=TRUE".
spm(hbat[,6:11],reg.line=lm, diagonal="histogram", smoother=FALSE, spread=FALSE, ellipse=TRUE, transform=TRUE)
