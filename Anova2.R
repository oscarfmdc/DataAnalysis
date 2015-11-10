## X1 is customer type, 3 different types of costumers (1=less than a year, 2=between 1 and 5 years, 3=longer than 5 years).
## X20 is likelihood of recommendation. We want to know if there are differences
## in X20 across the groups
boxplot(hbat$X20~hbat$X1)
anov1=lm(X20~factor(X1), data=hbat)
summary(anov1)
# package dplyr
hbat %>% group_by(X1) %>% summarise(mean(X20))
par(mfrow=c(2,2))
plot(anov1)
par(mfrow=c(1,1))
## ANOVA hypothesis: normality, constant variance between groups, independence of observations.
## testing Normality in the groups
 shapiro.test(hbat$X20[hbat$X1==1])
 shapiro.test(hbat$X20[hbat$X1==2])
 shapiro.test(hbat$X20[hbat$X1==3])
## or testing normality through the residuals
shapiro.test(anov1$residuals)

## Testing homogeneity of variances, very sensitive to non normality
bartlett.test(hbat$X20~hbat$X1)
## residuals independence
dwtest(anov1, alternative="two.sided")
##  Welch test, in case of different variances
oneway.test(X20~factor(X1)-1, data=hbat)

## Anova result tells us that there are differences in the mean values of the groups, but it doesn't tell anything about
## which groups are different. We colud try a series of t.test() but they don't control for family wise error rate. If we don't control
# this error, the probability of rejecting (one or some) H_0's being true is higher than 0.05. 
hbatt=data.frame(X1=hbat$X1, X20=hbat$X21)
 t.test(hbatt$X20[hbatt$X1==2],hbatt$X20[hbatt$X1==3])
 t.test(hbatt$X20[hbatt$X1==2],hbatt$X20[hbatt$X1==1])
 t.test(hbatt$X20[hbatt$X1==3],hbatt$X20[hbatt$X1==1])
#We find significant differences in 1 and 2, and, 1 and 3. But this is not the way to proceed when we are making many 
#comparison of this type. We have to adjust p-values.
##Using the function aov, it permits us to do that.

## 
anova2=aov(X20~X1, data=hbat)
## Once we have model anova2, fitted with the function aov(), we can use Levene test to test constant variances. It is less
#sensitive to non normality, package car.
leveneTest(anova2)

summary(anova2)
# General linear hypothesis and multiple comparisons for parametric models, package multcomp
summary(glht(anova2,linfct=mcp(X1="Tukey")))
plot(glht(anova2,linfct=mcp(X1="Tukey")))
confint(glht(anova2,linfct=mcp(X1="Tukey")))
cld(glht(anova2,linfct=mcp(X1="Tukey")))
plot(cld(glht(anova2,linfct=mcp(X1="Tukey"))))

## X22 is the percentage of purchases from hbat
anova3=aov(X22~X1, data=hbat)
par(oma=c(0,0,1,0))
plot(cld(glht(anova3,linfct=mcp(X1="Tukey"))))

#An example with 5 groups
#competition=read.table("competition.txt", header=TRUE)
boxplot(competition$biomass~competition$clipping)
bartlett.test(competition$biomass~competition$clipping) #equal variances in each group
anova4=aov(biomass~clipping, data=competition)
summary(anova4)
summary(glht(anova4,linfct=mcp(clipping="Tukey")))
cld(glht(anova4,linfct=mcp(clipping="Tukey")))
plot(cld(glht(anova4,linfct=mcp(clipping="Tukey"))))


## Ancova with data perf
perf$T=factor(perf$T)
boxplot(perf$Y~perf$T)
boxplot(perf$S~perf$T)
plot(perf$S, perf$Y)
plot(perf$S, perf$Y, col=perf$T)
ancova3=lm(perf$Y~perf$S*perf$T) #model with interactions, different slopes for regression lines
summary(ancova3)

plot(perf$S,perf$Y,col=perf$T)
abline(a=14.5646,b=0.9217,col=1)
abline(a=14.5646+17.096,b=0.9217,col=2)
abline(a=14.5646+17.7669,b=0.9217+0.5018,col=3)

ncvTest(ancova3)
 bptest(ancova3)

#comparing nested models, one with three parallel lines and the one with different slopes
anova(lm(perf$Y~perf$S+perf$T), ancova3)
# we should use the one with different slopes, the extra number of parameters (2) is worth 
#the reduction in the residual sum of squares.

#### relevel a factor
perf$T=relevel(perf$T, ref="2")
str(perf)

