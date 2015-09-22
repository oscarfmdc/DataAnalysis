## Basic inspection and Manipulation of a data set

# Import data from the url
url="http://lib.stat.cmu.edu/datasets/1993.expo/cereal"
cereals <- read.table(url, header=FALSE, as.is=TRUE, na.strings="-1")
names(cereals) <- c('name','mfr','type','calories','protein','fat','sodium','fiber','carbo',
        'sugars','shelf','potass','vitamins','weight','cups')

# or from local file
cereals <- read.table(cereal.txt, header=FALSE, as.is=TRUE, na.strings="-1")

# structure of the data set
str(cereals)
# checking for missing values (counts missing data by columns)
apply(cereals,2, function(x) sum(is.na(x)))
# or
sapply(cereals, function(x) sum(is.na(x)))
#deletes rows with missing values creating new data frame cereal
cereal=na.omit(cereals)
str(cereal)

# Types of variables: quantitative, qualitative, nominal, ordinal, interval, ratio.

# Creating a factor
cereal$shelf <- factor(cereal$shelf, levels=1:3)
cereal$shelf
table(cereal$shelf)
boxplot(sugars~shelf, data=cereal)

## Basic summaries of a data set
summary(cereal)
## More statistics: sd, skewness, kurtosis. Standard deviation of a specific column (variable), for instance "fiber":
sd(cereal[,8])
# or
sd(cereal$fiber)
## Skewness and kurtosis need package "moments" to be loaded.
apply(cereal[,7:10],2, kurtosis)
# defining a function to calculate coefficient of variation:
cv=function(x){
sd=sqrt(var(x))
mu=abs(mean(x))
sd/mu}
# using it 
apply(cereal[,7:10],2, cv)

#other function
cereal_stats=data.frame(
cfv=apply(cereal[,c(4:10,12:15)],2,cv),
kurtosis=apply(cereal[,c(4:10,12:15)],2,kurtosis),
skew=apply(cereal[,c(4:10,12:15)],2,skewness))
#using it
print(cereal_stats, gap=3)

## R functions to loop over elements of data structures

## by() lets you apply a function to a data frame split by factors. For instance, means for variables sodium, fiber, carbo and sugars, columns
## 7 to 10
by(cereal[, 7:10], cereal$shelf, colMeans)
by(cereal[, 7:10], cereal$shelf, max)

## Grouping when the subgroups are defined by the unique combinations of a list of several factors
## function summaryBy, package doBy
summaryBy(sugars + calories ~ mfr + shelf, data = cereal, FUN=function(x){c(m=mean(x),s=sd(x))})
summaryBy(sugars + calories ~ mfr + shelf, data = cereal, FUN=function(x){c(m=mean(x),s=sd(x),n=length(x))})


## The same with package dplyr and plyr
ddply(cereal, c("mfr","shelf"),summarise,N=length(sugars),sugars.m=mean(sugars), sugars.sd=sd(sugars),calories.m=mean(calories), calories.sd=sd(calories))

## Package dplyr, provides a consistent and concise grammar for manipulating tabular data. The five main data manipulation 'verbs' in #dplyr are: select(), filter(), arrange(), mutate(), and summarize().
## optional but advisable
cerealt=tbl_df(cereal)
glimpse(cerealt)

#summarise
summarise(group_by(cerealt,mfr,shelf), length(calories),mean(calories), sd(calories), mean(fiber), sd(fiber))
summarise(group_by(cerealt,mfr,shelf), length(sugars),mean(sugars), sd(sugars), mean(calories), sd(calories))

## optional: piping example
cerealt %>% group_by(shelf) %>% summarise(avg=mean(potass)) %>%arrange(avg)
cerealt %>%  group_by(shelf, mfr) %>% summarise(avg=mean(potass))

###
summarise_each(cerealt[,c(4,10)], funs(mean, sd))
summarise_each(cerealt[7:10], funs(mean, sd, max, min))
glimpse(summarise_each(cerealt[7:10], funs(mean, sd, max, min)))
glimpse(summarise_each(cerealt[7:10], funs(skewness, kurtosis)))


## Basic plotting
## for a single variable:  hist., boxplot, stem and leaf, density >plot(density(var)).  Hist patterns
## two variables, scatterplot, with points, grids, text. 13-ggplot.pdf, Lab 05
## Basic scatterplot for numerical variables
pairs(cereal[,c(4:10,12:13)])
## We can enhace somehow these basic plots changing the default parameters or including more plots in the diagonal.


## Function spm from package "car":
spm(cereal[,c(4:5,7:10,12)], reg.line=lm, diagonal="histogram",smoother=FALSE, spread=FALSE)
spm(cereal[,c(4:5,7:10,12)], reg.line=lm, diagonal="boxplot",smoother=FALSE, spread=FALSE)

## bubble plots, adding a third variable as a circle
symbols(cereal$fiber, cereal$potass, circles=cereal$shelf, inches=0.2, bg="lightblue")
identify(cereal$fiber, cereal$potass, labels=abbreviate(cereal[,1]))

## stars
stars(cereal[,7:10],frame.plot=T, key.loc=c(20,2),axes=T)
stars(cereal[,7:10],frame.plot=T, key.loc=c(20,2),axes=T, labels=abbreviate(cereal[,1]))
stars(cereal[,c(4,7,9:10)],frame.plot=T, key.loc=c(20,2),axes=T,labels=abbreviate(cereal[,1]), draw.segments=TRUE, scale=TRUE, full=FALSE)

### conditional plots
coplot(cereal$fiber~cereal$potass|cereal$sugars)
coplot(cereal$fiber~cereal$potass|cereal$sugars, rows=1)
#conditioning on non overlapping intervals of the variable sugars. 
coplot(cereal$fiber~cereal$calories|cereal$sugars,rows=1,overlap=0)

#conditioning on non overlapping intervals defined by us. First, we define a matrix of intervals
gv=c(0,4,4,8,8,12,12,16,16,21)
gv=matrix(gv,nrow=5,ncol=2,byrow=T)
#then we use it
coplot(cereal$fibre~cereal$calories|cereal$sugars, given.values=gv,rows=1)


# We can do it much better: package ggplot2
# create a ggplot object which will serve as the basis
# for a scatter plot of fiber vs potass
bs=ggplot(cereal, aes(x=fiber,y=potass))+ggtitle("Fiber vs Potassium")
bs
# adding layers
bs+geom_point()
bs+geom_point(aes(size=shelf))
bs+geom_point(aes(shape=shelf),size=3)
bs+geom_point(aes(color=shelf))
bs+geom_point(aes(color=shelf, size=vitamins))
bs+geom_point(aes(color=fat, size=vitamins, shape=shelf))

#adding different regression lines to each subset in shelf
ggplot(cereal, aes(x=fiber,y=potass, color=factor(shelf)))+geom_point()+geom_smooth(method="lm")

#ggplot2 is mainly intended to produce graphics
# of more than one variable. If you want to get univariate
# charts, probably ggplot2 may not be the best option, but still
#histogram, default histogram
ggplot(data = cereals, aes(x = calories)) +geom_histogram()

#histogram, changing bandwith
ggplot(data = cereals, aes(x = calories)) +geom_histogram(binwidth=13)


#boxplots

ggplot(cereal, aes(x=shelf,y=sugars))+geom_boxplot()
ggplot(cereal, aes(x=shelf,y=sugars, fill=shelf))+geom_boxplot()
ggplot(cereal, aes(x=shelf,y=sugars, fill=shelf))+geom_boxplot()+geom_point()
ggplot(cereal, aes(x=shelf,y=sugars, fill=shelf))+geom_boxplot()+geom_jitter()


# densities
ggplot(cereal, aes(x=sugars,group=shelf))+geom_density()
ggplot(cereal, aes(x=sugars,group=shelf))+geom_density(aes(fill=shelf), alpha=0.7)

#barplots
ggplot(cereal,aes(x=shelf,fill=factor(vitamins)))+geom_bar()
ggplot(cereal,aes(x=shelf,fill=factor(vitamins)))+geom_bar(position=position_dodge())
ggplot(cereal,aes(x=shelf,fill=factor(fiber)))+geom_bar()


#faceting
bs+geom_point(aes(color=shelf))+facet_grid(~vitamins)
bs+geom_point(aes(color=shelf))+facet_grid(vitamins~.)
bs+geom_point(aes(color=shelf))+facet_grid(~mfr)
bs+geom_point(aes(color=shelf),size=3)+facet_grid(vitamins~mfr)
bs+geom_point()+geom_smooth(method="lm")+facet_grid(shelf~.)
ggplot(cereal, aes(x=fiber,y=potass,color=shelf))+geom_point()+geom_smooth(method="lm")+facet_grid(shelf~.)
ggplot(cereal, aes(x=fiber,y=potass,color=shelf))+geom_point()+geom_smooth(method="lm", aes(fill=shelf))+facet_grid(shelf~.)

ggplot(cereal, aes(x=fiber,y=potass, color=shelf))+geom_point()+geom_smooth(method="lm", aes(fill=shelf))

## package GGally
ggpairs(cereal[,c("sugars","protein","shelf")])
ggpairs(cereal[,c("fiber","potass","shelf")])
ggpairs(cereal[,c("fiber","potass","mfr")])
custom_pl <- ggpairs(cereal[,c("sugars","protein","fiber")], upper = c(cor), title = "Custom Example")
ggpairs(cereal[,c("sugars","protein","fiber")],  upper = list(continuous = "density", combo = "box"))

## Reproduce the bar plot representing the class' survey results
V1=c("new user", "occasional user", "advanced", "1", "2", "+2")
V2=c(20,10,1,19,8,4)
V3=c(1,1,1,2,2,2)
V4=abbreviate(V1)
survey=data.frame(V1,V2,V3,V4)
rownames(survey)=seq(1:6)
ggplot(survey,aes(V4,y=V2,fill=as.factor(V3)))+geom_bar(stat="identity")+ylab("  ")+xlab("survey results")
