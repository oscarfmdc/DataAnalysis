#check you have the correct data set
head(cereal.pc)

#summary of the variables, shelf is a factor
summary(cereal.pc)
str(cereal.pc)

#covariance matrix, very different variances
cov(cereal.pc[,-9])

#load FactoMineR and perform a PCA analysis on matrix R, not S! Store the results in object cereal_pca_r. We are using shelf as
# a supplementary qualitative variable. By default 5 components are calculated, use ncp= to change it.
cereal_pca_r=PCA(cereal.pc,quali.sup=9,scale.unit=TRUE, graph=FALSE)

# by default dimensions 1 and 2 are plotted. We are using that option. To change them use axes=c(1,3).
# type cereal_pca_r to see the extensive list of results provided in the output of PCA()
cereal_pca_r

#summary of the numerical output
summary(cereal_pca_r)

#Working on the map of points
plot(cereal_pca_r, cex=0.7)

# coloring points by variable shelf
plot(cereal_pca_r, cex=0.7, shadow=TRUE, habillage=9)

# if we don't want shelfs on the plot
plot(cereal_pca_r, cex=0.7, shadow=TRUE, habillage=9, invisible="quali")

#labels for those points with cosin squared greater than 0.7, for example
plot(cereal_pca_r, cex=0.7, shadow=TRUE, habillage=9, invisible="quali", select="cos2 0.7")

# plotting only selected observations
plot(cereal_pca_r, cex=0.7, shadow=TRUE, habillage=9, invisible="quali", select="cos2 0.7", unselect=1)

#selecting a color for unselected points
plot(cereal_pca_r, cex=0.7, shadow=TRUE, habillage=9, invisible="quali", select="cos2 0.7", unselect="grey70")

# To select the five observations that contribute the most to the two first components, the more extreme individuals in both components
plot(cereal_pca_r, cex=0.7, shadow=TRUE, habillage=9, invisible="quali", select="cos2 5")

#selecting particular individuals by their names
plot(cereal_pca_r, cex=0.8, shadow=TRUE, habillage=9, invisible="quali", select=c("100%", "Ap_J", "Bs_4"))

#selecting particular individuals by their row in the dataset
plot(cereal_pca_r, cex=0.8, shadow=TRUE, habillage=9, invisible="quali", select=1:10)

## Working on variables, circle of correlations
plot(cereal_pca_r, choix="var")

# Ploting the 3 variables that contribute the most to the representation 
plot(cereal_pca_r, shadow=TRUE,choix="var", select="contrib 3" )

# selecting variables by their contributions
plot(cereal_pca_r, shadow=TRUE,choix="var", select="cos2 0.7" )

# Quality of each variable representation on these two dimensions
cereal_pca_r$var$cos2[,1:2][,1]+ cereal_pca_r$var$cos2[,1:2][,2]

# First a table, then a barplot of variable contribution
rbind(cereal_pca_r$var$contrib, TOTAL=colSums(cereal_pca_r$var$contrib))
barplot(t(cereal_pca_r$var$contrib), beside=TRUE, legend.text=TRUE,args.legend=list(x="topleft", bty='n', ncol=5))

### Objects contribution
objcontrib=data.frame(C1=cereal_pca_r$ind$contrib[,1],C2=cereal_pca_r$ind$contrib[,2],n=rownames(objcontrib))

# Barplots of object contributions to PC 1
par(mfrow=c(2,1))
barplot(objcontrib$C1[1:30],cex.names=0.7, names.arg=objcontrib$n[1:30])
abline(h=100/65, col="gray50")
barplot(objcontrib$C1[31:65],cex.names=0.7, names.arg=objcontrib$n[31:65])
abline(h=100/65, col="gray50")

#If you want to order observations by the first component value (score), make a data frame with the two first scores, for instance 
obsor=data.frame(C1=cereal_pca_r$ind$coord[,1],C2=cereal_pca_r$ind$coord[,2])

# ¡it will be printed out on your screen!
obsor[order(obsor[,1], decreasing=TRUE),]

# scree plot
plot(cereal_pca_r$eig$eigenvalue, type="l")
points(cereal_pca_r$eig$eigenvalue)

# scree plot (barplot type)
barplot(cereal_pca_r$eig$eigenvalue, names.arg=rownames(cereal_pca_r$eig))

# correlation coefficients between variables and dimensions and significance tests
dimdesc(cereal_pca_r,axes=c(1,2))

# To see whether the categories of the supplementary variable are significantly different from each other, 
# we can draw confidence ellipses around them. It #seems that the characteristics of cereals on each shelf are different.
concat = cbind.data.frame(cereal.pc[,9],cereal_pca_r$ind$coord)
ellipse.coord = coord.ellipse(concat,bary=T)
plot.PCA(cereal_pca_r,habillage=9,ellipse=ellipse.coord,cex=0.8)

## Clustering partition, using hierarchical clustering
cereal_cluster=hclust(dist(cereal_pca_r$ind$coord), method="ward.D")
plot(cereal_cluster)
rect.hclust(cereal_cluster, k = 3, border ="blue")
cereal_clusters=cutree(cereal_cluster, k=3)
cereal_clusters

#PC plot with clustering partition
cereal_pca_obs=data.frame(C1=cereal_pca_r$ind$coord[,1],C2=cereal_pca_r$ind$coord[,2], cluster=factor(cereal_clusters))
ggplot(cereal_pca_obs, aes(x=C1,y=C2, label=rownames(cereal.pc)))+
geom_hline(yintercept=0, color="gray70")+
geom_vline(xintercept=0,color="gray70")+
geom_point(aes(color=cluster), alpha=0.55, size=2)+
geom_text(aes(color=cluster),alpha=0.55)

## Univariate clustering, with PC1, using mclust. First install package "mclust"
mcl4=Mclust(cereal_pca_r$ind$coord[,1])
summary(mcl4)
par(mfrow=c(2,2))
plot(mcl4)

#############################################################################
### BIPLOTGUI, transform shelf to numerical values
cereal.pc.b=cereal.pc
cereal.pc.b$shelf=as.numeric(cereal.pc.b$shelf)
Biplots(Data=cereal.pc.b[,-9])

#canonical variate analysis for discriminating between points 
Biplots(Data=cereal.pc.b[,-9], groups=cereal.pc.b[,9])