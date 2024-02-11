turtles <- read.csv("C:\\Users\\Athira Beena Thulasi\\OneDrive\\Documents\\UFV\\DAC\\Fall Sem\\Stat_271_Data Analysis and Modelling\\UFV_Dataset\\turtles.csv", stringsAsFactors = TRUE)
summary(turtles) 
str(turtles)
attach(turtles)

#standardization
#-----------------------------------------------
Z1 <- scale(length)
Z2 <- scale(width)
Z3 <- scale (height)

#Eigen values & Eigen vectors
#------------------------------------------------

R <- cor(cbind(Z1,Z2,Z3)) 
eigen(R)$values
eigen(R)$vectors


s <- princomp(cbind(Z1,Z2,Z3),cor=TRUE)
#------------------------------------------------

#principal compo analysis of standardized values
#-------------------------------------------------

PC1 = -0.5787981*Z1 + -0.5779840*Z2 + -0.5752628*Z3
PC2 = -0.3250273*Z1 + -0.48346990*Z2 + 0.8127817*Z3
PC3 = 0.74789704*Z1 + -0.65741263*Z2 + -0.09197088*Z3

var(PC1) #2.935738
var(PC2) #0.04284387
var(PC3) #0.02141848


#scatterplot of PC1 versus PC2
#-------------------------------------------------
plot(PC1,PC2)
plot(PC1,PC2,xlim = c(-4,4),ylim=c(-4,4))


# variances of PC1 and PC2
#--------------------------------------
V1 <- var(PC1)/(var(PC1)+var(PC2)+var(PC3)) 

V2 <- (var(PC1)+var(PC2))/(var(PC1)+var(PC2)+var(PC3))


#loading plot
#-------------------------------------------------
turtles.pca <- princomp(cbind(Z1,Z2,Z3),cor=TRUE)
summary(turtles.pca)

evectors <- loadings(turtles.pca)[,1:3] 
evectors

plot(evectors[,1], evectors[,2],xlab="PC1",ylab="PC2",xlim=c(0,0.6))
lines(c(0,evectors[,1][1]),c(0,evectors[,2][1]),lty=1,col=1)
lines(c(0,evectors[,1][2]),c(0,evectors[,2][2]),lty=2,col=2)
lines(c(0,evectors[,1][3]),c(0,evectors[,2][3]),lty=3,col=3)
legend("topleft",c("length","width","height"),lty=1:3,col=1:3,lwd=2,bty="n")

#Scree plot
#-------------------------------------------------------------------------
screeplot(turtles.pca,type = "l")












