papertowels <- read.csv("C:\\Users\\Athira Beena Thulasi\\OneDrive\\Documents\\UFV\\DAC\\Fall Sem\\Stat_271_Data Analysis and Modelling\\UFV_Dataset\\PaperTowels.csv",stringsAsFactors = T)
str(papertowels) 
head(papertowels,150)
attach(papertowels)

library(lattice)
papertowels$Water <- as.factor(Water)
fa.water <- factor(Water)

#Anova
anova2 <- aov(sqrt(Strength) ~ Brand * fa.water, data = papertowels)
summary(anova2)
install.packages("gplots")
library(gplots)
install.packages("ggpubr")

#Mean effect plot
plotmeans(sqrt(Strength)~ Brand, xlab="Brand", ylab="sqrt(Strength)", main="Main Effect Plot for Brand")
plotmeans(sqrt(Strength) ~ Water.fact, xlab="Water", ylab="sqrt(Strength)", main="Main Effect Plot for water")

#Interaction plot
interaction.plot(x.factor = Brand,  trace.factor = fa.water,
                 response = sqrt(Strength), fun = mean, 
                 type = "b", legend = TRUE,
                 pch=c(1,19), col = c(2, 4), 
                 xlab = "Brand", ylab="sqrt(Strength)", main="Interaction plot")




#To find the maximum and minimum standard deviations

tapply(sqrt(Strength), Brand:fa.water, sd) 
max(tapply(sqrt(Strength) , Brand:fa.water, sd))
min(tapply(sqrt(Strength) , Brand:fa.water, sd))

sd.max <- max(tapply(sqrt(Strength) , Brand:fa.water, sd))
sd.min <- min(tapply(sqrt(Strength) , Brand:fa.water, sd))
sd.max/sd.min


#LevenesTest
install.packages("car")
library(carData)
library(car)
leveneTest(anova2)


# Residuals Vs fitted values - check the homogeneity of variances
plot(anova2, 1) 

#Histogram of the residuals - Normality check
res <- anova2$residuals
hist(res, main="Histogram of residuals", xlab="Residuals", col = "yellow")

# Normality plot of the residuals
plot(anova2, 2) 


library(car)

# Tukey multiple pairwise-comparisons
TukeyHSD(anova2)
TukeyHSD(anova2, which = "Brand") # specify the factor Brand
TukeyHSD(anova2, which = "fa.water") # specify the factor Time

#Plotting 95% CI
plot (TukeyHSD(anova2, conf.level=.95),las= 2)

