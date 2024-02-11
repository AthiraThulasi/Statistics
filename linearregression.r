cars.data <- read.csv ("C:\\Users\\Athira Beena Thulasi\\OneDrive\\Documents\\UFV\\DAC\\Fall Sem\\Stat_271_Data Analysis and Modelling\\UFV_Dataset\\Cars.csv")
summary(cars.data)
str(cars.data)


TPrice <- log10(cars.data$Price) 

model.trf <- lm (TPrice ~ Mileage + Cyl + Liter + Doors + Cruise + Sound + Leather, data = cars.data)
summary(model.trf)

#Forward and backward stepwise regression
forward <- step (model.trf, direction = "forward")
summary(forward)
backward <- step (model.trf, direction = "backward") 
summary(backward)

#Best Subset selection method
library(leaps)
data.pred <-cbind(cars.data$Mileage,cars.data$Cyl,cars.data$Liter,cars.data$Doors,cars.data$Cruise,cars.data$Sound,cars.data$Leather)

#Apply adjR2 to choose the best fitted model 
lm.adjr2 <- leaps (data.pred,TPrice, method = "adjr2", 
                    names = c('Mileage','Cyl','Liter','Doors','Cruise','Sound','Leather'), nbest=3)

#WHICH gives the TRUE indices of a logical object, allowing for array indices.
cb<-cbind (lm.adjr2$which, lm.adjr2$adjr2)

#Then the suggested model is :
Price ~ Mileage + Cyl + Litre + Doors + Cruise + Sound + Leather  

#Mallow's Cp to choose the best fitted model 
lm.cp <- leaps (data.pred,TPrice, method = "Cp", names = c('Mileage','Cyl','Liter','Doors','Cruise','Sound','Leather'), nbest=3) 
cbind (lm.cp$which, lm.cp$Cp)


#The model with all predictor variables has Cp = 8, which is close to the number of parameters in the regression model. So, the suggested model is :
Price ~ Mileage + Cyl +Litres + Doors + Cruise + Sound + Leather + Mileage
#=========================================================
#Assumptions Check - Diagnostic Plot / Residual Plot
#The error terms in the regression model are independent and have
#been sampled from a single population
#epsilon ~ N(0,sigma^2)
#=====================================
#Residual Vs Fitted plot
#The red lines representing the mean of the residuals are all basically horizontal and centred around zero. This means there are no outliers or
#biases in the data that would make a linear regression invalid.
residual <- residuals(model.trf)
fit <- fitted(model.trf)
plot (fit, residual)
abline (0,0, col="red")  
#=====================================
par(mfrow = c(2,2)) 
plot(model.trf)       
#========================================================
#QQ Plot - Fitted values vs. 
#Residuals plot to check heteroskedasticity
#As the residuals are randomly scattered around residual value of 0 with no clear pattern.
#residuals exibits homoskeasticity ie equal variances around the regression line

plot (fit, residual)
abline (0,0, col="red")  

#Histogram of residuals - To check whether the variance is normally distributed
hist(residual)
hist(residual.trf) 
#============================================================
#Observational Plot
#=============================================================
par(mfrow=c(1,2))
n <- length(cars.data$TPrice)
order <- c(1:n)
plot(order, residual, xlab = "Observation Order", ylab = "Residuals", main = "y = Price") 
abline(0,0, col="red")

residual.tr <- residuals(model.trf)
plot(order, residual.tr, xlab = "Observation Order", ylab="Residuals", main = "y = TPrice")
abline(0,0, col="red")
#==============================================================================

#Multicollinearity- VIF measures how much the variance of a regression Coeff is inflated due to multicollinearity in the model 
#VIF () calculates variance Inflation for regression model
model.trf <- lm (TPrice ~ Mileage + Cyl + Liter + Doors + Cruise + Sound+ Leather, data = cars.data)
vif (model.trf)

#==========================================================================
#mpg dataset - Lecture - slide 24
#==========================================================================
mpg.data <- read.csv ("C:\\Users\\Athira Beena Thulasi\\OneDrive\\Documents\\UFV\\DAC\\Fall Sem\\Stat_271_Data Analysis and Modelling\\UFV_Dataset\\MPG.csv")
summary(mpg.data)
str(mpg.data)
attach(mpg.data)
#===================================================
mpg.model <- lm(mpg ~ speed + displacement)
summary(mpg.model)
#================================================
forward <- step (mpg.model, direction = "forward")
summary(forward)
backward <- step (mpg.model, direction = "backward") 
summary(backward)
#==================================================
library(leaps)
data.pred <-cbind(mpg.data$speed,mpg.data$displacement)
lm.adjr2 <- leaps (data.pred,mpg, method = "adjr2", 
                   names = c('speed','displacement'), nbest=3)
cb<-cbind (lm.adjr2$which, lm.adjr2$adjr2)
#================================================================
#Then the suggested model is :
mpg ~ speed + displacement
#===============================================================
#Mallow's Cp to choose the best fitted model 
lm.cp <- leaps (data.pred,mpg, method = "Cp", names =  c('speed','displacement'), nbest=3) 
cbind (lm.cp$which, lm.cp$Cp)

#==================================================================================
resi <- residuals(mpg.model)
fit <- fitted(mpg.model)
plot(resi,fit)
abline (0,0,col="red") 
mpg.data
#==============================================================================
par(mfrow = c(2,2)) 
plot(mpg.model)   
#==============================================================================
mpg.data$speed
speed.sq <- (speed)^2
mpg.model2 <- lm(mpg ~ speed + speed.sq + displacement)
summary(mpg.model)
#================================================================
par(mfrow = c(2,2)) 
plot(mpg.model2) 

#On average, price is estimated to decrease by .03906
#if the car has sound holding all other predictor variables constant.

#y cap = 1.2 + 3.4 I(Leather = 0)+5.6 I(sound=yes)
#ycap = 1.2+ 1.3 I?(make = cadillac)+ 5.6 I(Make = cheverle)
#compare the mode with reference
#compared to buick, price of model cadillac is ---- 
#higher keeping all other predictor variables constant
#On an average, price is estimated to decrease by .000003236 for every 1 unit increase 
#in mileage holding all other predictor variables constant


#anova
#Fcalc =MSR/MSE
#~F(p-1,n-p) << deg of frdm
#p-value = P(Fp-1,n-p > Fcalc )





