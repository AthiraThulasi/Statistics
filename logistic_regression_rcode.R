burns<- read.csv("C:\\Users\\Athira Beena Thulasi\\OneDrive\\Documents\\UFV\\DAC\\Fall Sem\\Stat_271_Data Analysis and Modelling\\UFV_Dataset\\Burns.csv")
attach(burns)
head(burns,20)
str(burns)
#=============================
#Logistic Regression Model
#=============================
burns.model <- glm (Survive ~ Area, family = binomial)
summary(burns.model)
burns.model$deviance
pearson.calc <- sum(residuals(burns.model,"pearson")^2)
#==========================================
#Null model
burns.nullmodel <- glm (Survive ~ 1, family = binomial)
summary(burns.model)

#=====================================
#coef- Extract model coeff from objects
#=====================================
coef(burns.model)

#==================================
#Values are saved in bo and b1
#==================================
b0 <- burns.model$coeff[1]
b1 <- burns.model$coeff[2]
odds.ratio <- exp(-1.3724)
#====================================
#Finding the std error of coefficients
#====================================
install.packages("arm") #library for se.coef()
library(arm)
se.coef(burns.model)
se.b0 <- se.coef(burns.model)[1]
se.b1 <- se.coef(burns.model)[2]

#===============================================
#Expected probability of survival as per area
#===============================================

burns.prob <- predict(burns.model, type = "response") 
burns.prob 

#===============================================================
#create sequence of area 
x.Area <- seq(min(Area), max(Area), 0.05)
prob.survive <- predict(burns.model, data.frame(Area = x.Area), type = "response") 
data.frame(x.Area, prob.survive)

#=================================================
# Plot the probability of survival Vs median area
#================================================

plot(Area,Survive, xlab = "Area", ylab = "Estimated Probability of survival")
lines(x.Area, prob.survive, col = "red", lwd = 2)

#There exists a negative relationship between area and probability of survival. 
#When area increases, the estimated probability of survival decrease.
#================================================================================
## 95% CI for beta1
LB.95 <- b1 - qnorm(0.975,0,1)*se.b1 
UB.95 <- b1 + qnorm(0.975,0,1)*se.b1
CI.95 <- c(LB.95, UB.95)  
CI.95
exp(CI.95)

#=========================================================================
#likelihood ratio test #Gcalc & pvalue from chisquare
G.calc <- 525.39 - 332.66
pvalue <- 1-pchisq(G.calc, df=1)

#Pearson residuals & Normal probability plot
#Majority of the residuals follow an approximately straight line (except a very few)
#in the normal probability plot, so we can assume normality.  
#=========================================================================
res.pearson <- residuals(burns.model,type ="pearson")
res.pearson
qqnorm(res.pearson)
qqline(res.pearson, col="blue") 
#=========================================================================
#Deviance residuals
dev.res <- residuals(burns.model,type="deviance")
qqnorm(dev.res)
qqline(dev.res, col = "red")

#==========================================================================
#Hosmer and Lemeshow were based on g > p + 1
#g - num g grps
.
#Ho: Logistic regression model adequately fits the data 
#Ha: Logistic regression model does not fit the data
library (ResourceSelection)
hoslem.test(burns.model$y, fitted(burns.model), g = 3)

#****************************************************************************
#*****************************************************************************

burns.alternative<- read.csv("C:\\Users\\Athira Beena Thulasi\\OneDrive\\Documents\\UFV\\DAC\\Fall Sem\\Stat_271_Data Analysis and Modelling\\UFV_Dataset\\Burns_alternative.csv")
attach(burns.alternative)
head(burns.alternative)
str(burns.alternative)
#=============================
#Logistic Regression Model
#=============================

area.alter <- c(2.86,3.95,4.75,5.36,6.03,6.77,7.58,8.49,9.49)
survived<- c(13,19,67,45,71,50,35,7,1)
dead <- c(0,0,2,5,8,20,31,49,12)

## fit the logisitc regression model
model.alternative <- glm(cbind(survived,dead) ~ area.alter, family = binomial)
#model.alternative1 <- glm(survived ~ area.alter, family = binomial)
summary(model.alternative)

#=====================================
#coef- Extract model coeff from objects
#=====================================
coef(model.alternative)

#==================================
#Values are saved in bo and b1
#==================================
b0 <- model.alternative$coeff[1]
b1 <- model.alternative$coeff[2]
odds.ratio <- exp(-1.3724)
#====================================
#Finding the std error of coefficients
#====================================
install.packages("arm") #library for se.coef()
library(arm)
se.coef(model.alternative)

se.b0 <- se.coef(model.alternative)[1]#saving std error value in se.bo [1]is first value
se.b1 <- se.coef(model.alternative)[2]

#===========================================================================

#create sequence of area 
x.Area1 <- seq(min(area.alter), max(area.alter), 0.05)
prob.survive <- predict(model.alternative, data.frame(Area = x.Area1), type = "response") 
data.frame(x.Area, prob.survive)

#=================================================
# Plot the probability of survival Vs median area
#================================================
plot(area.alter,survived, xlab = "Area", ylab = "Estimated Probability of survival")
lines(x.Area, prob.survive, col = "red", lwd = 2)

#===============================================
#Expected probability of survival as per area
#===============================================

alter.prob <- predict(model.alternative, type = "response") 
alter.prob 


#================================================================================
## 95% CI for beta1
#e^(b₁−Z ∗se(b1))
#e(b₁+Z ∗se(b₁))


LB.95 <- b1 - qnorm(0.975,0,1)*se.b1 
UB.95 <- b1 + qnorm(0.975,0,1)*se.b1
CI.95 <- c(LB.95, UB.95)  
CI.95
exp(CI.95)
#When the Wald confidence interval for the odds ratio does not contain 1,we reject H0 and conclude that the odds of success do depend on the
#explanatory variable xi 
#If the interval contains 1, we fail to reject H0.

#=========================================================================
#Ho: β1= 0 Ha: β1!= 0
#likelihood ratio test #Gcalc & pvalue from chisquare
#Gcalc = Null deviance - Residual deviance
#calculates the difference between the adequacy of the full and reduced log-likelihood models.
#Gcal ~ χ2 with df = the number of parameters in the full model - that in the reduced model
G.calc <- 198.712 - 5.983
pvalue <- 1-pchisq(G.calc, df=1)

#Pearson residuals & Normal probability plot
#=========================================================================
res.pear <- residuals(model.alternative,type ="pearson")
res.pear
qqnorm(res.pear)
qqline(res.pear, col="blue") 
#=========================================================================
#Deviance residuals
dev.resid <- residuals(model.alternative,type="deviance")
qqnorm(dev.resid)
qqline(dev.resid)
#==========================================================================
#Pearson chi-sqre goodness-of-fit test
#H0 : the logistic regression model provides an adequate fit to the data
#Ha : the model does not adequately fit the data
df = n-p+1
#chi square = sigma i = 1 to n (ri)^2

r <- residuals(model.alternative,"pearson")
chisq.pearson <- sum(r^2)
pchisq(chisq.pearson, 7, lower.tail = F)

#====================================================================
# Deviance goodness-of-fit test
#D2 = residual (full model) deviance
#df = the number of groups - the number of parameters being estimated
model.alternative$deviance
pchisq(model.alternative$deviance, 7, lower.tail = F)
?pchisq

#====================================================================
#test statistic for pearson n deviance
pearson.calc <- sum(residuals(model.alternative,"pearson")^2)
1 - pchisq(pearson.calc, 24)

model.alternative$deviance


#==================================================================

#walds test
#H₀ :   β₁ = 0
#Hₐ :   β₁!= 0
#b₁ − 0
#Zcalc = b1 - 0/se(b₁)
#p − value = 2P(Z ≥ |Zcₐlc |)


#seq.area <- seq(min(Area), max(Area), 0.01) # create a sequence burn area values
#prob.survive <- predict(logmod,newdata=data.frame(Area=seq.area),type = "response")
#prob.survive
#data.frame(seq.area, prob.survive)

#plot(Area, Survive, xlab = "midpoint of set intervals(sq. cm.)", ylab = "Estimated Probability of Survival")
#lines(seq.area, prob.survive, col = "red", lwd = 2) #fitted logistic regression model



















