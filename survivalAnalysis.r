install.packages("survival")
library(survival)


grad.data <- read.csv("C:\\Users\\Athira Beena Thulasi\\OneDrive\\Documents\\UFV\\DAC\\Fall Sem\\Stat_271_Data Analysis and Modelling\\UFV_Dataset\\Graduation.csv")
str(grad.data)
attach(grad.data)
plot(grad.data)

KM.grad <- survfit(Surv(Years,Censor) ~ Gender, data = grad.data)
KM.grad

plot(KM.grad,lty=1:2,col = 1:2,xlab ="Years", ylab ="probability of survival",main = "Kaplan Meier survival by gender type" )
legend(1,.5,c("Female","Male"),lty=1:2, col=1:2, bty="n")

#---------------------------------------------------
#Log-rank test - Observed Events Vs Expected Events
#---------------------------------------------------
survdiff(Surv(Years,Censor) ~ Gender, data=grad.data)

#-------------------------------------------------------
#Estimated Mean survival Time
#-------------------------------------------------------
print(KM.grad, print.rmean=TRUE, rmean="individual")


#-------------------------------------------------------
#Estimated Median survival time
#-------------------------------------------------------
print(KM.grad)


#---------------------------------------------------------
#Confidence Interval for Male
#---------------------------------------------------------
data.male <- grad.data[grad.data$Gender=="Male",]
KM.male.95 <- survfit(Surv(data.male$Years, data.male$Censor) ~ Gender, conf.int = 0.95,data=data.male)
summary(KM.male.95)

#-------------------------------------------------------------
#Confidence Interval for Female
#-----------------------------------------------------------
data.female <- grad.data[grad.data$Gender=="Female",]
KM.female.95 <- survfit(Surv(Years, Censor) ~ Gender, conf.int = 0.95, data=data.female)
summary(KM.female.95)
--------------------------------------------------------------------
plot(KM.female.95, conf.int = TRUE, lty=1:2, col=1:2, xlab="Time in Second", ylab="Survival  Probability", 
main="Kaplan-Meier Estimate with 95% CI \n by Chocolate Type")

# Add a legend to the plot to distinguish between the two groups
legend(1,.5,c("Milk","White"),lty=1:2, col=1:2)

# Examine the values of the 95% confidence intervals 
summary(KM.female.95)

#--------------------------------------------------------------------
#Estimated cumulative hazard function
#------------------------------------------------------------------
KM.male<- survfit(Surv(Years,Censor) ~ Gender, data = data.male)
summary(KM.male)

#--------------------------------------------------------
#plot cumulative haz fun
#-----------------------------------------------------------

plot(KM.male,conf.int = FALSE, lty=1, col=1, fun="cumhaz", xlab="Time in years", ylab="Cumulative Hazard",
     main="Cumulative Hazard Function \n by time in years") 

----------------------------------------------------------------------------------------

plot(KM.female.95,conf.int = FALSE, lty=1, col=1, fun="cumhaz", xlab="Time in years", ylab="Cumulative Hazard",
       main="Cumulative Hazard Function \n by time in years") 


#Haz_fun <- KM.male$n.event / KM.male$n.risk
#Haz.est <- cumsum(Haz_fun)