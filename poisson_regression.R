data.gala <- read.csv("C:\\Users\\Athira Beena Thulasi\\OneDrive\\Documents\\UFV\\DAC\\Fall Sem\\Stat_271_Data Analysis and Modelling\\UFV_Dataset\\Gala.csv",stringsAsFactors =T) 
                        
data.gala
str(data.gala)
attach(data.gala)

#----------------------------------------------------------------------------------------------
#log(species) versus the potential covariates: area and elevation.
par(mfrow=c(2,2))
plot(data.gala$area,log(data.gala$species), col = "red", xlab="area", ylab="log(species)")

plot(data.gala$elevation,log(data.gala$species), col = "red", xlab="elevation", ylab="log(species)")

plot(log(data.gala$area),log(data.gala$species), col = "red", xlab="log(area)", ylab="log(species)")

plot(log(data.gala$elevation),log(data.gala$species), col = "red", xlab="log(elevation)", ylab="log(species)")


#----------------------------------------------------------------------------------------------------
#Fit a Poisson regression model to the total species count with covariates log (area) and log (elevation)
#--------------------------------------------------------------------------------------------------------
#Wald's Test 
model.species <- glm(species ~ log(area) + log(elevation) , family = poisson, data = data.gala)
summary(model.species)

#--------------------------------------------------------------
# The deviance test for (model.species)to check whether the model fits the data
#--------------------------------------------------------------
model.species$deviance
model.species$df.residual
1 - pchisq(model.species$deviance, model.species$df.residual)
residuals(model.species,type="deviance")



poi.gala <- glm(species~log(area) + log(elevation), family = poisson, data = data.gala)
summary(poi.gala)

poi.gala$deviance
poi.gala$df.residual
1 - pchisq(poi.gala$deviance,poi.gala$df.residual)



# The Pearson Chi-square test for model (model.species)
#------------------------------------------------------
pearson.calc <- sum(residuals(model.species,"pearson")^2)
1 - pchisq(pearson.calc, 27)

#-------------------------------------------------------------------------------------------------------------
model.species1 <- glm(species ~ log(area) + log(elevation) + nearest + scruz + adjacent , family = poisson, data = data.gala)
summary (model.species1)

#----------------------------------------------------------------------------------------
#Likelihood Ratio Test - determines if at least one covariate is significant in the model
#----------------------------------------------------------------------------------------
Gcalc <-model.species$deviance -  model.species1$deviance 
p.value <- 1 - pchisq(Gcalc, 3)
c(Gcalc, p.value)


#--------------------------------------------------------------
# The deviance test for (model.species1)to check whether the model fits the data
#--------------------------------------------------------------
model.species1$deviance
model.species1$df.residual
1 - pchisq(model.species1$deviance, model.species1$df.residual)
residuals(model.species1,type="deviance")

# The Pearson Chi-square test for model (model.species1)
#------------------------------------------------------
pearson.calc <- sum(residuals(model.species1,"pearson")^2)
1 - pchisq(pearson.calc, 24)

#-----------------------------------------------------------------
#compare 2 models
#----------------------------------------------------------------
model.species <- glm(species ~ log(area) + log(elevation) , family = poisson, data = data.gala)
summary(model.species)

model.species1 <- glm(species ~ log(area) + log(elevation) + nearest + scruz + adjacent , family = poisson, data = data.gala)
summary (model.species1)

AIC(model.species1, model.species)
BIC(model.species1, model.species)



