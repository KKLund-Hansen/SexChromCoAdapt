################################################################################################
########################################### SEX RATIO ##########################################
################################################################################################

#Set up environment
library(car)
library(Hmisc)

#Read in csv file with data
SR.data <- read.table(file = "SexRatio.csv", header = T, sep = ",")


##########################################  STATISTIC  #########################################


### CALCULATE THE PROPORTION OF MALE OFFSPRING ###
SR.data$prop_male <- SR.data$male / SR.data$total


##############################
######### POPULATION #########
##############################


### LINEAT MODEL ###

#Linear mixed model testing if population has a significant effect on the proportion of males with block
model.SRp <- lm(prop_male ~ population + block, data = SR.data)
#ANOVA
anova(model.SRp, test = "F")
#Not significant, P = 0.9336
#Block is not significant, P = 0.7572


#Homogeneity of variances
leveneTest(SR.data$prop_male, SR.data$population)
#Non significant, assumption of homogeneity of variances is met. Can use ANOVA

#Test the model by looking at the residuls
resid.SRp <- residuals(model.SRp)
hist(resid.SRp)
shapiro.test(resid.SRp)
qqnorm(resid.SRp)
qqline(resid.SRp)
plot(resid.SRp)
#The residuls are normally distrubuted, meaning that the model is valid



#############################
######### TREATMENT #########
#############################


### LINEAT MODEL ###

#Linear model testing if treatment has a significant effect on the proportion of males with block
model.SRt <- lm(prop_male ~ treatment + block, data = SR.data)
#ANOVA
anova(model.SRt, test = "F")
#Treatment is not significant, P = 0.2167
#Block is not significant, P = 0.7496


#Homogeneity of variances
leveneTest(SR.data$prop_male, SR.data$treatment)
#Non significant, assumption of homogeneity of variances is met. Can use ANOVA

#Test the model by looking at the residuls
resid.SRt <- residuals(model.SRt)
hist(resid.SRt)
shapiro.test(resid.SRt)
qqnorm(resid.SRt)
qqline(resid.SRt)
plot(resid.SRt)
#The residuls are normally distrubuted, meaning that the model is valid



#########################################  PLOT DATA  #########################################


### PLOT POPULATION ###


#Add fitted values
SR.data$fittedPop <- fitted(model.SRp)

#Caluclate mean
SRpop.M <- tapply(SR.data$fittedPop, SR.data$population, mean)
#SE
SRpop.SE <- (summary(model.SRp)$coefficients[,2])[-13]


#Plot
par(mar = c(6, 5, 2, 2))
plot(NULL, xlim = c(0, 8), xlab = "", xaxt = "n", ylim = c(0.3, 0.7),
     ylab = "Proportion of male offspring", cex.axis = 1.2, cex.lab = 1.5, las = 1)
#Add points
stripchart(SR.data$prop_male ~ SR.data$population, add = T, vertical = T,
           method = "jitter", pch = c(16, 16,17,18,17,18, 16,17,18,17,18, 16), 
           cex = 1.5, col = "gray80", at = c(0.5, 1.5,2,2.5,3,3.5, 4.5,5,5.5,6,6.5, 7.5))
#Add errorbars
xSRpop <- c(0.5, 1.5,2,2.5,3,3.5, 4.5,5,5.5,6,6.5, 7.5)
errbar(xSRpop, SRpop.M, SRpop.M + SRpop.SE, SRpop.M - SRpop.SE, 
       pch = c(16, 16,17,18,17,18, 16,17,18,17,18, 16), cex = c(3, 3,3,3.5,3,3.5, 3,3,3.5,3,3.5, 3), 
       lwd = 3, add = T)
#Add lines
abline(h = 0.5, lty = 2)
#AXIS
axis(1, at = c(0.5, 1.5,2,2.5,3,3.5, 4.5,5,5.5,6,6.5, 7.5), cex.axis = 1.2,
     labels = c("wt", "wt", expression("L"["X"]), expression("L"["Y"]), expression("O"["X"]), expression("O"["Y"]),
                "wt", expression("I"["X"]), expression("I"["Y"]), expression("D"["X"]), expression("D"["Y"]), "wt"))
#And add text on top
mtext(expression("LH"["M"]), side = 1, line = 3, at = 0.5, cex = 1.5)
mtext(expression(italic("Innisfail")), side = 1, line = 3, at = 2.5, cex = 1.5)
mtext(expression(italic("Odder")), side = 1, line = 3, at = 5.5, cex = 1.5)
mtext(expression(italic("Dahomey")), side = 1, line = 3, at = 7.5, cex = 1.5)



### PLOT TREATMENT ###

#Add fitted values
SR.data$fittedTreat <- fitted(model.SRt)

#Caluclate mean
SRtreat.M <- tapply(SR.data$fittedTreat, SR.data$treatment, mean)
#SE
SRtreat.SE <- (summary(model.SRt)$coefficients[,2])[-4]


#Plot
par(mar = c(3, 5, 2, 2))
plot(NULL, xlim = c(0.5, 3.5), xlab = "", xaxt = "n", ylim = c(0.3, 0.7),
     ylab = "Proportion of male offspring", cex.axis = 1.2, cex.lab = 1.5, las = 1)
#Add points
stripchart(SR.data$prop_male ~ SR.data$treatment, add = T, vertical = T, 
           method = "jitter", pch = c(16, 17, 18), cex = 1.5, col = "gray80")
#Add errorbars
xSRtreat <- c(1, 2, 3)
errbar(xSRtreat, SRtreat.M, SRtreat.M + SRtreat.SE, SRtreat.M - SRtreat.SE, 
       pch = c(16, 17, 18), cex = c(3, 3, 3.5), lwd = 2.5, add = T)
#Add lines
abline(h = 0.5, lty = 2)
#AXIS
axis(1, at = seq (1, 3, by = 1), cex.axis = 1.5, 
     labels = c("Wild-type", expression(italic("Novel X")), expression(italic("Novel Y"))))



