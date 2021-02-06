################################################################################################
################################ MALE EFFECT ON FEMALE FECUNDITY ###############################
################################################################################################

#Set up environment
library(car)
library(Hmisc)

#Read in csv file with data
MEFF.data <- read.table(file = "MaleEffectOnFemaleFecundity.csv", header = T, sep = ",")


##########################################  STATISTIC  #########################################


##############################
######### POPULATION #########
##############################


### LINEAT MODEL ###

#Linear model, testing if population has a significant effect on fecundity
model.MEFFp <- lm(egg ~ population, data = MEFF.data)
#ANOVA
anova(model.MEFFp, test = "F")
#Significant, P = 0.02807


#Test if there is homogeneity of variances
leveneTest(MEFF.data$egg, MEFF.data$population)
#Non significant, assumption of homogeneity of variances is met. Can use ANOVA


#Test the model by looking at the residuls
resid.MEFFp <- residuals(model.MEFFp)
hist(resid.MEFFp)
shapiro.test(resid.MEFFp)
qqnorm(resid.MEFFp)
qqline(resid.MEFFp)
plot(resid.MEFFp)
#The residuls are more or less normally distrubuted, meaning that the model is valid



#############################
######### TREATMENT #########
#############################


### LINEAT MODEL ###

#Linear model, testing if treament has a significant effect on fecundity
model.MEFFt <- lm(egg ~ treatment, data = MEFF.data)
#ANOVA
anova(model.MEFFt, test = "F")
#Not significant, P = 0.178


#Test if there is homogeneity of variances
leveneTest(MEFF.data$egg, MEFF.data$treatment)
#Non significant, assumption of homogeneity of variances is met. Can use ANOVA

#Test the model by looking at the residuls
resid.MEFFt <- residuals(model.MEFFt)
hist(resid.MEFFt)
shapiro.test(resid.MEFFt)
qqnorm(resid.MEFFt)
qqline(resid.MEFFt)
plot(resid.MEFFt)
#The residuls are almost normally distrubuted, meaning that the model is valid 



#########################################  PLOT DATA  #########################################


### PLOT POPULATION ###

#Add fitted values
MEFF.data$fittedPop <- fitted(model.MEFFp)

#Caluclate mean
MEFFpop.M <- tapply(MEFF.data$fittedPop, MEFF.data$population, mean)
#SE
MEFFpop.SE <- (summary(model.MEFFp)$coefficients[,2])


#Plot
par(mar = c(6, 5, 2, 2))
plot(NULL, xlim = c(0, 9), xlab = "", xaxt = "n", ylim = c(10, 90), yaxt = "n",
     ylab = "Number of Eggs", cex.axis = 1.2, cex.lab = 1.5, las = 1)
#Add points
stripchart(MEFF.data$egg ~ MEFF.data$population, add = T, vertical = T,
           method = "jitter", pch = c(16,18, 16,17,18,17,18, 16,17,18,17,18, 16,18), 
           cex = 1.5, col = "gray80", at = c(0.5,1, 2,2.5,3,3.5,4, 5,5.5,6,6.5,7, 8,8.5))
#Add errorbars
xMEFFpop <- c(0.5,1, 2,2.5,3,3.5,4, 5,5.5,6,6.5,7, 8,8.5)
errbar(xMEFFpop, MEFFpop.M, MEFFpop.M + MEFFpop.SE, MEFFpop.M - MEFFpop.SE, 
       pch = c(16,18, 16,17,18,17,18, 16,17,18,17,18, 16,18), cex = c(3,3.5, 3,3,3.5,3,3.5, 3,3,3.5,3,3.5, 3,3.5), 
       lwd = 3, add = T)
#AXIS
axis(1, at = c(0.5,1, 2,2.5,3,3.5,4, 5,5.5,6,6.5,7, 8,8.5), cex.axis = 1.2,
     labels = c("wt", expression("I"["Y"]),
                "wt", expression("L"["X"]), expression("L"["Y"]), expression("O"["X"]), expression("O"["Y"]),
                "wt", expression("I"["X"]), expression("I"["Y"]), expression("D"["X"]), expression("D"["Y"]),
                "wt", expression("O"["Y"])))
axis(2, at = seq(10, 90, 10),  cex.axis = 1.2, las = 1)
#And add text on top
mtext(expression("LH"["M"]), side = 1, line = 3, at = 0.75, cex = 1.5)
mtext(expression(italic("Innisfail")), side = 1, line = 3, at = 3, cex = 1.5)
mtext(expression(italic("Odder")), side = 1, line = 3, at = 6, cex = 1.5)
mtext(expression(italic("Dahomey")), side = 1, line = 3, at = 8.25, cex = 1.5)



### PLOT TREATMENT ###

#Add fitted values
MEFF.data$fittedTreat <- fitted(model.MEFFt)

#Caluclate mean
MEFFtreat.M <- tapply(MEFF.data$fittedTreat, MEFF.data$treatment, mean)
#SE
MEFFtreat.SE <- (summary(model.MEFFt)$coefficients[,2])


#Plot
par(mar = c(3, 5, 2, 2))
plot(NULL, xlim = c(0.5, 3.5), xlab = "", xaxt = "n", ylim = c(10, 90), yaxt = "n", 
     ylab = "Number of Eggs", cex.lab = 1.5)
#Add points
stripchart(MEFF.data$egg ~ MEFF.data$treatment, add = T, vertical = T, 
           method = "jitter", pch = c(16, 17, 18), cex = 1.5, col = "gray80")
#Add errorbars
xMEFFtreat <- c(1, 2, 3)
errbar(xMEFFtreat, MEFFtreat.M, MEFFtreat.M + MEFFtreat.SE, MEFFtreat.M - MEFFtreat.SE, 
       pch = c(16, 17, 18), cex = c(3, 3, 3.5), lwd = 2.5, add = T)
#AXIS
axis(1, at = seq (1, 3, by = 1), cex.axis = 1.5, 
     labels = c("Wild-type", expression(italic("Novel X")), expression(italic("Novel Y"))))
axis(2, at = seq(10, 90, 10),  cex.axis = 1.2, las = 1)

