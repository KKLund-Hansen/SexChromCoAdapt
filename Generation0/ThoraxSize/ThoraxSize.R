################################################################################################
###################################### MALE THORAX LENGTH ######################################
################################################################################################

#Set up environment
library(car)
library(Hmisc)

#Read in csv file with data
TS.data <- read.table(file = "ThoraxSize.csv", header = T, sep = ",")


##########################################  STATISTIC  #########################################


##############################
######### POPULATION #########
##############################


### LINEAT MODEL ###

#Linear model, testing if population has a significant effect on thorax size
model.TSp <- lm(size_mm ~ population, data = TS.data)
#ANOVA
anova(model.TSp, test = "F")
#Significant, P < 2.2e-16


#Test if there is homogeneity of variances
leveneTest(TS.data$size_mm, TS.data$population)
#Significant, assumption of homogeneity of variances is not met. Can't use ANOVA

#Test the model by looking at the residuls
resid.TSp <- residuals(model.TSp)
hist(resid.TSp)
shapiro.test(resid.TSp)
qqnorm(resid.TSp)
qqline(resid.TSp)
plot(resid.TSp)
#The residuls are more or less normally distrubuted, meaning that the model is valid


#I do a post hoc test to see which populations are significantly different
RESULT.TSp <- aov(model.TSp)
#Get the results
TukeyHSD(RESULT.TSp)
#



#############################
######### TREATMENT #########
#############################


### LINEAT MODEL ###

#Linear model, testing if treatment has a significant effect on thorax size
model.TSt <- lm(size_mm ~ treatment, data = TS.data)
#ANOVA
anova(model.TSt, test = "F")
#Significant, P < 2.2e-16


#Test if there is homogeneity of variances
leveneTest(TS.data$size_mm, TS.data$treatment)
#Significant, assumption of homogeneity of variances is not met. Can't use ANOVA

#Test the model by looking at the residuls
resid.TSt <- residuals(model.TSt)
hist(resid.TSt)
shapiro.test(resid.TSt)
qqnorm(resid.TSt)
qqline(resid.TSt)
plot(resid.TSt)
#The residuls are more or less normally distrubuted, meaning that the model is valid


#Do a Tukey test
RESULT.TSt <- aov(model.TSt)
TukeyHSD(RESULT.TSt)$treatment
#Novel X is significnatly different from the others



#########################################  PLOT DATA  #########################################


### PLOT POPULATION ###

#Add fitted values
TS.data$fittedPop <- fitted(model.TSp)

#Caluclate mean
TSpop.M <- tapply(TS.data$fittedPop, TS.data$population, mean)
#SE
TSpop.SE <- (summary(model.TSp)$coefficients[,2])


#Plot
par(mar = c(6, 5, 2, 2))
plot(NULL, xlim = c(0, 15), xlab = "", xaxt = "n", ylim = c(0.6, 0.9), ylab = "Thorax length/mm", 
     cex.axis = 1.2, cex.lab = 1.5, las = 1)
#Add points
stripchart(TS.data$size_mm ~ TS.data$population, add = T, vertical = T, 
           method = "jitter", pch = c(16, 17, 18, 17, 18), cex = 1.5, col = "gray80", 
           at = c(0.5,1,1.5,2,2.5, 3.5,4,4.5,5,5.5, 6.5,7,7.5,8,8.5, 9.5,10,10.5,11,11.5, 12.5,13,13.5,14,14.5))
#Add errorbars
xTSpop <- c(0.5,1,1.5,2,2.5, 3.5,4,4.5,5,5.5, 6.5,7,7.5,8,8.5, 9.5,10,10.5,11,11.5, 12.5,13,13.5,14,14.5)
errbar(xTSpop, TSpop.M, TSpop.M + TSpop.SE, TSpop.M - TSpop.SE, 
       pch = c(16, 17, 18, 17, 18), cex = c(3, 3, 3.5, 3, 3.5), lwd = 3, add = T)
#AXIS
axis(1, at = c(0.5,1,1.5,2,2.5, 3.5,4,4.5,5,5.5, 6.5,7,7.5,8,8.5, 9.5,10,10.5,11,11.5, 12.5,13,13.5,14,14.5), 
     cex.axis = 1.5,
     labels = c("wt", expression("T"["X"]), expression("T"["Y"]), expression("I"["X"]), expression("I"["Y"]),
                "wt", expression("L"["X"]), expression("L"["Y"]), expression("O"["X"]), expression("O"["Y"]),
                "wt", expression("I"["X"]), expression("I"["Y"]), expression("D"["X"]), expression("D"["Y"]),
                "wt", expression("O"["X"]), expression("O"["Y"]), expression("T"["X"]), expression("T"["Y"]),
                "wt", expression("D"["X"]), expression("D"["Y"]), expression("L"["X"]), expression("L"["Y"]))) 
#And add text on top
mtext(expression("LH"["M"]), side = 1, line = 3, at = 1.5, cex = 1.6)
mtext(expression(italic("Innisfail")), side = 1, line = 3, at = 4.5, cex = 1.6)
mtext(expression(italic("Odder")), side = 1, line = 3, at = 7.5, cex = 1.6)
mtext(expression(italic("Dahomey")), side = 1, line = 3, at = 10.5, cex = 1.6)
mtext(expression(italic("Tasmania")), side = 1, line = 3, at = 13.5, cex = 1.6)



### PLOT TREATMENT ###

#Add fitted values
TS.data$fittedTreat <- fitted(model.TSt)

#Caluclate mean
TStreat.M <- tapply(TS.data$fittedTreat, TS.data$treatment, mean)
#SE
TStreat.SE <- (summary(model.TSt)$coefficients[,2])


#Plot
par(mar = c(3, 5, 2, 2))
plot(NULL, xlim = c(0.5, 3.5), xlab = "", xaxt = "n", ylim = c(0.6, 0.95), ylab = "Thorax length/mm", 
     cex.axis = 1.2, cex.lab = 1.5, las = 1)
#Add points
stripchart(TS.data$size_mm ~ TS.data$treatment, add = T, vertical = T, 
           method = "jitter", pch = c(16, 17, 18), cex = 1.5, col = "gray80")
#Add errorbars
xTStreat <- c(1, 2, 3)
errbar(xTStreat, TStreat.M, TStreat.M + TStreat.SE, TStreat.M - TStreat.SE, 
       pch = c(16, 17, 18), cex = c(3, 3, 3.5), lwd = 3, add = T)
#AXIS
axis(1, at = seq (1, 3, by = 1), cex.axis = 1.5, 
     labels = c("Wild-type", expression(italic("Novel X")), expression(italic("Novel Y"))))
#Mark significant between the treatment with letters
#Novel X is significant different from the other two, but they are not
mtext("b", side = 3, line = -2, at = 1, cex = 1.5)
mtext("a", side = 3, line = -2, at = 2, cex = 1.5)
mtext("b", side = 3, line = -2, at = 3, cex = 1.5)


