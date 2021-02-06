################################################################################################
############################### EGG-TO-ADULT OFFSPRING VIABILITY ###############################
################################################################################################

#Set up environment
library(car)
library(Hmisc)

#Read in csv file with data
EtA.data <- read.table(file = "EggToAdultViability.csv", header = T, sep = ",")


##########################################  STATISTIC  #########################################


### CALCULATE THE PROPORTION OF LIVE OFFSPRING ###
EtA.data$prop_eclose <- EtA.data$eclosed / 100


##############################
######### POPULATION #########
##############################


### LINEAT MODEL ###

#Linear model testing if population has a significant effect on the offspring survival with block
model.EtAp <- lm(prop_eclose ~ population + block, data = EtA.data)
#ANOVA
anova(model.EtAp, test = "F")
#Population is significant, P = 2.777e-06
#Block is significant, P = 0.0002801


#Homogeneity of variances
leveneTest(EtA.data$prop_eclose, EtA.data$population)
#No significant, assumption of homogeneity of variances is met, can use ANOVA

#Test the model by looking at the residuls
resid.EtAp <- residuals(model.EtAp)
hist(resid.EtAp)
shapiro.test(resid.EtAp)
qqnorm(resid.EtAp)
qqline(resid.EtAp)
plot(resid.EtAp)
#The residuls are more or less normally distrubuted, meaning that the model is valid 



#############################
######### TREATMENT #########
#############################


### LINEAT MODEL ###

#Linear mixed model testing if treatment has a significant effect on offspring survival with block
model.EtAt <- lm(prop_eclose ~ treatment + block, data = EtA.data)
#ANOVA
anova(model.EtAt, test = "F")
#Treatment is significant, P = 0.004107
#Block is significant, P = 0.001023


#Homogeneity of variances
leveneTest(EtA.data$prop_eclose, EtA.data$treatment)
#No significant, assumption of homogeneity of variances is met, can use ANOVA

#Test the model by looking at the residuls
resid.EtAt <- residuals(model.EtAt)
hist(resid.EtAt)
shapiro.test(resid.EtAt)
qqnorm(resid.EtAt)
qqline(resid.EtAt)
plot(resid.EtAt)
#The residuls are more or less normally distrubuted, meaning that the model is valid


#I do a post hoc test to see which treaments are significantly different
RESULT.EtAt <- aov(model.EtAt)
TukeyHSD(RESULT.EtAt)
#Novel X is significant different from the other two



#########################################  PLOT DATA  #########################################


#Create new dataframe for plotting
EtAplot <- EtA.data[c(1:2, 6)]
#Remove NAs
EtAplot <- EtAplot[which(is.na(EtA.data$prop_eclose) == F),]


### PLOT POPULATION ###

#Add fitted values
EtAplot$fittedPop <- fitted(model.EtAp)

#Caluclate mean
EtApop.M <- tapply(EtAplot$fittedPop, EtAplot$population, mean)
#SE
EtApop.SE <- (summary(model.EtAp)$coefficients[,2])[-13]

# Plot
par(mar = c(6, 5, 2, 2))
plot(NULL, xlim = c(0, 8), xlab = "", xaxt = "n", ylim = c(0.2, 1),
     ylab = "Proportion of eclosed offspring", cex.axis = 1.2, cex.lab = 1.5, las = 1)
#Add points
stripchart(EtAplot$prop_eclose ~ EtAplot$population, add = T, vertical = T,
           method = "jitter", pch = c(16, 16,17,18,17,18, 16,17,18,17,18, 16), 
           cex = 1.5, col = "gray80", at = c(0.5, 1.5,2,2.5,3,3.5, 4.5,5,5.5,6,6.5, 7.5))
#Add errorbars
xEtApop <- c(0.5, 1.5,2,2.5,3,3.5, 4.5,5,5.5,6,6.5, 7.5)
errbar(xEtApop, EtApop.M, EtApop.M + EtApop.SE, EtApop.M - EtApop.SE, 
       pch = c(16, 16,17,18,17,18, 16,17,18,17,18, 16), cex = c(3, 3,3,3.5,3,3.5, 3,3,3.5,3,3.5, 3), 
       lwd = 3, add = T)
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
EtAplot$fittedTreat <- fitted(model.EtAt)

#Caluclate mean
EtAtreat.M <- tapply(EtAplot$fittedTreat, EtAplot$treatment, mean)
#SE
EtAtreat.SE <- (summary(model.EtAt)$coefficients[c(1:3), 2])


# Plot
par(mar = c(3, 5, 2, 2))
plot(NULL, xlim = c(0.5, 3.5), xlab = "", xaxt = "n", ylim = c(0.2, 1.02),
     ylab = "Proportion of eclosed offspring", cex.axis = 1.2, cex.lab = 1.5, las = 1)
#Add points
stripchart(EtAplot$prop_eclose ~ EtAplot$treatment, add = T, vertical = T, 
           method = "jitter", pch = c(16, 17, 18), cex = 1.5, col = "gray80")
#Add errorbars
xEtAtreat <- c(1, 2, 3)
errbar(xEtAtreat, EtAtreat.M, EtAtreat.M + EtAtreat.SE, EtAtreat.M - EtAtreat.SE, 
       pch = c(16, 17, 18), cex = c(3, 3, 3.5), lwd = 2.5, add = T)
#AXIS
axis(1, at = seq (1, 3, by = 1), cex.axis = 1.5, 
     labels = c("Wild-type", expression(italic("Novel X")), expression(italic("Novel Y"))))
#Novel X is different
mtext("a", side = 3, line = -2, at = 1, cex = 1.7)
mtext("b", side = 3, line = -2, at = 2, cex = 1.7)
mtext("a", side = 3, line = -2, at = 3, cex = 1.7)


