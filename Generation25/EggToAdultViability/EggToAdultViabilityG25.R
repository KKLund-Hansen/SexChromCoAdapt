################################################################################################
######################## EGG-TO-ADULT OFFSPRING VIABILITY, GENERATION 25 #######################
################################################################################################

#Set up environment
library(car)
library(Hmisc)
library(lme4)
library(lmerTest)
library(multcomp)

#Read in csv file with data
EtAg25.data <- read.table(file = "EggToAdultViabilityG25.csv", header = T, sep = ",")


##########################################  STATISTIC  #########################################


### CALCULATE THE PROPORTION OF ECLOSED OFFSPRING ###
EtAg25.data$prop_eclose <- EtAg25.data$eclosed / 100


##############################
######### POPULATION #########
##############################


### LINEAT MODEL ###

#Linear mixed model testing if population has a significant effect on the offspring survival with block
model.EtAg25p <- lmer(prop_eclose ~ population + block + (1|population:rep_population), data = EtAg25.data)
#ANOVA
anova(model.EtAg25p, test = "F")
#Not significant, P = 0.474963
#Block is significant, P = 0.003412
ranova(model.EtAg25p)
# Nested factor is significant, P = 0.01122


#Homogeneity of variances
leveneTest(EtAg25.data$prop_eclose, EtAg25.data$population)
#Significant, can't perhaps not use ANOVA

#Test the model by looking at the residuls
resid.EtAg25p <- residuals(model.EtAg25p)
hist(resid.EtAg25p)
shapiro.test(resid.EtAg25p)
qqnorm(resid.EtAg25p)
qqline(resid.EtAg25p)
plot(resid.EtAg25p)
#The residuls are normally distrubuted, meaning that the model is valid



#############################
######### TREATMENT #########
#############################


### LINEAT MODEL ###

#Linear mixed model testing if treatment has a significant effect on the offspring survival with block
model.EtAg25t <- lmer(prop_eclose ~ treatment + block + (1|treatment:rep_population), data = EtAg25.data)
#ANOVA
anova(model.EtAg25t, test = "F")
#Not significant, P = 0.0001467
#Block is significant, P = 1.717e-09
ranova(model.EtAg25t)
#Nested factor is not significant, P = 1


#Homogeneity of variances
leveneTest(EtAg25.data$prop_eclose, EtAg25.data$treatment)
#Significant, assumption of homogeneity of variances is met

#Test the model by looking at the residul
resid.EtAg25t <- residuals(model.EtAg25t)
hist(resid.EtAg25t)
shapiro.test(resid.EtAg25t)
qqnorm(resid.EtAg25t)
qqline(resid.EtAg25t)
plot(resid.EtAg25t)
#The residuls are more or less normally distrubuted, meaning that the model is valid


#I do a post hoc test to see if any of the genotypes are significantly different from each other
RESULT.EtAg25t <- glht(model.EtAg25t, mcp(treatment = "Tukey"))
#Look at the results
summary(RESULT.EtAg25t)
#Wt is significant different from novel Y



#########################################  PLOT DATA  #########################################


#Create new dataframe for plotting
EtAg25plot <- EtAg25.data[c(1:5, 7)]
#Remove NAs
EtAg25plot <- EtAg25plot[which(is.na(EtAg25.data$prop_eclose) == F),]


### PLOT POPULATION ###

#Add fitted values
EtAg25plot$fittedPop <- fitted(model.EtAg25p)

#Caluclate mean
EtAg25pop.M <- tapply(EtAg25plot$fittedPop, EtAg25plot$population, mean)
#SE
EtAg25pop.SE <- (summary(model.EtAg25p)$coefficients[c(1:12), 2])


#Plot
par(mar = c(6, 5, 2, 2))
plot(NULL, xlim = c(0, 8), xlab = "", xaxt = "n", ylim = c(0.6, 1), ylab = "Proportion of eclosed offspring", 
     cex.axis = 1.2, cex.lab = 1.5, las = 1)
#Add points
stripchart(EtAg25plot$prop_eclose ~ EtAg25plot$population, add = T, vertical = T,
           method = "jitter", pch = c(16, 16,17,18,17,18, 16,17,18,17,18, 16), 
           cex = 1.5, col = "gray80", at = c(0.5, 1.5,2,2.5,3,3.5, 4.5,5,5.5,6,6.5, 7.5))
#Add errorbars
xEtAg25pop <- c(0.5, 1.5,2,2.5,3,3.5, 4.5,5,5.5,6,6.5, 7.5)
errbar(xEtAg25pop, EtAg25pop.M, EtAg25pop.M + EtAg25pop.SE, EtAg25pop.M - EtAg25pop.SE, 
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
EtAg25plot$fittedTreat <- fitted(model.EtAg25t)

#Caluclate mean
EtAg25treat.M <- tapply(EtAg25plot$fittedTreat, EtAg25plot$treatment, mean)
#SE
EtAg25treat.SE <- (summary(model.EtAg25t)$coefficients[c(1:3), 2])


#Plot
par(mar = c(3, 5, 2, 2))
plot(NULL, xlim = c(0.5, 3.5), xlab = "", xaxt = "n", ylim = c(0.6, 1.1), ylab = "Proportion of eclosed offspring", 
     cex.axis = 1.2, cex.lab = 1.5, las = 1)
#Add points
stripchart(EtAg25plot$prop_eclose ~ EtAg25plot$treatment, add = T, vertical = T, 
           method = "jitter", pch = c(16, 17, 18), cex = 1.5, col = "gray80")
#Add errorbars
xEtAg25treat <- c(1, 2, 3)
errbar(xEtAg25treat, EtAg25treat.M, EtAg25treat.M + EtAg25treat.SE, EtAg25treat.M - EtAg25treat.SE, 
       pch = c(16, 17, 18), cex = c(3, 3, 3.5), lwd = 2.5, add = T)
#AXIS
axis(1, at = seq (1, 3, by = 1), cex.axis = 1.5, 
     labels = c("Wild-type", expression(italic("Novel X")), expression(italic("Novel Y"))))
#Mark significant between the genotypes with letters
mtext("b", side = 3, line = -2.5, at = 1, cex = 1.5)
mtext("ab", side = 3, line = -2.5, at = 2, cex = 1.5)
mtext("a", side = 3, line = -2.5, at = 3, cex = 1.5)


