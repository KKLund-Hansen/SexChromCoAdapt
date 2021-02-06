################################################################################################
################################### SEX RATIO, GENERATION 25 ###################################
################################################################################################

#Set up environment
library(car)
library(Hmisc)
library(lme4)
library(lmerTest)
library(multcomp)

#Read in csv file with data
SRg25.data <- read.table(file = "SexRatioG25.csv", header = T, sep = ",")


##########################################  STATISTIC  #########################################


### CALCULATE THE PROPORTION OF MALE OFFSPRING ###
SRg25.data$prop_male <- SRg25.data$male / SRg25.data$total


##############################
######### POPULATION #########
##############################


### LINEAT MODEL ###

#Linear mixed model testing if population has a significant effect on the proportion of males with block
model.SRg25p <- lmer(prop_male ~ population + block + (1|population:rep_population), data = SRg25.data)
#ANOVA
anova(model.SRg25p, test = "F")
#Not significant, P = 0.4122
#Block is not significant, P = 0.8611
ranova(model.SRg25p)
#Nested factor is not significant, P = 0.8623


#Homogeneity of variances
leveneTest(SRg25.data$prop_male, SRg25.data$population)
#Non significant, assumption of homogeneity of variances is met. Can use ANOVA

#Test the model by looking at the residuls
resid.SRg25p <- residuals(model.SRg25p)
hist(resid.SRg25p)
shapiro.test(resid.SRg25p)
qqnorm(resid.SRg25p)
qqline(resid.SRg25p)
plot(resid.SRg25p)
#The residuls are normally distrubuted, meaning that the model is valid



#############################
######### TREATMENT #########
#############################


### LINEAT MODEL ###

#Linear mixed model testing if treatment has a significant effect on the proportion of males with block
model.SRg25t <- lmer(prop_male ~ treatment + block + (1|treatment:rep_population), data = SRg25.data)
#ANOVA
anova(model.SRg25t, test = "F")
#Significant, P = 0.009219
#Block is not significant, P = 0.820917
ranova(model.SRg25t)
# Nested facto is not significant, P = 1 


#Homogeneity of variances
leveneTest(SRg25.data$prop_male, SRg25.data$treatment)
#Non significant, assumption of homogeneity of variances is met. Can use ANOVA

#Test the model by looking at the residuls
resid.SRg25t <- residuals(model.SRg25t)
hist(resid.SRg25t)
shapiro.test(resid.SRg25t)
qqnorm(resid.SRg25t)
qqline(resid.SRg25t)
plot(resid.SRg25t)
#The residuls are normally distrubuted, meaning that the model is valid


#I do a post hoc test to see if any of the genotypes are significantly different from each other
RESULT.SRg25t <- glht(model.SRg25t, mcp(treatment = "Tukey"))
#Look at the results
summary(RESULT.SRg25t)
#Wt is significant different from novel Y


#########################################  PLOT DATA  #########################################


### PLOT POPULATION ###

#Add fitted values
SRg25.data$fittedPop <- fitted(model.SRg25p)

#Caluclate mean
SRg25pop.M <- tapply(SRg25.data$fittedPop, SRg25.data$population, mean)
#SE
SRg25pop.SE <- (summary(model.SRg25p)$coefficients[c(1:12),2])


#Plot
par(mar = c(6, 5, 2, 2))
plot(NULL, xlim = c(0, 8), xlab = "", xaxt = "n", ylim = c(0.3, 0.65), ylab = "Proportion of male offspring", 
     cex.axis = 1.2, cex.lab = 1.5, las = 1)
#Add points
stripchart(SRg25.data$prop_male ~ SRg25.data$population, add = T, vertical = T,
           method = "jitter", pch = c(16, 16,17,18,17,18, 16,17,18,17,18, 16), 
           cex = 1.5, col = "gray80", at = c(0.5, 1.5,2,2.5,3,3.5, 4.5,5,5.5,6,6.5, 7.5))
#Add errorbars
xSRg25pop <- c(0.5, 1.5,2,2.5,3,3.5, 4.5,5,5.5,6,6.5, 7.5)
errbar(xSRg25pop, SRg25pop.M, SRg25pop.M + SRg25pop.SE, SRg25pop.M - SRg25pop.SE, 
       pch = c(16, 16,17,18,17,18, 16,17,18,17,18, 16), cex = c(3, 3,3,3.5,3,3.5, 3,3,3.5,3,3.5, 3), 
       lwd = 3, add = T)
#Add line
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
SRg25.data$fittedTreat <- fitted(model.SRg25t)

#Caluclate mean
SRg25treat.M <- tapply(SRg25.data$fittedTreat, SRg25.data$treatment, mean)
#SE
SRg25treat.SE <- (summary(model.SRg25t)$coefficients[c(1:3),2])


#Plot
par(mar = c(3, 5, 2, 2))
plot(NULL, xlim = c(0.5, 3.5), xlab = "", xaxt = "n", ylim = c(0.3, 0.65), ylab = "Proportion of male offspring", 
     cex.axis = 1.2, cex.lab = 1.5, las = 1)
#Add points
stripchart(SRg25.data$prop_male ~ SRg25.data$treatment, add = T, vertical = T, 
           method = "jitter", pch = c(16, 17, 18), cex = 1.5, col = "gray80")
#Add errorbars
xSRg25treat <- c(1, 2, 3)
errbar(xSRg25treat, SRg25treat.M, SRg25treat.M + SRg25treat.SE, SRg25treat.M - SRg25treat.SE, 
       pch = c(16, 17, 18), cex = c(3, 3, 3.5), lwd = 2.5, add = T)
#Add line
abline(h = 0.5, lty = 2)
#AXIS
axis(1, at = seq (1, 3, by = 1), cex.axis = 1.5, 
     labels = c("Wild-type", expression(italic("Novel X")), expression(italic("Novel Y"))))
#Mark significant between the genotypes with letters
mtext("b", side = 3, line = -2, at = 1, cex = 1.5)
mtext("ab", side = 3, line = -2, at = 2, cex = 1.5)
mtext("a", side = 3, line = -2, at = 3, cex = 1.5)


