################################################################################################
############################ NOVEL XY POPULATION MALE FITNESS ASSAY ############################
################################################################################################

#Set up environment
library(car)
library(Hmisc)

#Read in csv file with data
NXYMFA.data <- read.table(file = "NovelXYMaleFitnessAssay.csv", header = T, sep = ",")


##########################################  STATISTIC  #########################################


###CALCULATE RELATIVE FITNESS###

#Calculate the proportion of red eyed offspring
NXYMFA.data$prop_red <- NXYMFA.data$red / NXYMFA.data$total
#Now divid each proportion red by 5 to get the number for each male
NXYMFA.data$prop_red_male <- NXYMFA.data$prop_red / 5
#Find maxium
maxNXYMFA <- max(NXYMFA.data$prop_red_male, na.rm = T)
#Calculate relative fecundity by dividing each proportion by the maximum
NXYMFA.data$relative_fit <- NXYMFA.data$prop_red_male / maxNXYMFA



##############################
######### POPULATION #########
##############################


### LINEAT MODEL ###

#Linear model, testing if population has a significant effect on the relative fitness
model.NXYMFAp <- lm(relative_fit ~ population, data = NXYMFA.data)
#ANOVA
anova(model.NXYMFAp, test = "F")
#Population is not significant, P = 0.1454


#Test if there is homogeneity of variances
leveneTest(NXYMFA.data$relative_fit, NXYMFA.data$population)
#Non significant, assumption of homogeneity of variances is met. Can use ANOVA

#Test the model by looking at the residuls
resid.NXYMFAp <- residuals(model.NXYMFAp)
hist(resid.NXYMFAp)
shapiro.test(resid.NXYMFAp)
qqnorm(resid.NXYMFAp)
qqline(resid.NXYMFAp)
plot(resid.NXYMFAp)
#The residuls are normally distrubuted, meaning that the model is valid 



#############################
######### TREATMENT #########
#############################


### LINEAT MODEL ###

#Linear model, testing if treatment has a significant effect on the relative fitness
model.NXYMFAt <- lm(relative_fit ~ treatment, data = NXYMFA.data)
#ANOVA
anova(model.NXYMFAt, test = "F")
#Treatment is not significant, P = 0.9814


#Test if there is homogeneity of variances
leveneTest(NXYMFA.data$relative_fit, NXYMFA.data$treatment)
#Non significant, assumption of homogeneity of variances is met. Can use ANOVA

#Test the model by looking at the residuls
resid.NXYMFAt <- residuals(model.NXYMFAt)
hist(resid.NXYMFAt)
shapiro.test(resid.NXYMFAt)
qqnorm(resid.NXYMFAt)
qqline(resid.NXYMFAt)
plot(resid.NXYMFAt)
#The residuls are normally distrubuted, meaning that the model is valid 




#########################################  PLOT DATA  #########################################


### PLOT POPULATION ###

#Add fitted values
NXYMFA.data$fittedPop <- fitted(model.NXYMFAp)

#Caluclate mean
NXYMFApop.M <- tapply(NXYMFA.data$fittedPop, NXYMFA.data$population, mean)
#SE
NXYMFApop.SE <- (summary(model.NXYMFAp)$coefficients[,2])


#Plot
par(mar = c(5, 5, 2, 2))
plot(NULL, xlim = c(0, 7), xlab = "", xaxt = "n", ylim = c(0, 1), ylab = "Relative fitness", 
     cex.axis = 1.2, cex.lab = 1.5, las = 1)
#Add points
stripchart(NXYMFA.data$relative_fit ~ NXYMFA.data$population, add = T, vertical = T, 
           method = "jitter", pch = c(16,15, 16,15,15, 16,15,15, 16,15), cex = 1.5, col = "gray80", 
           at = c(0.5,1, 2,2.5,3, 4,4.5,5, 6,6.5))
#Add errorbars
xNXYMFApop <- c(0.5,1, 2,2.5,3, 4,4.5,5, 6,6.5)
errbar(xNXYMFApop, NXYMFApop.M, NXYMFApop.M + NXYMFApop.SE, NXYMFApop.M - NXYMFApop.SE, 
       pch = c(16,15, 16,15,15, 16,15,15, 16,15), cex = 3, lwd = 3, add = T)
#AXIS
axis(1, at = c(0.5,1, 2,2.5,3, 4,4.5,5, 6,6.5), cex.axis = 1.5,
     labels = c("wt", expression("I"["XY"]), 
                "wt", expression("L"["XY"]), expression("O"["XY"]), 
                "wt", expression("I"["XY"]), expression("D"["XY"]), 
                "wt", expression("O"["XY"])))
#And add text
mtext(expression("LH"["M"]), side = 1, line = 3, at = 0.75, cex = 1.6)
mtext(expression(italic("Innisfail")), side = 1, line = 3, at = 2.5, cex = 1.6)
mtext(expression(italic("Odder")), side = 1, line = 3, at = 4.5, cex = 1.6)
mtext(expression(italic("Dahomey")), side = 1, line = 3, at = 6.25, cex = 1.6)



### PLOT GENOTYPE ###


#Add fitted values
NXYMFA.data$fittedTreat <- fitted(model.NXYMFAt)

#Caluclate mean
NXYMFAtreat.M <- tapply(NXYMFA.data$fittedTreat, NXYMFA.data$treatment, mean)
#SE
NXYMFAtreat.SE <- (summary(model.NXYMFAt)$coefficients[,2])


#Plot
par(mar = c(3, 5, 2, 2))
plot(NULL, xlim = c(0.5, 2.5), xlab = "", xaxt = "n", ylim = c(0, 1), ylab = "Relative fitness", 
     cex.axis = 1.2, cex.lab = 1.5, las = 1)
#Add points
stripchart(NXYMFA.data$relative_fit ~ NXYMFA.data$treatment, add = T, vertical = T, 
           method = "jitter", pch = c(16, 15), cex = 1.5, col = "gray80")
#Add errorbars
xNXYMFAtreat <- c(1, 2)
errbar(xNXYMFAtreat, NXYMFAtreat.M, NXYMFAtreat.M + NXYMFAtreat.SE, NXYMFAtreat.M - NXYMFAtreat.SE, 
       pch = c(16, 15), cex = 3, lwd = 3, add = T)
#AXIS
axis(1, at = seq (1, 2, by = 1), cex.axis = 1.5, labels = c("Wild-type", expression(italic("Novel XY"))))

