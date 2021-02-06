################################################################################################
########################### SPERM COMPETITION, OFFENCE, GENERATION 25 ##########################
################################################################################################

#Set up environment
library(car)
library(Hmisc)
library(lme4)
library(lmerTest)

#Read in csv file with data
SCg25.data <- read.table(file = "SpermCompetitionG25.csv", header = T, sep = ",")


##########################################  STATISTIC  #########################################


### CALCULATE OFFSPRING PROPOTION ###

#Calculate the proportion of red eyed offspring
SCg25.data$prop_red <- SCg25.data$red / SCg25.data$total



##############################
######### POPULATION #########
##############################


### LINEAT MODEL ###

#Linear mixed model testing if population has a significant effect on sperm competition
model.SCg25p <- lmer(prop_red ~ population + (1|population:rep_population), data = SCg25.data)
#ANOVA
anova(model.SCg25p, test = "F")
#Population is not significant, P = 0.5842
ranova(model.SCg25p)
#Nested factor is not significant, P = 0.6776


#Test if there is homogeneity of variances
leveneTest(SCg25.data$prop_red, SCg25.data$population)
#Non significant, assumption of homogeneity of variances is met. Can use ANOVA

#Test the model by looking at the residuls
resid.SCg25p <- residuals(model.SCg25p)
hist(resid.SCg25p)
shapiro.test(resid.SCg25p)
qqnorm(resid.SCg25p)
qqline(resid.SCg25p)
plot(resid.SCg25p)
#The residuls are more or less normally distrubuted, meaning that the model is valid 



#############################
######### TREATMENT #########
#############################


### LINEAT MODEL ###

#Linear mixed model testing if treatment has a significant effect on sperm competition
model.SCg25t <- lmer(prop_red ~ treatment + (1|treatment:rep_population), data = SCg25.data)
#ANOVA
anova(model.SCg25t, test = "F")
#Treatment is almost significant, P = 0.06328
ranova(model.SCg25t)
#Nested factor is not significant, P = 1


#Test if there is homogeneity of variances
leveneTest(SCg25.data$prop_red, SCg25.data$treatment)
#Non significant, assumption of homogeneity of variances is met. Can use ANOVA

#Test the model by looking at the residuls
resid.SCg25t <- residuals(model.SCg25t)
hist(resid.SCg25t)
shapiro.test(resid.SCg25t)
qqnorm(resid.SCg25t)
qqline(resid.SCg25t)
plot(resid.SCg25t)
#The residuls are more or less normally distrubuted, meaning that the model is valid



#########################################  PLOT DATA  #########################################


#Create new dataframe for plotting
SCg25plot <- SCg25.data[c(1:4, 7)]
#Remove NAs
SCg25plot <- SCg25plot[which(is.na(SCg25.data$prop_red) == F),]


### PLOT POPULATION ###

#Add fitted values
SCg25plot$fittedPop <- fitted(model.SCg25p)

#Caluclate mean
SCg25pop.M <- tapply(SCg25plot$fittedPop, SCg25plot$population, mean)
#SE
SCg25pop.SE <- (summary(model.SCg25p)$coefficients[,2])


#Plot
par(mar = c(6, 5, 2, 2))
plot(NULL, xlim = c(0, 8), xlab = "", xaxt = "n", ylim = c(0, 1), ylab = "Proportion of offspring", 
     cex.axis = 1.2, cex.lab = 1.5, las = 1)
#Add points
stripchart(SCg25plot$prop_red ~ SCg25plot$population, add = T, vertical = T,
           method = "jitter", pch = c(16, 16,17,18,17,18, 16,17,18,17,18, 16), 
           cex = 1.5, col = "gray80", at = c(0.5, 1.5,2,2.5,3,3.5, 4.5,5,5.5,6,6.5, 7.5))
#Add errorbars
xSCg25pop <- c(0.5, 1.5,2,2.5,3,3.5, 4.5,5,5.5,6,6.5, 7.5)
errbar(xSCg25pop, SCg25pop.M, SCg25pop.M + SCg25pop.SE, SCg25pop.M - SCg25pop.SE, 
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
SCg25plot$fittedTreat <- fitted(model.SCg25t)

#Caluclate mean
SCg25treat.M <- tapply(SCg25plot$fittedTreat, SCg25plot$treatment, mean)
#SE
SCg25treat.SE <- (summary(model.SCg25t)$coefficients[,2])


#Plot
par(mar = c(3, 5, 2, 2))
plot(NULL, xlim = c(0.5, 3.5), xlab = "", xaxt = "n", ylim = c(0, 1), ylab = "Proportion of offspring", 
     cex.axis = 1.2, cex.lab = 1.5, las = 1)
#Add points
stripchart(SCg25plot$prop_red ~ SCg25plot$treatment, add = T, vertical = T, 
           method = "jitter", pch = c(16, 17, 18), cex = 1.5, col = "gray80")
#Add errorbars
xSCg25treat <- c(1, 2, 3)
errbar(xSCg25treat, SCg25treat.M, SCg25treat.M + SCg25treat.SE, SCg25treat.M - SCg25treat.SE, 
       pch = c(16, 17, 18), cex = c(3, 3, 3.5), lwd = 2.5, add = T)
#AXIS
axis(1, at = seq (1, 3, by = 1), cex.axis = 1.5, 
     labels = c("Wild-type", expression(italic("Novel X")), expression(italic("Novel Y"))))


