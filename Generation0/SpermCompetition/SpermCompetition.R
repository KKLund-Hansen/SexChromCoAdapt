################################################################################################
################################## SPERM COMPETITION, OFFENCE ##################################
################################################################################################

#Set up environment
library(car)
library(Hmisc)

#Read in csv file with data
SC.data <- read.table(file = "SpermCompetition.csv", h = T, sep = ",")


##########################################  STATISTIC  #########################################


### REMOVE OUTLIERS ###
#Are there any outliers?
outlierSC <- SC.data$total[which((abs(SC.data$total - median(SC.data$total)) / mad(SC.data$total)) > 2)]
#Check the list
outlierSC
#The outlier number 8 is very different from the rest so we remove it
SC.data$total[SC.data$total %in% outlierSC[8]] <- NA
SC.data$red[is.na(SC.data$total)] <- NA


### CALCULATE OFFSPRING PROPOTION ###

#Calculate the proportion of red eyed offspring
SC.data$prop_red <- SC.data$red / SC.data$total


##############################
######### POPULATION #########
##############################


### LINEAT MODEL ###

#Linear model testing if population has a significant effect on sperm competition with block
model.SCp <- lm(prop_red ~ population + block, data = SC.data)
#ANOVA
anova(model.SCp, test = "F")
#Almost significant, P = 0.06330
#Block is not significant, P = 0.07623


#Test if there is homogeneity of variances
leveneTest(SC.data$prop_red, SC.data$population)
#Not significant, assumption of homogeneity of variances is met. Can use ANOVA

#Test the model by looking at the residuls
resid.SCp <- residuals(model.SCp)
hist(resid.SCp)
shapiro.test(resid.SCp)
qqnorm(resid.SCp)
qqline(resid.SCp)
plot(resid.SCp)
#The residuls are more or less normally distrubuted, meaning that the model is valid 



#############################
######### TREATMENT #########
#############################


### LINEAT MODEL ###

#Linear model testing if treatment has a significant effect on P1 with block
model.SCt <- lm(prop_red ~ treatment + block, data = SC.data)
#ANOVA
anova(model.SCt, test = "F")
#Treatment is significant, P = 0.02980
#Block is not significant, P = 0.08265


#Test if there is homogeneity of variances
leveneTest(SC.data$prop_red, SC.data$treatment)
#Not significant, assumption of homogeneity of variances is met. Can use ANOVA

#Test the model by looking at the residuls
resid.SCt <- residuals(model.SCt)
hist(resid.SCt)
shapiro.test(resid.SCt)
qqnorm(resid.SCt)
qqline(resid.SCt)
plot(resid.SCt)
#The residuls are more or less normally distrubuted, meaning that the model is valid


#I do a post hoc test to see which treatments are significantly different
RESULT.SCt <- aov(model.SCt)
TukeyHSD(RESULT.SCt)
#Novel Y is significant different from wild-type



#########################################  PLOT DATA  #########################################


#Create new dataframe for plotting
SCplot <- SC.data[c(1:2, 7)]
#Remove NAs
SCplot <- SCplot[which(is.na(SC.data$prop_red) == F),]


### PLOT POPULATION ###

#Add fitted values
SCplot$fittedPop <- fitted(model.SCp)

#Caluclate mean
SCpop.M <- tapply(SCplot$fittedPop, SCplot$population, mean)
#SE
SCpop.SE <- (summary(model.SCp)$coefficients[,2])[-13]


#Plot
par(mar = c(6, 5, 2, 2))
plot(NULL, xlim = c(0, 8), xlab = "", xaxt = "n", ylim = c(0.5, 1),
     ylab = "Proportion of offspring", cex.axis = 1.2, cex.lab = 1.5, las = 1)
#Add points
stripchart(SCplot$prop_red ~ SCplot$population, add = T, vertical = T,
           method = "jitter", pch = c(16, 16,17,18,17,18, 16,17,18,17,18, 16), 
           cex = 1.5, col = "gray80", at = c(0.5, 1.5,2,2.5,3,3.5, 4.5,5,5.5,6,6.5, 7.5))
#Add errorbars
xSCpop <- c(0.5, 1.5,2,2.5,3,3.5, 4.5,5,5.5,6,6.5, 7.5)
errbar(xSCpop, SCpop.M, SCpop.M + SCpop.SE, SCpop.M - SCpop.SE, 
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
SCplot$fittedTreat <- fitted(model.SCt)

#Caluclate mean
SCtreat.M <- tapply(SCplot$fittedTreat, SCplot$treatment, mean)
#SE
SCtreat.SE <- (summary(model.SCt)$coefficients[,2])[-4]


#Plot
par(mar = c(3, 5, 2, 2))
plot(NULL, xlim = c(0.5, 3.5), xlab = "", xaxt = "n", ylim = c(0.5, 1.05),
     ylab = "Proportion of offspring", cex.axis = 1.2, cex.lab = 1.5, las = 1)
#Add points
stripchart(SCplot$prop_red ~ SCplot$treatment, add = T, vertical = T, 
           method = "jitter", pch = c(16, 17, 18), cex = 1.5, col = "gray80")
#Add errorbars
xSCtreat <- c(1, 2, 3)
errbar(xSCtreat, SCtreat.M, SCtreat.M + SCtreat.SE, SCtreat.M - SCtreat.SE, 
       pch = c(16, 17, 18), cex = c(3, 3, 3.5), lwd = 2.5, add = T)
#AXIS
axis(1, at = seq (1, 3, by = 1), cex.axis = 1.5, 
     labels = c("Wild-type", expression(italic("Novel X")), expression(italic("Novel Y"))))
#Wt is significant different from novel Y
mtext("a", side = 3, line = -2, at = 1, cex = 1.7)
mtext("ab", side = 3, line = -2, at = 2, cex = 1.7)
mtext("b", side = 3, line = -2, at = 3, cex = 1.7)


