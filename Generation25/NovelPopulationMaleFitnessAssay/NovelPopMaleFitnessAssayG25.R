################################################################################################
####################### NOVEL POPULATION MALE FITNESS ASSAY GENERATION 25 ######################
################################################################################################

#Set up environment
library(car)
library(Hmisc)
library(lme4)
library(lmerTest)

#Read in csv file with data
NPFAg25.data <- read.table(file = "NovelPopMaleFitnessAssayG25.csv", h = T, sep = ",")


##########################################  STATISTIC  #########################################


### CALCULATE RELATIVE FITNESS ###

#Calculate the proportion of red eyed offspring
NPFAg25.data$prop_red <- NPFAg25.data$red / NPFAg25.data$total
#Now divid each proportion red by 5 to get the number for each male
NPFAg25.data$prop_red_male <- NPFAg25.data$prop_red / 5
#Find maxium
maxNPFAg25 <- max(NPFAg25.data$prop_red_male, na.rm = T)
#Calculate relative fitness by dividing each proportion by the maximum
NPFAg25.data$relative_fit <- NPFAg25.data$prop_red_male / maxNPFAg25



##############################
######### POPULATION #########
##############################


### LINEAT MODEL ###

#Linear mixed model testing if population has a significant effect on the relative fitness
# with block and replicate population
model.NPFAg25p <- lmer(relative_fit ~ population + block + (1|population:rep_population), data = NPFAg25.data)
#ANOVA
anova(model.NPFAg25p, test = "F")
#Not significant, P = 0.7131
#Block is significant, P = 3.255e-07
ranova(model.NPFAg25p)
#Nested factor is not significant, P = 1


#Test if there is homogeneity of variances
leveneTest(NPFAg25.data$relative_fit, NPFAg25.data$population)
#Non significant, assumption of homogeneity of variances is met. Can use ANOVA

#Test the model by looking at the residuls
resid.NPFAg25p <- residuals(model.NPFAg25p)
hist(resid.NPFAg25p)
shapiro.test(resid.NPFAg25p)
qqnorm(resid.NPFAg25p)
qqline(resid.NPFAg25p)
plot(resid.NPFAg25p)
#The residuls are normally distrubuted, meaning that the model is valid



#############################
######### TREATMENT #########
#############################


### LINEAT MODEL ###

#Linear mixed model testing if treatment has a significant effect on the relative fitness with block 
#and replicate population
model.NPFAg25t <- lmer(relative_fit ~ treatment + block + (1|treatment:rep_population), data = NPFAg25.data)
#ANOVA
anova(model.NPFAg25t, test = "F")
#Not significant, P = 0.8451
#Block is significant, P = 2.647e-07
ranova(model.NPFAg25t)
#Nested factore is not significant, P = 1


#Test if there is homogeneity of variances
leveneTest(NPFAg25.data$relative_fit, NPFAg25.data$treatment)
#Non significant, assumption of homogeneity of variances is met. Can use ANOVA

#Test the model by looking at the residuls
resid.NPFAg25t <- residuals(model.NPFAg25t)
hist(resid.NPFAg25t)
shapiro.test(resid.NPFAg25t)
qqnorm(resid.NPFAg25t)
qqline(resid.NPFAg25t)
plot(resid.NPFAg25t)
#The residuls are normally distrubuted, meaning that the model is valid



#########################################  PLOT DATA  #########################################


#Create new dataframe for plotting
NPFAg25plot <- NPFAg25.data[c(2:3, 10)]
#Remove NAs
NPFAg25plot <- NPFAg25plot[which(is.na(NPFAg25plot$relative_fit) == F),]


### PLOT POPULATION ###

#Add fitted values
NPFAg25plot$fittedPop <- fitted(model.NPFAg25p)

#Caluclate mean
NPFAg25pop.M <- tapply(NPFAg25plot$fittedPop, NPFAg25plot$population, mean)
#SE
NPFAg25pop.SE <- (summary(model.NPFAg25p)$coefficients[,2])[-13]


#Plot
par(mar = c(6, 5, 2, 2))
plot(NULL, xlim = c(0, 8), xlab = "", xaxt = "n", ylim = c(0, 1), 
     ylab = "Relative fitness", cex.axis = 1.2, cex.lab = 1.5, las = 1)
#Add points
stripchart(NPFAg25plot$relative_fit ~ NPFAg25plot$population, add = T, vertical = T,
           method = "jitter", pch = c(16, 16,17,18,17,18, 16,17,18,17,18, 16), 
           cex = 1.5, col = "gray80", at = c(0.5, 1.5,2,2.5,3,3.5, 4.5,5,5.5,6,6.5, 7.5))
#Add errorbars
xNPFAg25pop <- c(0.5, 1.5,2,2.5,3,3.5, 4.5,5,5.5,6,6.5, 7.5)
errbar(xNPFAg25pop, NPFAg25pop.M, NPFAg25pop.M + NPFAg25pop.SE, NPFAg25pop.M - NPFAg25pop.SE, 
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
NPFAg25plot$fittedTreat <- fitted(model.NPFAg25t)

#Caluclate mean
NPFAg25treat.M <- tapply(NPFAg25plot$fittedTreat, NPFAg25plot$treatment, mean)
#SE
NPFAg25treat.SE <- (summary(model.NPFAg25t)$coefficients[,2])[-4]


#Plot
par(mar = c(3, 5, 2, 2))
plot(NULL, xlim = c(0.5, 3.5), xlab = "", xaxt = "n", ylim = c(0, 1),
     ylab = "Relative fitness", cex.axis = 1.2, cex.lab = 1.5, las = 1)
#Add points
stripchart(NPFAg25plot$relative_fit ~ NPFAg25plot$treatment, add = T, vertical = T, 
           method = "jitter", pch = c(16, 17, 18), cex = 1.5, col = "gray80")
#Add errorbars
xNPFAg25treat <- c(1, 2, 3)
errbar(xNPFAg25treat, NPFAg25treat.M, NPFAg25treat.M + NPFAg25treat.SE, NPFAg25treat.M - NPFAg25treat.SE, 
       pch = c(16, 17, 18), cex = c(3, 3, 3.5), lwd = 2.5, add = T)
#AXIS
axis(1, at = seq (1, 3, by = 1), cex.axis = 1.5, 
     labels = c("Wild-type", expression(italic("Novel X")), expression(italic("Novel Y"))))


