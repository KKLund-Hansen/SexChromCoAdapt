################################################################################################
############################# NOVEL POPULATION MALE FITNESS ASSAY ##############################
################################################################################################

#Set up environment
library(car)
library(Hmisc)

#Read in csv file with data
NPMFA.data <- read.table(file = "NovelPopMaleFitnessAssay.csv", h = T, sep = ",")


##########################################  STATISTIC  #########################################


### CALCULATE RELATIVE FITNESS ###

#Calculate the proportion of red eyed offspring
NPMFA.data$prop_red <- NPMFA.data$red / NPMFA.data$total
#Now divid each proportion red by 5 to get the number for each male
NPMFA.data$prop_red_male <- NPMFA.data$prop_red / 5
#Find maxium
maxNPMFA <- max(NPMFA.data$prop_red_male, na.rm = T)
#Calculate relative fitness by dividing each proportion by the maximum
NPMFA.data$relative_fit <- NPMFA.data$prop_red_male / maxNPMFA



##############################
######### POPULATION #########
##############################


### LINEAT MODEL ###

#Linear model testing if population has a significant effect on the relative fitness with block
model.NPMFAp <- lm(relative_fit ~ population + block, data = NPMFA.data)
#ANOVA
anova(model.NPMFAp, test = "F")
#Population is significant, P = 3.381e-10
#Block is not, P = 0.5507


#Test if there is homogeneity of variances
leveneTest(NPMFA.data$relative_fit, NPMFA.data$population)
#Non significant, assumption of homogeneity of variances is met

#Test the model by looking at the residuls
resid.NPMFAp <- residuals(model.NPMFAp)
hist(resid.NPMFAp)
shapiro.test(resid.NPMFAp)
qqnorm(resid.NPMFAp)
qqline(resid.NPMFAp)
plot(resid.NPMFAp)
#The residuls are normally distrubuted, meaning that the model is valid 


#I do a post hoc test to see which populations are significantly different
RESULT.NPMFAp <- aov(model.NPMFAp)
#Get the results
TukeyHSD(RESULT.NPMFAp)
#



#############################
######### TREATMENT #########
#############################


### LINEAT MODEL ###

#Linear model testing if treatment has a significant effect on the relative fitness with block
model.NPMFAt <- lm(relative_fit ~ treatment + block, data = NPMFA.data)
#ANOVA
anova(model.NPMFAt, test = "F")
#Treatment is significant, P = 0.0002494
#Block is not significant, P = 0.6866460


#Test if there is homogeneity of variances
leveneTest(NPMFA.data$relative_fit, NPMFA.data$treatment)
#Non significant, assumption of homogeneity of variances is met. Can use ANOVA

#Test the model by looking at the residuls
resid.NPMFAt <- residuals(model.NPMFAt)
hist(resid.NPMFAt)
shapiro.test(resid.NPMFAt)
qqnorm(resid.NPMFAt)
qqline(resid.NPMFAt)
plot(resid.NPMFAt)
#The residuls are normally distrubuted, meaning that the model is valid 


#I do a post hoc test to see which treaments are significantly different
RESULT.NPMFAt <- aov(model.NPMFAt)
TukeyHSD(RESULT.NPMFAt)
#Wild-type is significant different from the other two



#########################################  PLOT DATA  #########################################


#Create new dataframe for plotting
NPMFAplot <- NPMFA.data[c(1:3, 9)]
#Remove NAs
NPMFAplot <- NPMFAplot[which(is.na(NPMFAplot$relative_fit) == F),]


### PLOT POPULATION ###

#Add fitted values
NPMFAplot$fitP <- fitted(model.NPMFAp)

#Caluclate mean
NPMFApop.M <- tapply(NPMFAplot$fitP, NPMFAplot$population, mean)
#SE
NPMFApop.SE <- (summary(model.NPMFAp)$coefficients[,2])[-26]


#Plot
par(mar = c(6, 5, 2, 2))
plot(NULL, xlim = c(0, 15), xlab = "", xaxt = "n", ylim = c(0, 1), ylab = "Relative fitness", 
     cex.axis = 1.4, cex.lab = 1.6, las = 1)
#Add points
stripchart(NPMFAplot$relative_fit ~ NPMFAplot$population, add = T, vertical = T, 
           method = "jitter", pch = c(16, 17, 18, 17, 18), cex = 1.5, col = "gray80", 
           at = c(0.5,1,1.5,2,2.5, 3.5,4,4.5,5,5.5, 6.5,7,7.5,8,8.5, 9.5,10,10.5,11,11.5, 12.5,13,13.5,14,14.5))
#Add errorbars
xNPMFApop <- c(0.5,1,1.5,2,2.5, 3.5,4,4.5,5,5.5, 6.5,7,7.5,8,8.5, 9.5,10,10.5,11,11.5, 12.5,13,13.5,14,14.5)
errbar(xNPMFApop, NPMFApop.M, NPMFApop.M + NPMFApop.SE, NPMFApop.M - NPMFApop.SE, 
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
NPMFAplot$fitT <- fitted(model.NPMFAt)

#Caluclate mean
NPMFAtreat.M <- tapply(NPMFAplot$fitT, NPMFAplot$treatment, mean)
#SE
NPMFAtreat.SE <- (summary(model.NPMFAt)$coefficients[,2])[-4]


#Plot
par(mar = c(3, 5, 2, 2))
plot(NULL, xlim = c(0.5, 3.5), xlab = "", xaxt = "n", ylim = c(0, 1.05), ylab = "Relative fitness", 
     cex.axis = 1.2, cex.lab = 1.5, las = 1)
#Add points
stripchart(NPMFAplot$relative_fit ~ NPMFAplot$treatment, add = T, vertical = T, 
           method = "jitter", pch = c(16, 17, 18), cex = 1.5, col = "gray80")
#Add errorbars
xNPMFAtreat <- c(1, 2, 3)
errbar(xNPMFAtreat, NPMFAtreat.M, NPMFAtreat.M + NPMFAtreat.SE, NPMFAtreat.M - NPMFAtreat.SE, 
       pch = c(16, 17, 18), cex = c(3, 3, 3.5), lwd = 3, add = T)
#AXIS
axis(1, at = seq (1, 3, by = 1), cex.axis = 1.5, 
     labels = c("Wild-type", expression(italic("Novel X")), expression(italic("Novel Y"))))
#Mark significant between the treatment with letters
#wt is significant different from the other two, but they are not
mtext("b", side = 3, line = -1.5, at = 1, cex = 1.5)
mtext("a", side = 3, line = -1.5, at = 2, cex = 1.5)
mtext("a", side = 3, line = -1.5, at = 3, cex = 1.5)


