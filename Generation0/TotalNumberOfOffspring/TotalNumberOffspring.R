################################################################################################
################################### TOTAL NUMBER OF OFFSPRING ##################################
################################################################################################

#Set up environment
library(car)
library(Hmisc)

#Read in csv file with data
TNO.data <- read.table(file = "TotalNumberOffspring.csv", h = T, sep = ",")


##########################################  STATISTIC  #########################################


##############################
######### POPULATION #########
##############################


### LINEAT MODEL ###

#Linear model testing if population has a significant effect on total offspring number with block
model.TNOp <- lm(totaloffspring ~ population + block, data = TNO.data)
#ANOVA
anova(model.TNOp, test = "F")
#Not significant, P = 0.4000
#Block is not significant, P = 0.6963


#Test if there is homogeneity of variances
leveneTest(TNO.data$totaloffspring, TNO.data$population)
#Non significant, assumption of homogeneity of variances is met. Can use ANOVA

#Test the model by looking at the residuls
resid.TNOp <- residuals(model.TNOp)
hist(resid.TNOp)
shapiro.test(resid.TNOp)
qqnorm(resid.TNOp)
qqline(resid.TNOp)
plot(resid.TNOp)
#The residuls are normally distrubuted, meaning that the model is valid 



#############################
######### TREATMENT #########
#############################


### LINEAT MODEL ###

#Linear model testing if treatment has a significant effect on total offspring number with block
model.TNOt <- lm(totaloffspring ~ treatment + block, data = TNO.data)
#ANOVA
anova(model.TNOt, test = "F")
#Significant, P = 0.01752
#Block is not significant, P = 0.68941


#Test if there is homogeneity of variances
leveneTest(TNO.data$totaloffspring, TNO.data$treatment)
#Non significant, assumption of homogeneity of variances is met. Can use ANOVA

#Test the model by looking at the residuls
resid.TNOt <- residuals(model.TNOt)
hist(resid.TNOt)
shapiro.test(resid.TNOt)
qqnorm(resid.TNOt)
qqline(resid.TNOt)
#The residuls are normally distrubuted, meaning that the model is valid 


#I do a post hoc test to see which treaments are significantly different
RESULT.TNOt <- aov(model.TNOt)
TukeyHSD(RESULT.TNOt)
#Wt is significant different from novel X



#########################################  PLOT DATA  #########################################


#Create new dataframe for plotting
TNOplot <- TNO.data[c(1:2, 5)]
#Remove NAs
TNOplot <- TNOplot[which(is.na(TNO.data$totaloffspring) == F),]


### PLOT POPULATION ###

#Add fitted values
TNOplot$fitP <- fitted(model.TNOp)

#Caluclate mean
TNOpop.M <- tapply(TNOplot$fitP, TNOplot$population, mean)
#SE
TNOpop.SE <- (summary(model.TNOp)$coefficients[,2])[-26]


#Plot
par(mar = c(6, 5, 2, 2))
plot(NULL, xlim = c(0, 15), xlab = "", xaxt = "n", ylim = c(100, 800), ylab = "Total number of offspring", 
     cex.axis = 1.2, cex.lab = 1.5, las = 1)
#Add points
stripchart(TNOplot$totaloffspring ~ TNOplot$population, add = T, vertical = T, 
           method = "jitter", pch = c(16, 17, 18, 17, 18), cex = 1.5, col = "gray80", 
           at = c(0.5,1,1.5,2,2.5, 3.5,4,4.5,5,5.5, 6.5,7,7.5,8,8.5, 9.5,10,10.5,11,11.5, 12.5,13,13.5,14,14.5))
#Add errorbars
xTNOpop <- c(0.5,1,1.5,2,2.5, 3.5,4,4.5,5,5.5, 6.5,7,7.5,8,8.5, 9.5,10,10.5,11,11.5, 12.5,13,13.5,14,14.5)
errbar(xTNOpop, TNOpop.M, TNOpop.M + TNOpop.SE, TNOpop.M - TNOpop.SE, 
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
TNOplot$fitT <- fitted(model.TNOt)

#Caluclate mean
TNOtreat.M <- tapply(TNOplot$fitT, TNOplot$treatment, mean)
#SE
TNOtreat.SE <- (summary(model.TNOt)$coefficients[c(1:3),2])


#Plot
par(mar = c(3, 5, 2, 2))
plot(NULL, xlim = c(0.5, 3.5), xlab = "", xaxt = "n", ylim = c(100, 800), ylab = "Total number of offspring", 
     cex.axis = 1.2, cex.lab = 1.5, las = 1)
#Add points
stripchart(TNOplot$totaloffspring ~ TNOplot$treatment, add = T, vertical = T, 
           method = "jitter", pch = c(16, 17, 18), cex = 1.5, col = "gray80")
#Add errorbars
xTNOtreat <- c(1, 2, 3)
errbar(xTNOtreat, TNOtreat.M, TNOtreat.M + TNOtreat.SE, TNOtreat.M - TNOtreat.SE, 
       pch = c(16, 17, 18), cex = c(3, 3, 3.5), lwd = 2.5, add = T)
#AXIS
axis(1, at = seq (1, 3, by = 1), cex.axis = 1.5, 
     labels = c("Wild-type", expression(italic("Novel X")), expression(italic("Novel Y"))))
#Mark significant between the treatment with letters
#Wt is significant different from novel X
mtext("a", side = 3, line = -2, at = 1, cex = 1.5)
mtext("b", side = 3, line = -2, at = 2, cex = 1.5)
mtext("ab", side = 3, line = -2, at = 3, cex = 1.5)

