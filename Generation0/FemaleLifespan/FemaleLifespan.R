################################################################################################
############################# FEMALE LIFESPAN WITH MALE HARASSMENT #############################
################################################################################################

#Set up environment
library(survival)

#Read in the data
FL.data <- read.csv("FemaleLifespan.csv", header = T, sep = ",")


##########################################  STATISTIC  #########################################


#First create a surv object to use in future models
msurv.FL <- Surv(FL.data$age, FL.data$dead)


##############################
######### POPULATION #########
##############################


### CREATING SURVIVAL CURVES ###

#Model survival as an effect of population
fitFLp <- survfit(msurv.FL ~ FL.data$population)

#Look at the data
summary(fitFLp)

#Is there a significant difference in survival between populations?
survdiff(msurv.FL ~ FL.data$population)
#Yes, P = 1e-07



#############################
######### TREATMENT #########
#############################


### CREATING SURVIVAL CURVES ###

#Model survival as an effect of treatment
fitFLt <- survfit(msurv.FL ~ FL.data$treatment)

#Look at the data
summary(fitFLt)

#Is there a significant difference in survival between treatments?
survdiff(msurv.FL ~ FL.data$treatment)
#No, P = 0.1


#########################################  PLOT DATA  #########################################


### PLOT POPULATION ###

par(mar = c(5, 5, 2, 2))
plot(fitFLp, xlim = c(0, 45), xlab = "Days", ylim = c(0, 1), ylab = "Proportion of survival", lwd = 2.5,
     cex.axis = 1.2, cex.lab = 1.5, las = 1, 
     col = c("gray5", "gray10", "gray15", "gray20", "gray25", "gray30", "gray35",
             "gray40", "gray45", "gray50", "gray55", "gray60", "gray65", "gray70"))
legend("bottomleft", lty = 1, lwd = 2, 
       c(expression("LH"["M"]), expression("LH"["M"]-"I"["Y"]),
         expression(italic("Innisfail")), expression(italic("Inn")-"L"["X"]), expression(italic("Inn")-"L"["Y"]),
         expression(italic("Inn")-"O"["X"]), expression(italic("Inn")-"O"["Y"]),
         expression(italic("Odder")), expression(italic("Odd")-"I"["X"]), expression(italic("Odd")-"I"["Y"]),
         expression(italic("Odd")-"D"["X"]), expression(italic("Odd")-"D"["Y"]),
         expression(italic("Dahomey")), expression(italic("Dah")-"O"["Y"])),
       col = c("gray5", "gray10", "gray15", "gray20", "gray25", "gray30", "gray35",
               "gray40", "gray45", "gray50", "gray55", "gray60", "gray65", "gray70"))



### PLOT TREATMENT ###


par(mar = c(5, 5, 2, 2))
plot(fitFLt, xlim = c(0, 45), xlab = "Days", ylim = c(0, 1), ylab = "Proportion of survival", lwd = 2.5,
     cex.axis = 1.2, cex.lab = 1.5, las = 1, col = c("gray80", "gray65", "gray50"))
legend("topright", lty = 1, lwd = 2, c("Wild-type", expression(italic("Novel X")), expression(italic("Novel Y"))),
       col = c("gray80", "gray65", "gray50"))

