################################################################################################
########################## ΔSPERM COMPETITION, OFFENCE, GENERATION 25 ##########################
################################################################################################

#Set up environment
library(Hmisc)

#Read in csv file with data
DeltaSCg25.data <- read.table(file = "SpermCompetitionG25.csv", h = T, sep = ",")


##########################################  STATISTIC  #########################################


### CALCULATE OFFSPRING PROPOTION ###
#Calculate the proportion of red eyed offspring
DeltaSCg25.data$prop_red <- DeltaSCg25.data$red / DeltaSCg25.data$total


### ΔPROP RED CALCULATIONS ###
#To test if the change of sex chromosomes is significantly from the wild type population, 
#we do a bootstrap model to generate CI. If theses don't overlap 0 there has been a significant change
Delta.SCg25 <- as.numeric(tapply(DeltaSCg25.data$prop_red, DeltaSCg25.data$population, mean, na.rm = T))
#View Delta.SCg25
Delta.SCg25


#ΔPROPRED Inn-Lx - Innisfail
Delta.SCg25[3] - Delta.SCg25[2]
# 0.06426653

#ΔPROPRED Inn-Ly - Innisfail
Delta.SCg25[4] - Delta.SCg25[2]
# 0.1153892

#ΔPROPRED Inn-Ox - Innisfail
Delta.SCg25[5] - Delta.SCg25[2]
# 0.05576563

#ΔPROPRED Inn-Oy - Innisfail
Delta.SCg25[6] - Delta.SCg25[2]
# 0.06791959

#ΔPROPRED Odd-Ix - Odder
Delta.SCg25[8] - Delta.SCg25[7]
# 0.0596039

#ΔPROPRED Odd-Iy - Odder
Delta.SCg25[9] - Delta.SCg25[7]
# 0.04749198

#ΔPROPRED Odd-Dx - Odder
Delta.SCg25[10] - Delta.SCg25[7]
# 0.08672839

#ΔPROPRED Odd-Dy - Odder
Delta.SCg25[11] - Delta.SCg25[7]
# 0.06270016


#TEST OF PROBABILITY OF SUCCESS. THE PROBABILITY OF POSITIVE VALUE
binom.test(8, 8, p = 0.5,  alternative = "two.sided")
#Significant, P = 0.007812


#First we make new vector to collect the data
LxI <- numeric(10000)
LyI <- numeric(10000)
OxI <- numeric(10000)
OyI <- numeric(10000)
IxO <- numeric(10000)
IyO <- numeric(10000)
DxO <- numeric(10000)
DyO <- numeric(10000)

#Then we set up a bootstrap that resampels the data from 20 data points to calculate a new mean everytime for 10000 times
for (i in 1:10000){
  DATA <- do.call(rbind, lapply(split(DeltaSCg25.data, DeltaSCg25.data$population), function(x) x[sample(20, replace = T),])) 
  Delta.SCg25 <- as.numeric(tapply(DATA$prop_red, DATA$population, mean, na.rm = T))
  LxI[i] <- Delta.SCg25[3] - Delta.SCg25[2]
  LyI[i] <- Delta.SCg25[4] - Delta.SCg25[2]
  OxI[i] <- Delta.SCg25[5] - Delta.SCg25[2]
  OyI[i] <- Delta.SCg25[6] - Delta.SCg25[2]
  IxO[i] <- Delta.SCg25[8] - Delta.SCg25[7]
  IyO[i] <- Delta.SCg25[9] - Delta.SCg25[7]
  DxO[i] <- Delta.SCg25[10] - Delta.SCg25[7]
  DyO[i] <- Delta.SCg25[11] - Delta.SCg25[7]  }
#Run the calculation


#95% CI for LxI
mean(LxI)
# 0.04690085
mean(LxI) - (1.96 * sd(LxI))
# -0.09983077
mean(LxI) + (1.96 * sd(LxI))
# 0.1936325

#95% CI for LyI
mean(LyI)
# 0.08769314
mean(LyI) - (1.96 * sd(LyI))
# -0.01947225
mean(LyI) + (1.96 * sd(LyI))
# 0.1948585

#95% CI for OxI
mean(OxI)
# 0.01603175
mean(OxI) - (1.96 * sd(OxI))
# -0.1350262
mean(OxI) + (1.96 * sd(OxI))
# 0.1670897

#95% CI for OyI
mean(OyI)
# 0.07267357
mean(OyI) - (1.96 * sd(OyI))
# -0.03773652
mean(OyI) + (1.96 * sd(OyI))
# 0.1830837

#95% CI for IxO
mean(IxO)
# 0.0323782
mean(IxO) - (1.96 * sd(IxO))
# -0.09621062
mean(IxO) + (1.96 * sd(IxO))
# 0.160967

#95% CI for IyO
mean(IyO)
# 0.01320945
mean(IyO) - (1.96 * sd(IyO))
# -0.1171842
mean(IyO) + (1.96 * sd(IyO))
# 0.1436031

#95% CI for DxO
mean(DxO)
# 0.1188002
mean(DxO) - (1.96 * sd(DxO))
# 0.02884807
mean(DxO) + (1.96 * sd(DxO))
# 0.2087523

#95% CI for DyOe
mean(DyO)
# 0.06762949
mean(DyO) - (1.96 * sd(DyO))
# -0.03942213
mean(DyO) + (1.96 * sd(DyO))
# 0.1746811



#########################################  PLOT DATA  #########################################


#To be able to plot the bootstrap data as boxplots I create a new data frame with the data

#First I make a vector with the populations
population <- c(rep("aLHmX_Inn", 10000), rep("bLHmY_Inn", 10000), rep("cOddX_Inn", 10000), rep("dOddY_Inn", 10000),
                rep("eInnX_Odd", 10000), rep("fInnY_Odd", 10000), rep("gDahX_Odd", 10000), rep("hDahY_Odd", 10000))

#Then I collcet all the bootstrap data in a new vector
deltaProp_red <- c(LxI, LyI, OxI, OyI, IxO, IyO, DxO, DyO)

#Then it's all collceted in a new data frame
DSCg25 <- data.frame(population, deltaProp_red)
#And write it into a new file
write.csv(DSCg25, file = "DeltaSpermCompetitionG25.csv")


#Read in csv file with data
DSCg25plot.data <- read.table(file = "DeltaSpermCompetitionG25.csv", h = T, sep = ",")

#MEAN
meanDSCg25 <- tapply(DSCg25plot.data$deltaProp_red, DSCg25plot.data$population, mean, na.rm = T)
#SD
sdDSCg25 <- tapply(DSCg25plot.data$deltaProp_red, DSCg25plot.data$population, sd, na.rm = T)


#Plot
par(mar = c(6, 5, 2, 2))
#Plot errorbars
xDSCg25 <- c(0.5,1,1.5,2, 3,3.5,4,4.5)
errbar(xDSCg25, meanDSCg25, meanDSCg25 + (1.96 * sdDSCg25), meanDSCg25 - (1.96 * sdDSCg25), 
       xlim = c(0.3, 4.7), xlab = "", xaxt = "n", ylim = c(-0.3, 0.3), 
       ylab = expression(Delta~"Proportion of offspring"), 
       cex.axis = 1.2, cex.lab = 1.5, las = 1,  pch = c(17, 18), cex = c(3, 3.5), lwd = 3)
#AXIS
axis(1, at = c(0.5,1,1.5,2, 3,3.5,4,4.5), cex.axis = 1.2,
     labels = c(expression("L"["X"]), expression("L"["Y"]), expression("O"["X"]), expression("O"["Y"]),
                expression("I"["X"]), expression("I"["Y"]), expression("D"["X"]), expression("D"["Y"])))
#Add line at 0
abline(h = 0, lty = 2, lwd = 2)
#And add text below
mtext(expression(italic("Innisfail")), side = 1, line = 3, at = 1.25, cex = 1.5)
mtext(expression(italic("Odder")), side = 1, line = 3, at = 3.75, cex = 1.5)
#Now add arrows to show significance
points(4, 0.29, pch = "*", bg = "black", cex = 1.5)


