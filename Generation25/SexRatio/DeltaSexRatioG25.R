################################################################################################
################################### ΔSEX RATIO, GENERATION 25 ##################################
################################################################################################

#Set up environment
library(Hmisc)

#Read in csv file with data
DeltaSRg25.data <- read.table(file = "SexRatioG25.csv", h = T, sep = ",")


##########################################  STATISTIC  #########################################


### CALCULATE THE PROPORTION OF MALE OFFSPRING ###
DeltaSRg25.data$prop_male <- DeltaSRg25.data$male / DeltaSRg25.data$total


### ΔPROP MALE CALCULATIONS ###
#To test if the change of sex chromosomes is significantly from the wild type population, 
#we do a bootstrap model to generate CI. If theses don't overlap 0 there has been a significant change
Delta.SRg25 <- as.numeric(tapply(DeltaSRg25.data$prop_male, DeltaSRg25.data$population, mean, na.rm = T))
#View Delta.SRg25
Delta.SRg25


#ΔSexRatio Inn-Lx - Innisfail
Delta.SRg25[3] - Delta.SRg25[2]
# 0.0316835

#ΔSexRatio Inn-Ly - Innisfail
Delta.SRg25[4] - Delta.SRg25[2]
# 0.03380503

#ΔSexRatio Inn-Ox - Innisfail
Delta.SRg25[5] - Delta.SRg25[2]
# 0.01680445

#ΔSexRatio Inn-Oy - Innisfail
Delta.SRg25[6] - Delta.SRg25[2]
# 0.03523821

#ΔSexRatio Odd-Ix - Odder
Delta.SRg25[8] - Delta.SRg25[7]
# 0.03540998

#ΔSexRatio Odd-Iy - Odder
Delta.SRg25[9] - Delta.SRg25[7]
# 0.03370977

#ΔSexRatio Odd-Dx - Odder
Delta.SRg25[10] - Delta.SRg25[7]
# -0.001339183

#ΔSexRatio Odd-Dy - Odder
Delta.SRg25[11] - Delta.SRg25[7]
# 0.04708402


#TEST OF PROBABILITY OF SUCCESS. THE PROBABILITY OF POSITIVE VALUE
binom.test(7, 8, p = 0.5,  alternative = "two.sided")
#Not significant, P = 0.07031


#First we make new vector to collect the data
LxI <- numeric(10000)
LyI <- numeric(10000)
OxI <- numeric(10000)
OyI <- numeric(10000)
IxO <- numeric(10000)
IyO <- numeric(10000)
DxO <- numeric(10000)
DyO <- numeric(10000)

#Then we set up a bootstrap that resampels the data from 12 data points to calculate a new mean everytime for 10000 times
for (i in 1:10000){
  DATA <- do.call(rbind, lapply(split(DeltaSRg25.data, DeltaSRg25.data$population), function(x) x[sample(12, replace = T),])) 
  Delta.SRg25 <- as.numeric(tapply(DATA$prop_male, DATA$population, mean, na.rm = T))
  LxI[i] <- Delta.SRg25[3] - Delta.SRg25[2]
  LyI[i] <- Delta.SRg25[4] - Delta.SRg25[2]
  OxI[i] <- Delta.SRg25[5] - Delta.SRg25[2]
  OyI[i] <- Delta.SRg25[6] - Delta.SRg25[2]
  IxO[i] <- Delta.SRg25[8] - Delta.SRg25[7]
  IyO[i] <- Delta.SRg25[9] - Delta.SRg25[7]
  DxO[i] <- Delta.SRg25[10] - Delta.SRg25[7]
  DyO[i] <- Delta.SRg25[11] - Delta.SRg25[7]  }
#Run the calculation


#95% CI for LxI
mean(LxI)
# 0.03184215
mean(LxI) - (1.96 * sd(LxI))
# -0.01152818
mean(LxI) + (1.96 * sd(LxI))
# 0.07521249

#95% CI for LyI
mean(LyI)
# 0.03365769
mean(LyI) - (1.96 * sd(LyI))
# -0.005278361
mean(LyI) + (1.96 * sd(LyI))
# 0.07259374

#95% CI for OxI
mean(OxI)
# 0.01684987
mean(OxI) - (1.96 * sd(OxI))
# -0.02379212
mean(OxI) + (1.96 * sd(OxI))
# 0.05749186

#95% CI for OyI
mean(OyI)
# 0.03519208
mean(OyI) - (1.96 * sd(OyI))
# -0.001209829
mean(OyI) + (1.96 * sd(OyI))
# 0.07159399

#95% CI for IxO
mean(IxO)
# 0.03571266
mean(IxO) - (1.96 * sd(IxO))
# 0.0006376027
mean(IxO) + (1.96 * sd(IxO))
# 0.07078772

#95% CI for IyO
mean(IyO)
# 0.03413871
mean(IyO) - (1.96 * sd(IyO))
# -0.001359057
mean(IyO) + (1.96 * sd(IyO))
# 0.06963647

#95% CI for DxO
mean(DxO)
# -0.001358048
mean(DxO) - (1.96 * sd(DxO))
# -0.04594042
mean(DxO) + (1.96 * sd(DxO))
# 0.04322432

#95% CI for DyOe
mean(DyO)
# 0.04738098
mean(DyO) - (1.96 * sd(DyO))
# 0.01008802
mean(DyO) + (1.96 * sd(DyO))
# 0.08467395



#########################################  PLOT DATA  #########################################


#To be able to plot the bootstrap data as boxplots I create a new data frame with the data

#First I make a vector with the populations
population <- c(rep("aLHmX_Inn", 10000), rep("bLHmY_Inn", 10000), rep("cOddX_Inn", 10000), rep("dOddY_Inn", 10000),
                rep("eInnX_Odd", 10000), rep("fInnY_Odd", 10000), rep("gDahX_Odd", 10000), rep("hDahY_Odd", 10000))

#Then I collcet all the bootstrap data in a new vector
deltaProp_male <- c(LxI, LyI, OxI, OyI, IxO, IyO, DxO, DyO)

#Then it's all collceted in a new data frame
DSRg25 <- data.frame(population, deltaProp_male)

#And write it into a new file
write.csv(DSRg25, file = "DeltaSexRatioG25.csv")


#Read in csv file with data
DSRg25plot.data <- read.table(file = "DeltaSexRatioG25.csv", h = T, sep = ",")

#MEAN
meanDSRg25 <- tapply(DSRg25plot.data$deltaProp_male, DSRg25plot.data$population, mean)
#SD
sdDSRg25 <- tapply(DSRg25plot.data$deltaProp_male, DSRg25plot.data$population, sd)


#Plot
par(mar = c(6, 5, 2, 2))
#Plot errorbars
xDSRg25 <- c(0.5,1,1.5,2, 3,3.5,4,4.5)
errbar(xDSRg25, meanDSRg25, meanDSRg25 + (1.96 * sdDSRg25), meanDSRg25 - (1.96 * sdDSRg25), 
       xlim = c(0.3, 4.7), xlab = "", xaxt = "n", ylim = c(-0.05, 0.1), 
       ylab = expression(Delta~"Proportion of male offspring"), 
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
points(2, 0.1, pch = "*", bg = "black", cex = 1.5)
points(4.5, 0.1, pch = "*", bg = "black", cex = 1.5)


