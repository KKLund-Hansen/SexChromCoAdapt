################################################################################################
############################### ΔMALE EFFECT ON FEMALE FECUNDITY ###############################
################################################################################################

#Set up environment
library(Hmisc)

#Read in csv file with data
deltaMEFF.data <- read.table(file = "MaleEffectOnFemaleFecundity.csv", header = T, sep = ",")


##########################################  STATISTIC  #########################################


### ΔNUMBEROFEGGS CALCULATIONS ###

#To test if the change of sex chromosomes is significantly from the wild type population, 
#we do a bootstrap model to generate CI. If theses don't overlap 0 there has been a significant change
Delta.MEFF <- as.numeric(tapply(deltaMEFF.data$egg, deltaMEFF.data$population, mean, na.rm = T))
#View Delta.MEFF
Delta.MEFF


#ΔNumberOfEggs LHm-Iy - LHm
Delta.MEFF[2] - Delta.MEFF[1]
# -14.42407

#ΔNumberOfEggs Inn-Lx - Innisfail
Delta.MEFF[4] - Delta.MEFF[3]
# -0.1888889

#ΔNumberOfEggs Inn-Ox - Innisfail
Delta.MEFF[5] - Delta.MEFF[3]
# -7.388889

#ΔNumberOfEggs Inn-Oy - Innisfail
Delta.MEFF[6] - Delta.MEFF[3]
# 1.596296

#ΔNumberOfEggs Odd-Ix - Odder
Delta.MEFF[8] - Delta.MEFF[7]
# -1.231481

#ΔNumberOfEggs Odd-Dx - Odder
Delta.MEFF[9] - Delta.MEFF[7]
# -4.559259

#NumberOfEggs Odd-Dy - Odder
Delta.MEFF[10] - Delta.MEFF[7]
# -2.444444

#ΔNumberOfEggs Dah-Oy - Dahomey
Delta.MEFF[12] - Delta.MEFF[11]
# 2.677778


#TEST OF PROBABILITY OF SUCCESS. THE PROBABILITY OF POSITIVE VALUE
binom.test(2, 8, p = 0.5,  alternative = "two.sided")
#Not significant, P = 0.2891


#First we make new vector to collect the data
IyL <- numeric(10000)
LxI <- numeric(10000)
OxI <- numeric(10000)
OyI <- numeric(10000)
IxO <- numeric(10000)
DxO <- numeric(10000)
DyO <- numeric(10000)
OyD <- numeric(10000)


#Then we set up a bootstrap that resampels the data from 9 data points to calculate a new mean everytime for 10000 times
for (i in 1:10000){
  DATA <- do.call(rbind, lapply(split(deltaMEFF.data, deltaMEFF.data$population), function(x) x[sample(9, replace = T),])) 
  Delta.MEFF <- as.numeric(tapply(DATA$egg, DATA$population, mean, na.rm = T))
  IyL[i] <- Delta.MEFF[2] - Delta.MEFF[1]
  LxI[i] <- Delta.MEFF[4] - Delta.MEFF[3]
  OxI[i] <- Delta.MEFF[5] - Delta.MEFF[3]
  OyI[i] <- Delta.MEFF[6] - Delta.MEFF[3]
  IxO[i] <- Delta.MEFF[8] - Delta.MEFF[7]
  DxO[i] <- Delta.MEFF[9] - Delta.MEFF[7]
  DyO[i] <- Delta.MEFF[10] - Delta.MEFF[7]
  OyD[i] <- Delta.MEFF[12] - Delta.MEFF[11]  }
#Run the calculation


#95% CI for IyL
mean(IyL)
# -14.37497
mean(IyL) - (1.96 * sd(IyL))
# -25.21863
mean(IyL) + (1.96 * sd(IyL))
# -3.531313

#95% CI for LxI
mean(LxI)
# -0.1659128
mean(LxI) - (1.96 * sd(LxI))
# -9.78784
mean(LxI) + (1.96 * sd(LxI))
# 9.456015


#95% CI for OxI
mean(OxI)
# -7.418902
mean(OxI) - (1.96 * sd(OxI))
# -16.20372
mean(OxI) + (1.96 * sd(OxI))
# 1.365919

#95% CI for OyI
mean(OyI)
# 1.678986
mean(OyI) - (1.96 * sd(OyI))
# -11.45485
mean(OyI) + (1.96 * sd(OyI))
# 14.81282

#95% CI for IxO
mean(IxO)
# -1.198037
mean(IxO) - (1.96 * sd(IxO))
# -12.23688
mean(IxO) + (1.96 * sd(IxO))
# 9.840805

#95% CI for DxO
mean(DxO)
# -4.478467
mean(DxO) - (1.96 * sd(DxO))
# -12.84108
mean(DxO) + (1.96 * sd(DxO))
# 3.884144

#95% CI for DyO
mean(DyO)
# -2.341772
mean(DyO) - (1.96 * sd(DyO))
# -12.19874
mean(DyO) + (1.96 * sd(DyO))
# 7.5152

#95% CI for OyD
mean(OyD)
# 2.734301
mean(OyD) - (1.96 * sd(OyD))
# -10.61288
mean(OyD) + (1.96 * sd(OyD))
# 16.08149


#########################################  PLOT DATA  #########################################


#To be able to plot the bootstrap data as boxplots I create a new data frame with the data

#First I make a vector with the populations
population <- c( rep("aInnY-LHm", 10000),
                 rep("bLHmX-Inn", 10000), rep("cOddX-Inn", 10000), rep("dOddY-Inn", 10000),
                 rep("eInnX-Odd", 10000), rep("fDahX-Odd", 10000), rep("gDahY-Odd", 10000),
                 rep("hOddY-Dah", 10000))

#Then I collcet all the bootstrap data in a new vector
deltaEggs <- c(IyL, LxI, OxI, OyI, IxO, DxO, DyO, OyD)

#Then it's all collceted in a new data frame
DMEFF <- data.frame(population, deltaEggs)

#And write it into a new file
write.csv(DMEFF, file = "DeltaMaleEffectOnFemaleFecundityPlot.csv") 


############################################  PLOT ############################################


#Read in csv file with data
DMEFFPlot.data <- read.table(file = "DeltaMaleEffectOnFemaleFecundityPlot.csv", h = T, sep = ",")

#MEAN
meanDMEFF <- tapply(DMEFFPlot.data$deltaEggs, DMEFFPlot.data$population, mean, na.rm = T)
#SD
sdDMEFF <- tapply(DMEFFPlot.data$deltaEggs, DMEFFPlot.data$population, sd, na.rm = T)


#Plot
par(mar = c(6, 5, 2, 2))
#Plot errorbars
xDMEFF <- c(0.5, 1.5,2,2.5,  3.5,4,4.5, 5.5)
errbar(xDMEFF, meanDMEFF, meanDMEFF + (1.96 * sdDMEFF), meanDMEFF - (1.96 * sdDMEFF), 
       xlim = c(0.3, 5.7), xlab = "", xaxt = "n", ylim = c(-30, 20), ylab = expression(Delta~"Number of Eggs"), 
       cex.axis = 1.2, cex.lab = 1.6, las = 1,  pch = c(17, 18), cex = c(3, 3.5), lwd = 3)
#AXIS
axis(1, at = c(0.5, 1.5,2,2.5,  3.5,4,4.5, 5.5), cex.axis = 1.5,
     labels = c(expression("I"["Y"]), 
                expression("L"["X"]), expression("O"["X"]), expression("O"["Y"]),
                expression("I"["X"]), expression("D"["X"]), expression("D"["Y"]),
                expression("O"["Y"])))
#Add line at 0
abline(h = 0, lty = 2, lwd = 2)
#And add text on top
mtext(expression("LH"["M"]), side = 1, line = 3, at = 0.5, cex = 1.6)
mtext(expression(italic("Innisfail")), side = 1, line = 3, at = 2, cex = 1.6)
mtext(expression(italic("Odder")), side = 1, line = 3, at = 4, cex = 1.6)
mtext(expression(italic("Dahomey")), side = 1, line = 3, at = 5.5, cex = 1.6)
#Now add arrows to show significance
points(0.5, 19.5, pch = "*", bg = "black", cex = 2.5)

