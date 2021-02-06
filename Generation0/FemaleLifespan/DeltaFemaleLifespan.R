################################################################################################
####################################### ΔFEMALE LIFESPAN #######################################
################################################################################################

#Set up environment
library(Hmisc)

#Read in csv file with data
deltaFL.data <- read.table(file = "FemaleLifespanMean.csv", h = T, sep = ",")


##########################################  STATISTIC  #########################################


### ΔFEMALELONGEVITY CALCULATIONS ###

#To test if the change of sex chromosomes is significantly from the wild type population, 
#we do a bootstrap model to generate CI. If theses don't overlap 0 there has been a significant change
Delta.FL <- as.numeric(tapply(deltaFL.data$age, deltaFL.data$population, mean, na.rm = T))
#View Delta.FL
Delta.FL


#Δfemalelongevity LHm-Iy - LHm
Delta.FL[2] - Delta.FL[1]
# 0.66

#Δfitness Inn-Lx - Innisfail
Delta.FL[4] - Delta.FL[3]
# 2.01

#Δfitness Inn-Ly - Innisfail
Delta.FL[5] - Delta.FL[3]
# -1.1

#Δfitness Inn-Ox - Innisfail
Delta.FL[6] - Delta.FL[3]
# 0.84

#Δfitness Inn-Oy - Innisfail
Delta.FL[7] - Delta.FL[3]
# -0.62

#Δfitness Odd-Ix - Odder
Delta.FL[9] - Delta.FL[8]
# 0.32

#Δfitness Odd-Iy - Odder
Delta.FL[10] - Delta.FL[8]
# -0.03

#Δfitness Odd-Dx - Odder
Delta.FL[11] - Delta.FL[8]
# -1.69

#Δfitness Odd-Dy - Odder
Delta.FL[12] - Delta.FL[8]
# -3

#Δfitness Dah-Oy - Dahomey
Delta.FL[14] - Delta.FL[13]
# 6.34


#TEST OF PROBABILITY OF SUCCESS. THE PROBABILITY OF POSITIVE VALUE
binom.test(5, 10, p = 0.5,  alternative = "two.sided")
#Not significant, P = 1


#First we make new vector to collect the data
IyL <- numeric(10000)
LxI <- numeric(10000)
LyI <- numeric(10000)
OxI <- numeric(10000)
OyI <- numeric(10000)
IxO <- numeric(10000)
IyO <- numeric(10000)
DxO <- numeric(10000)
DyO <- numeric(10000)
OyD <- numeric(10000)


#Then we set up a bootstrap that resampels the data from 10 data points to calculate a new mean everytime for 10000 times
for (i in 1:10000){
  DATA <- do.call(rbind, lapply(split(deltaFL.data, deltaFL.data$population), function(x) x[sample(10, replace = T),])) 
  Delta.FL <- as.numeric(tapply(DATA$age, DATA$population, mean, na.rm = T))
  IyL[i] <- Delta.FL[2] - Delta.FL[1]
  LxI[i] <- Delta.FL[4] - Delta.FL[3]
  LyI[i] <- Delta.FL[5] - Delta.FL[3]
  OxI[i] <- Delta.FL[6] - Delta.FL[3]
  OyI[i] <- Delta.FL[7] - Delta.FL[3]
  IxO[i] <- Delta.FL[9] - Delta.FL[8]
  IyO[i] <- Delta.FL[10] - Delta.FL[8]
  DxO[i] <- Delta.FL[11] - Delta.FL[8]
  DyO[i] <- Delta.FL[12] - Delta.FL[8]
  OyD[i] <- Delta.FL[14] - Delta.FL[13]  }
#Run the calculation


#95% CI for IyL
mean(IyL)
# 0.620203
mean(IyL) - (1.96 * sd(IyL))
# -4.418655
mean(IyL) + (1.96 * sd(IyL))
# 5.659061

#95% CI for LxI
mean(LxI)
# 2.00001
mean(LxI) - (1.96 * sd(LxI))
# -2.178354
mean(LxI) + (1.96 * sd(LxI))
# 6.178376

#95% CI for LyI
mean(LyI)
# -1.099556
mean(LyI) - (1.96 * sd(LyI))
# -5.538119
mean(LyI) + (1.96 * sd(LyI))
# 3.339007

#95% CI for OxI
mean(OxI)
# 0.842568
mean(OxI) - (1.96 * sd(OxI))
# -3.451585
mean(OxI) + (1.96 * sd(OxI))
# 5.136721

#95% CI for OyI
mean(OyI)
# -0.640347
mean(OyI) - (1.96 * sd(OyI))
# -5.092054
mean(OyI) + (1.96 * sd(OyI))
# 3.81136

#95% CI for IxO
mean(IxO)
# 0.303346
mean(IxO) - (1.96 * sd(IxO))
# -4.879275
mean(IxO) + (1.96 * sd(IxO))
# 5.485967

#95% CI for IyO
mean(IyO)
# -0.042935
mean(IyO) - (1.96 * sd(IyO))
# -4.059259
mean(IyO) + (1.96 * sd(IyO))
# 3.973389

#95% CI for DxO
mean(DxO)
# -1.694258
mean(DxO) - (1.96 * sd(DxO))
# -5.753506
mean(DxO) + (1.96 * sd(DxO))
# 2.36499

#95% CI for DyO
mean(DyO)
# -3.00299
mean(DyO) - (1.96 * sd(DyO))
# -7.1771
mean(DyO) + (1.96 * sd(DyO))
# 1.17112

#95% CI for OyD
mean(OyD)
# 6.317752
mean(OyD) - (1.96 * sd(OyD))
# 1.131343
mean(OyD) + (1.96 * sd(OyD))
# 11.50416


#########################################  PLOT DATA  #########################################


#To be able to plot the bootstrap data as boxplots I create a new data frame with the data

#First I make a vector with the populations
population <- c( rep("aInnY-LHm", 10000), 
                 rep("bLHmX-Inn", 10000), rep("cLHmY-Inn", 10000), rep("dOddX-Inn", 10000), rep("eOddY-Inn", 10000),
                 rep("fInnX-Odd", 10000), rep("gInnY-Odd", 10000), rep("hDahX-Odd", 10000), rep("iDahY-Odd", 10000),
                 rep("jOddY-Dah", 10000))

#Then I collcet all the bootstrap data in a new vector
deltaAge <- c(IyL, LxI, LyI, OxI, OyI, IxO, IyO, DxO, DyO, OyD)

#Then it's all collceted in a new data frame
DFL <- data.frame(population, deltaAge)

#And write it into a new file
write.csv(DFL, file = "DeltaFemaleLifespanPlot.csv") 


#Read in csv file with data
DFLplot.data <- read.table(file = "DeltaFemaleLifespanPlot.csv", h = T, sep = ",")

#MEAN
meanDFL <- tapply(DFLplot.data$deltaAge, DFLplot.data$population, mean, na.rm = T)
#SD
sdDFL <- tapply(DFLplot.data$deltaAge, DFLplot.data$population, sd, na.rm = T)


par(mar = c(6, 5, 2, 2))
#Plot errorbars
xDFL <- c(0.5, 1.5,2,2.5,3, 4,4.5,5,5.5, 6.5)
errbar(xDFL, meanDFL, meanDFL + (1.96 * sdDFL), meanDFL - (1.96 * sdDFL), 
       xlim = c(0.3, 6.7), xlab = "", xaxt = "n", ylim = c(-10, 15), ylab = expression(Delta~"Mean age at death"), 
       cex.axis = 1.2, cex.lab = 1.6, las = 1,  pch = c(17, 18), cex = c(3, 3.5), lwd = 3)
#AXIS
axis(1, at = c(0.5, 1.5,2,2.5,3, 4,4.5,5,5.5, 6.5), cex.axis = 1.5,
     labels = c(expression("I"["Y"]), 
                expression("L"["X"]), expression("L"["Y"]), expression("O"["X"]), expression("O"["Y"]),
                expression("I"["X"]), expression("I"["Y"]), expression("D"["X"]), expression("D"["Y"]),
                expression("O"["Y"])))
#Add line at 0
abline(h = 0, lty = 2, lwd = 2)
#And add text on top
mtext(expression("LH"["M"]), side = 1, line = 3, at = 0.5, cex = 1.6)
mtext(expression(italic("Innisfail")), side = 1, line = 3, at = 2.25, cex = 1.6)
mtext(expression(italic("Odder")), side = 1, line = 3, at = 4.75, cex = 1.6)
mtext(expression(italic("Dahomey")), side = 1, line = 3, at = 6.5, cex = 1.6)
#Now add arrows to show significance
points(6.5, 14.5, pch = "*", bg = "black", cex = 2.5)


