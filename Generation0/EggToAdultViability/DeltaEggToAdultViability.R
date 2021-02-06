################################################################################################
############################### ΔEGG-TO-ADULT OFFSPRING VIABILITY ##############################
################################################################################################

#Set up environment
library(Hmisc)

#Read in csv file with data
deltaEtA.data <- read.table(file = "EggToAdultViability.csv", h = T, sep = ",")


##########################################  STATISTIC  #########################################


### CALCULATE THE PROPORTION OF ECLOSED OFFSPRING ###
deltaEtA.data$prop_eclose <- deltaEtA.data$eclosed / 100


### ΔEGG-TO-ADULT CALCULATIONS ###

#To test if the change of sex chromosomes is significantly from the wild type population, 
#we do a bootstrap model to generate CI. If theses don't overlap 0 there has been a significant change
Delta.EtA <- as.numeric(tapply(deltaEtA.data$prop_eclose, deltaEtA.data$population, mean, na.rm = T))
#View Delta.EtA
Delta.EtA

#ΔEtA Inn-Lx - Innisfail
Delta.EtA[3] - Delta.EtA[2]
# 0.012

#ΔEtA Inn-Ly - Innisfail
Delta.EtA[4] - Delta.EtA[2]
# 0.06522222

#ΔEtA Inn-Ox - Innisfail
Delta.EtA[5] - Delta.EtA[2]
# -0.021

#ΔEtA Inn-Oy - Innisfail
Delta.EtA[6] - Delta.EtA[2]
# 0.045

#ΔEtA Odd-Ix - Odder
Delta.EtA[8] - Delta.EtA[7]
# -0.041

#ΔEtA Odd-Iy - Odder
Delta.EtA[9] - Delta.EtA[7]
# 0.083

#ΔEtA Odd-Dx - Odder
Delta.EtA[10] - Delta.EtA[7]
# -0.027

#ΔEtA Odd-Dy - Odder
Delta.EtA[11] - Delta.EtA[7]
# 0.082


#TEST OF PROBABILITY OF SUCCESS. THE PROBABILITY OF POSITIVE VALUE
binom.test(5, 8, p = 0.5,  alternative = "two.sided")
#Not significant, P = 0.7266


#First we make new vector to collect the data
LxI <- numeric(10000)
LyI <- numeric(10000)
OxI <- numeric(10000)
OyI <- numeric(10000)
IxO <- numeric(10000)
IyO <- numeric(10000)
DxO <- numeric(10000)
DyO <- numeric(10000)

#Then we set up a bootstrap that resampels the data from 10 data points to calculate a new mean everytime for 10000 times
for (i in 1:10000){
  DATA <- do.call(rbind, lapply(split(deltaEtA.data, deltaEtA.data$population), function(x) x[sample(10, replace = T),])) 
  Delta.EtA <- as.numeric(tapply(DATA$prop_eclose, DATA$population, mean, na.rm = T))
  LxI[i] <- Delta.EtA[3] - Delta.EtA[2]
  LyI[i] <- Delta.EtA[4] - Delta.EtA[2]
  OxI[i] <- Delta.EtA[5] - Delta.EtA[2] 
  OyI[i] <- Delta.EtA[6] - Delta.EtA[2]
  IxO[i] <- Delta.EtA[8] - Delta.EtA[7]
  IyO[i] <- Delta.EtA[9] - Delta.EtA[7]
  DxO[i] <- Delta.EtA[10] - Delta.EtA[7]
  DyO[i] <- Delta.EtA[11] - Delta.EtA[7]   }
#Run the calculation

#95% CI for LxI
mean(LxI)
# 0.0120167
mean(LxI) - (1.96 * sd(LxI))
# -0.07538602
mean(LxI) + (1.96 * sd(LxI))
# 0.09941942

#95% CI for LyI
mean(LyI)
# 0.0649472
mean(LyI) - (1.96 * sd(LyI))
# -0.001384755
mean(LyI) + (1.96 * sd(LyI))
# 0.1312792

#95% CI for OxI
mean(OxI)
# -0.0209501
mean(OxI) - (1.96 * sd(OxI))
# -0.09146467
mean(OxI) + (1.96 * sd(OxI))
# 0.04956447

#95% CI for OyI
mean(OyI)
# 0.0449297
mean(OyI) - (1.96 * sd(OyI))
# -0.03566782
mean(OyI) + (1.96 * sd(OyI))
# 0.1255272

#95% CI for IxO
mean(IxO)
# -0.0417557
mean(IxO) - (1.96 * sd(IxO))
# -0.1708264
mean(IxO) + (1.96 * sd(IxO))
# 0.08731496

#95% CI for IyO
mean(IyO)
# 0.0826641
mean(IyO) - (1.96 * sd(IyO))
# -0.01776238
mean(IyO) + (1.96 * sd(IyO))
# 0.1830906

#95% CI for DxO
mean(DxO)
# -0.0279631
mean(DxO) - (1.96 * sd(DxO))
# -0.1436605
mean(DxO) + (1.96 * sd(DxO))
# 0.08773426

#95% CI for DyO
mean(DyO)
# 0.0812175
mean(DyO) - (1.96 * sd(DyO))
# -0.023429
mean(DyO) + (1.96 * sd(DyO))
# 0.185864


#########################################  PLOT DATA  #########################################


#To be able to plot the bootstrap data as boxplots I create a new data frame with the data
#First I make a vector with the populations
population <- c( rep("aLHmX-Inn", 10000), rep("bLHmY-Inn", 10000), rep("cOddX-Inn", 10000), rep("dOddY-Inn", 10000),
                 rep("eInnX-Odd", 10000), rep("fInnY-Odd", 10000), rep("gDahX-Odd", 10000), rep("hDahY-Odd", 10000))

#Then I collcet all the bootstrap data in a new vector
deltaPropEclose <- c(LxI, LyI, OxI, OyI, IxO, IyO, DxO, DyO)

#Then it's all collceted in a new data frame
DEtA <- data.frame(population, deltaPropEclose)

#And write it into a new file
write.csv(DEtA, file = "DeltaEggToAdultViabilityplot.csv") 


#Read in csv file with data
DEtAplot.data <- read.table(file = "DeltaEggToAdultViabilityplot.csv", h = T, sep = ",")

#MEAN
meanDEtA <- tapply(DEtAplot.data$deltaPropEclose, DEtAplot.data$population, mean, na.rm = T)
#SD
sdDEtA <- tapply(DEtAplot.data$deltaPropEclose, DEtAplot.data$population, sd, na.rm = T)


# Plot
par(mar = c(6, 5, 2, 2))
#Plot errorbars
xDEtA <- c(0.5, 1, 1.5, 2, 3, 3.5, 4, 4.5)
errbar(xDEtA, meanDEtA, meanDEtA + (1.96 * sdDEtA), meanDEtA - (1.96 * sdDEtA), 
       xlim = c(0.3, 4.7), xlab = "", xaxt = "n", ylim = c(-0.2, 0.2), 
       ylab = expression(Delta~"Proportion of eclosed offspring"), 
       cex.axis = 1.2, cex.lab = 1.6, las = 1,  pch = c(17, 18), cex = c(3, 3.5), lwd = 3)
#AXIS
axis(1, at = c(0.5, 1, 1.5, 2, 3, 3.5, 4, 4.5), cex.axis = 1.5,
     labels = c(expression("L"["X"]), expression("L"["Y"]), expression("O"["X"]), expression("O"["Y"]),
                expression("I"["X"]), expression("I"["Y"]), expression("D"["X"]), expression("D"["Y"])))
#Add line at 0
abline(h = 0, lty = 2, lwd = 2)
#And add text on top
mtext(expression(italic("Innisfail")), side = 1, line = 3, at = 1.25, cex = 1.6)
mtext(expression(italic("Odder")), side = 1, line = 3, at = 3.75, cex = 1.6)
#Now add arrows to show significance
points(1, 0.19, pch = "*", bg = "black", cex = 2.5)

