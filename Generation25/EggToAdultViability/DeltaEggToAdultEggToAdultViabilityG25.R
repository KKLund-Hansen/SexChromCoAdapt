################################################################################################
####################### ΔEGG-TO-ADULT OFFSPRING VIABILITY, GENERATION 25 #######################
################################################################################################

#Set up environment
library(Hmisc)

#Read in csv file with data
DeltaEtAg25.data <- read.table(file = "EggToAdultViabilityG25.csv", h = T, sep = ",")


##########################################  STATISTIC  #########################################


### CALCULATE THE PROPORTION OF ECLOSED OFFSPRING ###
DeltaEtAg25.data$prop_eclose <- DeltaEtAg25.data$eclose / 100


### ΔPROP ECLOSED CALCULATIONS ###
#To test if the change of sex chromosomes is significantly from the wild type population, 
#we do a bootstrap model to generate CI. If theses don't overlap 0 there has been a significant change
Delta.EtAg25 <- as.numeric(tapply(DeltaEtAg25.data$prop_eclose, DeltaEtAg25.data$population, mean, na.rm = T))
#View Delta.EtAg25
Delta.EtAg25


#ΔEggToAdult Inn-Lx - Innisfail
Delta.EtAg25[3] - Delta.EtAg25[2]
# -0.02522727

#ΔEggToAdult Inn-Ly - Innisfail
Delta.EtAg25[4] - Delta.EtAg25[2]
# -0.0225

#ΔEggToAdult Inn-Ox - Innisfail
Delta.EtAg25[5] - Delta.EtAg25[2]
# -0.008333333

#ΔEggToAdult Inn-Oy - Innisfail
Delta.EtAg25[6] - Delta.EtAg25[2]
# -0.02

#ΔEggToAdult Odd-Ix - Odder
Delta.EtAg25[8] - Delta.EtAg25[7]
# 0.05327273

#ΔEggToAdult Odd-Iy - Odder
Delta.EtAg25[9] - Delta.EtAg25[7]
# 0.02727273

#ΔEggToAdult Odd-Dx - Odder
Delta.EtAg25[10] - Delta.EtAg25[7]
# -0.02772727

#ΔEggToAdult Odd-Dy - Odder
Delta.EtAg25[11] - Delta.EtAg25[7]
# 0.03427273


#TEST OF PROBABILITY OF SUCCESS. THE PROBABILITY OF POSITIVE VALUE
binom.test(3, 8, p = 0.5)
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

#Then we set up a bootstrap that resampels the data from 12 data points to calculate a new mean everytime for 10000 times
for (i in 1:10000){
  DATA <- do.call(rbind, lapply(split(DeltaEtAg25.data, DeltaEtAg25.data$population), function(x) x[sample(12, replace = T),])) 
  Delta.EtAg25 <- as.numeric(tapply(DATA$prop_eclose, DATA$population, mean, na.rm = T))
  LxI[i] <- Delta.EtAg25[3] - Delta.EtAg25[2]
  LyI[i] <- Delta.EtAg25[4] - Delta.EtAg25[2]
  OxI[i] <- Delta.EtAg25[5] - Delta.EtAg25[2]
  OyI[i] <- Delta.EtAg25[6] - Delta.EtAg25[2]
  IxO[i] <- Delta.EtAg25[8] - Delta.EtAg25[7]
  IyO[i] <- Delta.EtAg25[9] - Delta.EtAg25[7]
  DxO[i] <- Delta.EtAg25[10] - Delta.EtAg25[7]
  DyO[i] <- Delta.EtAg25[11] - Delta.EtAg25[7]  }
#Run the calculation


#95% CI for LxI
mean(LxI)
# -0.02575038
mean(LxI) - (1.96 * sd(LxI))
# -0.07915415
mean(LxI) + (1.96 * sd(LxI))
# 0.02765339

#95% CI for LyI
mean(LyI)
# -0.02285031
mean(LyI) - (1.96 * sd(LyI))
# -0.07837185
mean(LyI) + (1.96 * sd(LyI))
# 0.03267124

#95% CI for OxI
mean(OxI)
# -0.008731305
mean(OxI) - (1.96 * sd(OxI))
# -0.06090735
mean(OxI) + (1.96 * sd(OxI))
# 0.04344473

#95% CI for OyI
mean(OyI)
# -0.02038681
mean(OyI) - (1.96 * sd(OyI))
# -0.07526117
mean(OyI) + (1.96 * sd(OyI))
# 0.03448756

#95% CI for IxO
mean(IxO)
# 0.05354358
mean(IxO) - (1.96 * sd(IxO))
# 0.004360899
mean(IxO) + (1.96 * sd(IxO))
# 0.1027263

#95% CI for IyO
mean(IyO)
# 0.02745676
mean(IyO) - (1.96 * sd(IyO))
# -0.0173369
mean(IyO) + (1.96 * sd(IyO))
# 0.07225042

#95% CI for DxO
mean(DxO)
# -0.02724059
mean(DxO) - (1.96 * sd(DxO))
# -0.08587661
mean(DxO) + (1.96 * sd(DxO))
# 0.03139543

#95% CI for DyOe
mean(DyO)
# 0.03454394
mean(DyO) - (1.96 * sd(DyO))
# -0.004893534
mean(DyO) + (1.96 * sd(DyO))
# 0.07398142



#########################################  PLOT DATA  #########################################


#To be able to plot the bootstrap data as boxplots I create a new data frame with the data

#First I make a vector with the populations
population <- c(rep("aLHmX_Inn", 10000), rep("bLHmY_Inn", 10000), rep("cOddX_Inn", 10000), rep("dOddY_Inn", 10000),
                rep("eInnX_Odd", 10000), rep("fInnY_Odd", 10000), rep("gDahX_Odd", 10000), rep("hDahY_Odd", 10000))

#Then I collcet all the bootstrap data in a new vector
deltaProp_eclose <- c(LxI, LyI, OxI, OyI, IxO, IyO, DxO, DyO)

#Then it's all collceted in a new data frame
EvolDEtA <- data.frame(population, deltaProp_eclose)

#And write it into a new file
write.csv(EvolDEtA, file = "DeltaEggToAdultEggToAdultViabilityG25.csv")


#Read in csv file with data
DeltaEtAg25plot.data <- read.table(file = "DeltaEggToAdultEggToAdultViabilityG25.csv", h = T, sep = ",")

#MEAN
meanDEtAg25 <- tapply(DeltaEtAg25plot.data$deltaProp_eclose, DeltaEtAg25plot.data$population, mean)
#SD
sdDEtAg25 <- tapply(DeltaEtAg25plot.data$deltaProp_eclose, DeltaEtAg25plot.data$population, sd)


#Plot
par(mar = c(6, 5, 2, 2))
#Plot errorbars
xDEtAg25 <- c(0.5,1,1.5,2, 3,3.5,4,4.5)
errbar(xDEtAg25, meanDEtAg25, meanDEtAg25 + (1.96 * sdDEtAg25), meanDEtAg25 - (1.96 * sdDEtAg25), 
       xlim = c(0.3, 4.7), xlab = "", xaxt = "n", ylim = c(-0.1, 0.12), yaxt = "n",
       ylab = expression(Delta~"Proportion of eclosed offspring"), 
       cex.axis = 1.2, cex.lab = 1.5, las = 1,  pch = c(17, 18), cex = c(3, 3.5), lwd = 3)
#AXIS
axis(1, at = c(0.5,1,1.5,2, 3,3.5,4,4.5), cex.axis = 1.2,
     labels = c(expression("L"["X"]), expression("L"["Y"]), expression("O"["X"]), expression("O"["Y"]),
                expression("I"["X"]), expression("I"["Y"]), expression("D"["X"]), expression("D"["Y"])))
axis(2, at = seq(-0.10, 0.12,  by = 0.02), cex.axis = 1.2, las = 1)
#Add line at 0
abline(h = 0, lty = 2, lwd = 2)
#And add text below
mtext(expression(italic("Innisfail")), side = 1, line = 3, at = 1.25, cex = 1.5)
mtext(expression(italic("Odder")), side = 1, line = 3, at = 3.75, cex = 1.5)



