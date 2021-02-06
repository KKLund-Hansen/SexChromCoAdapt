################################################################################################
########################################## ΔSEX RATIO ##########################################
################################################################################################

#Set up environment
library(Hmisc)

#Read in csv file with data
deltaSR.data <- read.table(file = "SexRatio.csv", h = T, sep = ",")


##########################################  STATISTIC  #########################################


### CALCULATE THE PROPORTION OF MALE OFFSPRING ###
deltaSR.data$prop_male <- deltaSR.data$male / deltaSR.data$total


### ΔSEX RATIO, MALE OFFSPRING CALCULATIONS ###
#To test if the change of sex chromosomes is significantly from the wild type population, 
#we do a bootstrap model to generate CI. If theses don't overlap 0 there has been a significant change
Delta.SR <- as.numeric(tapply(deltaSR.data$prop_male, deltaSR.data$population, mean, na.rm = T))
#View Delta.SR
Delta.SR


#ΔSexRatio Inn-Lx - Innisfail
Delta.SR[3] - Delta.SR[2]
# -0.02132319

#ΔSexRatio Inn-Ly - Innisfail
Delta.SR[4] - Delta.SR[2]
# -0.008459837

#ΔSexRatio Inn-Ox - Innisfail
Delta.SR[5] - Delta.SR[2]
# -0.003409214

#ΔSexRatio Inn-Oy - Innisfail
Delta.SR[6] - Delta.SR[2]
# -0.003409214

#ΔSexRatio Odd-Ix - Odder
Delta.SR[8] - Delta.SR[7]
# -0.02182432

#ΔSexRatio Odd-Iy - Odder
Delta.SR[9] - Delta.SR[7]
# -0.01608431

#ΔSexRatio Odd-Dx - Odder
Delta.SR[10] - Delta.SR[7]
# -0.03623733

#ΔSexRatio Odd-Dy - Odder
Delta.SR[11] - Delta.SR[7]
# -0.03623733


#TEST OF PROBABILITY OF SUCCESS. THE PROBABILITY OF POSITIVE VALUE
binom.test(0, 8, p = 0.5,  alternative = "two.sided")
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


#Then we set up a bootstrap that resampels the data from 10 data points to calculate a new mean everytime for 10000 times
for (i in 1:10000){
  DATA <- do.call(rbind, lapply(split(deltaSR.data, deltaSR.data$population), function(x) x[sample(10, replace = T),])) 
  Delta.SR <- as.numeric(tapply(DATA$prop_male, DATA$population, mean, na.rm = T))
  LxI[i] <- Delta.SR[3] - Delta.SR[2]
  LyI[i] <- Delta.SR[4] - Delta.SR[2]
  OxI[i] <- Delta.SR[5] - Delta.SR[2]
  OyI[i] <- Delta.SR[6] - Delta.SR[2]
  IxO[i] <- Delta.SR[8] - Delta.SR[7]
  IyO[i] <- Delta.SR[9] - Delta.SR[7]
  DxO[i] <- Delta.SR[10] - Delta.SR[7]
  DyO[i] <- Delta.SR[11] - Delta.SR[7] }
#Run the calculation


#95% CI for LxI
mean(LxI)
# -0.02124663
mean(LxI) - (1.96 * sd(LxI))
# -0.05579977
mean(LxI) + (1.96 * sd(LxI))
# 0.01330651

#95% CI for LyI
mean(LyI)
# -0.008444966
mean(LyI) - (1.96 * sd(LyI))
# -0.04745121
mean(LyI) + (1.96 * sd(LyI))
# 0.03056128

#95% CI for OxI
mean(OxI)
# -0.003296774
mean(OxI) - (1.96 * sd(OxI))
# -0.04022508
mean(OxI) + (1.96 * sd(OxI))
# 0.03363153

#95% CI for OyI
mean(OyI)
# -0.01259933
mean(OyI) - (1.96 * sd(OyI))
# -0.06792843
mean(OyI) + (1.96 * sd(OyI))
# 0.04272977

#95% CI for IxO
mean(IxO)
# -0.02172025
mean(IxO) - (1.96 * sd(IxO))
# -0.05964586
mean(IxO) + (1.96 * sd(IxO))
# 0.01620535

#95% CI for IyO
mean(IyO)
# -0.0160189
mean(IyO) - (1.96 * sd(IyO))
# -0.06397044
mean(IyO) + (1.96 * sd(IyO))
# 0.03193265

#95% CI for DxO
mean(DxO)
# -0.03602441
mean(DxO) - (1.96 * sd(DxO))
# -0.09130232
mean(DxO) + (1.96 * sd(DxO))
# 0.0192535

#95% CI for DyO
mean(DyO)
# -0.01640735
mean(DyO) - (1.96 * sd(DyO))
# -0.06326163
mean(DyO) + (1.96 * sd(DyO))
# 0.03044693


#########################################  PLOT DATA  #########################################


#To be able to plot the bootstrap data as boxplots I create a new data frame with the data

#First I make a vector with the populations
population <- c( rep("aLHmX-Inn", 10000), rep("bLHmY-Inn", 10000), rep("cOddX-Inn", 10000), rep("dOddY-Inn", 10000),
                 rep("eInnX-Odd", 10000), rep("fInnY-Odd", 10000), rep("gDahX-Odd", 10000), rep("hDahY-Odd", 10000))

#Then I collcet all the bootstrap data in a new vector
deltaProp_male <- c(LxI, LyI, OxI, OyI, IxO, IyO, DxO, DyO)

#Then it's all collceted in a new data frame
DSR <- data.frame(population, deltaProp_male)

#And write it into a new file
write.csv(DSR, file = "DeltaSexRatioPlot.csv") 


#Read in csv file with data
DSRplot.data <- read.table(file = "DeltaSexRatioPlot.csv", h = T, sep = ",")

#MEAN
meanDSR <- tapply(DSRplot.data$deltaProp_male, DSRplot.data$population, mean, na.rm = T)
#SD
sdDSR <- tapply(DSRplot.data$deltaProp_male, DSRplot.data$population, sd, na.rm = T)


#Plot
par(mar = c(6, 5, 2, 2))
#Plot errorbars
xDSR <- c(0.5,1,1.5,2, 3,3.5,4,4.5)
errbar(xDSR, meanDSR, meanDSR + (1.96 * sdDSR), meanDSR - (1.96 * sdDSR), 
       xlim = c(0.3, 4.7), xlab = "", xaxt = "n", ylim = c(-0.12, 0.06),
       ylab = expression(Delta~"Proportion of male offspring"), 
       cex.axis = 1.2, cex.lab = 1.6, las = 1,  pch = c(17, 18), cex = c(3, 3.5), lwd = 3)
#AXIS
axis(1, at = c(0.5,1,1.5,2, 3,3.5,4,4.5), cex.axis = 1.5,
     labels = c(expression("L"["X"]), expression("L"["Y"]), expression("O"["X"]), expression("O"["Y"]),
                expression("I"["X"]), expression("I"["Y"]), expression("D"["X"]), expression("D"["Y"])))
#Add line at 0
abline(h = 0, lty = 2, lwd = 2)
#And add text on top
mtext(expression(italic("Innisfail")), side = 1, line = 3, at = 1.25, cex = 1.6)
mtext(expression(italic("Odder")), side = 1, line = 3, at = 3.75, cex = 1.6)
#Now add arrows to show significance
points(0.5, 0.055, pch = "*", bg = "black", cex = 2.5)



