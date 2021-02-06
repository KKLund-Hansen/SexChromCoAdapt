################################################################################################
######################################### ΔTHORAX SIZE #########################################
################################################################################################

#Set up environment
library(Hmisc)

#Read in csv file with data
deltaTS.data <- read.table(file = "ThoraxSize.csv", header = T, sep = ",")


##########################################  STATISTIC  #########################################


### ΔTHORAXSIZE CALCULATIONS ###

#To test if the change of sex chromosomes is significantly from the wild type population, 
#we do a bootstrap model to generate CI. If theses don't overlap 0 there has been a significant change
Delta.TS <- as.numeric(tapply(deltaTS.data$size_mm, deltaTS.data$population, mean, na.rm = T))
#View Delta.TS
Delta.TS


#Δthoraxsize LHm-Tx - LHm
Delta.TS[2] - Delta.TS[1]
# 0.05328798

#Δthoraxsize LHm-Ty - LHm
Delta.TS[3] - Delta.TS[1]
# -0.006077097

#Δthoraxsize LHm-Ix - LHm
Delta.TS[4] - Delta.TS[1]
# 0.01678005

#Δthoraxsize LHm-Iy - LHm
Delta.TS[5] - Delta.TS[1]
# 0.00185941

#Δthoraxsize Inn-Lx - Innisfail
Delta.TS[7] - Delta.TS[6]
# 0.02793651

#Δthoraxsize Inn-Ly - Innisfail
Delta.TS[8] - Delta.TS[6]
# 0.005065856

#Δthoraxsize Inn-Ox - Innisfail
Delta.TS[9] - Delta.TS[6]
# 0.007767646

#Δthoraxsize Inn-Oy - Innisfail
Delta.TS[10] - Delta.TS[6]
# 0.01653439

#Δthoraxsize Odd-Ix - Odder
Delta.TS[12] - Delta.TS[11]
# 0.02922579

#Δthoraxsize Odd-Iy - Odder
Delta.TS[13] - Delta.TS[11]
# -0.0203369

#Δthoraxsize Odd-Dx - Odder
Delta.TS[14] - Delta.TS[11]
# -0.00478782

#Δthoraxsize Odd-Dy - Odder
Delta.TS[15] - Delta.TS[11]
# -0.007619048

#Δthoraxsize Dah-Ox - Dahomey
Delta.TS[17] - Delta.TS[16]
# -0.01079365

#Δthoraxsize Dah-Oy - Dahomey
Delta.TS[18] - Delta.TS[16]
# -0.00467768

#Δthoraxsize Dah-Tx - Dahomey
Delta.TS[19] - Delta.TS[16]
# 0.004444444

#Δthoraxsize Dah-Ty - Dahomey
Delta.TS[20] - Delta.TS[16]
# -0.01809524

#Δthoraxsize Tas-Dx - Tasmania
Delta.TS[22] - Delta.TS[21]
# 0.01650794

#Δthoraxsize Tas-Dy - Tasmania
Delta.TS[23] - Delta.TS[21]
# -0.002539682

#Δthoraxsize Tas-Lx - Tasmania
Delta.TS[24] - Delta.TS[21]
# -0.003809524

#Δthoraxsize Tas-Ly - Tasmania
Delta.TS[25] - Delta.TS[21]
# -0.003174603


#TEST OF PROBABILITY OF SUCCESS. THE PROBABILITY OF POSITIVE VALUE
binom.test(10, 20, p = 0.5,  alternative = "two.sided")
#Non-significant, P = 1


#First we make new vector to collect the data
TxL <- numeric(10000)
TyL <- numeric(10000)
IxL <- numeric(10000)
IyL <- numeric(10000)
LxI <- numeric(10000)
LyI <- numeric(10000)
OxI <- numeric(10000)
OyI <- numeric(10000)
IxO <- numeric(10000)
IyO <- numeric(10000)
DxO <- numeric(10000)
DyO <- numeric(10000)
OxD <- numeric(10000)
OyD <- numeric(10000)
TxD <- numeric(10000)
TyD <- numeric(10000)
DxT <- numeric(10000)
DyT <- numeric(10000)
LxT <- numeric(10000)  
LyT <- numeric(10000)

#Then we set up a bootstrap that resampels the data from 50 data points to calculate a new mean everytime for 10000 times
for (i in 1:10000){
  DATA <- do.call(rbind, lapply(split(deltaTS.data, deltaTS.data$population), function(x) x[sample(50, replace = T),])) 
  Delta.TS <- as.numeric(tapply(DATA$size_mm, DATA$population, mean, na.rm = T))
  TxL[i] <- Delta.TS[2] - Delta.TS[1]
  TyL[i] <- Delta.TS[3] - Delta.TS[1]
  IxL[i] <- Delta.TS[4] - Delta.TS[1]
  IyL[i] <- Delta.TS[5] - Delta.TS[1]
  LxI[i] <- Delta.TS[7] - Delta.TS[6]
  LyI[i] <- Delta.TS[8] - Delta.TS[6]
  OxI[i] <- Delta.TS[9] - Delta.TS[6]
  OyI[i] <- Delta.TS[10] - Delta.TS[6]
  IxO[i] <- Delta.TS[12] - Delta.TS[11]
  IyO[i] <- Delta.TS[13] - Delta.TS[11]
  DxO[i] <- Delta.TS[14] - Delta.TS[11]
  DyO[i] <- Delta.TS[15] - Delta.TS[11]
  OxD[i] <- Delta.TS[17] - Delta.TS[16]
  OyD[i] <- Delta.TS[18] - Delta.TS[16]
  TxD[i] <- Delta.TS[19] - Delta.TS[16]
  TyD[i] <- Delta.TS[20] - Delta.TS[16]
  DxT[i] <- Delta.TS[22] - Delta.TS[21] 
  DyT[i] <- Delta.TS[23] - Delta.TS[21] 
  LxT[i] <- Delta.TS[24] - Delta.TS[21]
  LyT[i] <- Delta.TS[25] - Delta.TS[21]  }
#Run the calculation

#95% CI for TxL
mean(TxL)
# 0.05332979
mean(TxL) - (1.96 * sd(TxL))
# 0.04359923
mean(TxL) + (1.96 * sd(TxL))
# 0.06306035

#95% CI for TyL
mean(TyL)
# -0.006035257
mean(TyL) - (1.96 * sd(TyL))
# -0.01483202
mean(TyL) + (1.96 * sd(TyL))
# 0.002761509

#95% CI for IxL
mean(IxL)
# 0.01684182
mean(IxL) - (1.96 * sd(IxL))
# 0.00751809
mean(IxL) + (1.96 * sd(IxL))
# 0.02616555

#95% CI for IyL
mean(IyL)
# 0.001860584
mean(IyL) - (1.96 * sd(IyL))
# -0.009308939
mean(IyL) + (1.96 * sd(IyL))
# 0.01303011

#95% CI for LxI
mean(LxI)
# 0.0279632
mean(LxI) - (1.96 * sd(LxI))
# 0.02049004
mean(LxI) + (1.96 * sd(LxI))
# 0.03543637

#95% CI for LyI
mean(LyI)
# 0.005171427
mean(LyI) - (1.96 * sd(LyI))
# -0.005996135
mean(LyI) + (1.96 * sd(LyI))
# 0.01633899

#95% CI for OxI
mean(OxI)
# 0.007803413
mean(OxI) - (1.96 * sd(OxI))
# -0.001299376
mean(OxI) + (1.96 * sd(OxI))
# 0.0169062

#95% CI for OyI
mean(OyI)
# 0.01649386
mean(OyI) - (1.96 * sd(OyI))
# 0.008252916
mean(OyI) + (1.96 * sd(OyI))
# 0.0247348

#95% CI for IxO
mean(IxO)
# 0.02930494
mean(IxO) - (1.96 * sd(IxO))
# 0.01658436
mean(IxO) + (1.96 * sd(IxO))
# 0.04202551

#95% CI for IyO
mean(IyO)
# -0.02028359
mean(IyO) - (1.96 * sd(IyO))
# -0.03192828
mean(IyO) + (1.96 * sd(IyO))
# -0.008638899

#95% CI for DxO
mean(DxO)
# -0.004691979
mean(DxO) - (1.96 * sd(DxO))
# -0.01649282
mean(DxO) + (1.96 * sd(DxO))
# 0.007108864

#95% CI for DyO
mean(DyO)
# -0.007608921
mean(DyO) - (1.96 * sd(DyO))
# -0.01875811
mean(DyO) + (1.96 * sd(DyO))
# 0.00354027

#95% CI for OxD
mean(OxD)
# -0.01077724
mean(OxD) - (1.96 * sd(OxD))
# -0.01776642
mean(OxD) + (1.96 * sd(OxD))
# -0.003788061

#95% CI for OyD
mean(OyD)
# -0.004688958
mean(OyD) - (1.96 * sd(OyD))
# -0.01102898
mean(OyD) + (1.96 * sd(OyD))
# 0.00165106

#95% CI for TxD
mean(TxD)
# 0.004446127
mean(TxD) - (1.96 * sd(TxD))
# -0.003138911
mean(TxD) + (1.96 * sd(TxD))
# 0.01203116

#95% CI for TyD
mean(TyD)
# -0.01811978
mean(TyD) - (1.96 * sd(TyD))
# -0.02539528
mean(TyD) + (1.96 * sd(TyD))
# -0.01084428

#95% CI for DxT
mean(DxT)
# 0.0164514
mean(DxT) - (1.96 * sd(DxT))
# 0.007985115
mean(DxT) + (1.96 * sd(DxT))
# 0.02491768

#95% CI for DyT
mean(DyT)
# -0.002585016
mean(DyT) - (1.96 * sd(DyT))
# -0.01020125
mean(DyT) + (1.96 * sd(DyT))
# 0.005031223

#95% CI for LxT
mean(LxT)
# -0.003844857
mean(LxT) - (1.96 * sd(LxT))
# -0.01124102
mean(LxT) + (1.96 * sd(LxT))
# 0.003551302

#95% CI for LyT
mean(LyT)
# -0.00318327
mean(LyT) - (1.96 * sd(LyT))
# -0.0109266
mean(LyT) + (1.96 * sd(LyT))
# 0.004560056



#########################################  PLOT DATA  #########################################


#To be able to plot the bootstrap data as boxplots I create a new data frame with the data

#First I make a vector with the populations
population <- c( rep("aTasX-LHm", 10000), rep("bTasY-LHm", 10000), rep("cInnX-LHm", 10000), rep("dInnY-LHm", 10000),
                 rep("eLHmX-Inn", 10000), rep("fLHmY-Inn", 10000), rep("gOddX-Inn", 10000), rep("hOddY-Inn", 10000),
                 rep("iInnX-Odd", 10000), rep("jInnY-Odd", 10000), rep("kDahX-Odd", 10000), rep("lDahY-Odd", 10000),
                 rep("mOddX-Dah", 10000), rep("nOddY-Dah", 10000), rep("oTasX-Dah", 10000), rep("pTasY-Dah", 10000),
                 rep("qDahX-Tas", 10000), rep("rDahY-Tas", 10000), rep("sLHmX-Tas", 10000), rep("tLHmY-Tas", 10000))

#Then I collcet all the bootstrap data in a new vector
deltasize_mm <- c(TxL, TyL, IxL, IyL, LxI, LyI, OxI, OyI,
                  IxO, IyO, DxO, DyO, OxD, OyD, TxD, TyD, DxT, DyT, LxT, LyT)

#Then it's all collceted in a new data frame
DTS <- data.frame(population, deltasize_mm)

#And write it into a new file
write.csv(DTS, file = "DeltaThoraxSizeplot.csv") 


#Read in csv file with data
DTSplot.data <- read.table(file = "DeltaThoraxSizeplot.csv", h = T, sep = ",")

#MEAN
meanDTS <- tapply(DTSplot.data$deltasize_mm, DTSplot.data$population, mean, na.rm = T)
#SD
sdDTS <- tapply(DTSplot.data$deltasize_mm, DTSplot.data$population, sd, na.rm = T)


#Plot
par(mar = c(6, 5, 2, 2))
#Plot errorbars
xDTS <- c(0.5,1,1.5,2, 3,3.5,4,4.5, 5.5,6,6.5,7, 8,8.5,9,9.5, 10.5,11,11.5,12)
errbar(xDTS, meanDTS, meanDTS + (1.96 * sdDTS), meanDTS - (1.96 * sdDTS), 
       xlim = c(0.3, 12.2), xlab = "", xaxt = "n", ylim = c(-0.04, 0.08), ylab = expression(Delta~"Thorax length/mm"), 
       cex.axis = 1.2, cex.lab = 1.6, las = 1,  pch = c(17, 18), cex = c(3, 3.5), lwd = 3)
#AXIS
axis(1, at = c(0.5,1,1.5,2, 3,3.5,4,4.5, 5.5,6,6.5,7, 8,8.5,9,9.5, 10.5,11,11.5,12), cex.axis = 1.5,
     labels = c(expression("T"["X"]), expression("T"["Y"]), expression("I"["X"]), expression("I"["Y"]), 
                expression("L"["X"]), expression("L"["Y"]), expression("O"["X"]), expression("O"["Y"]),
                expression("I"["X"]), expression("I"["Y"]), expression("D"["X"]), expression("D"["Y"]),
                expression("O"["X"]), expression("O"["Y"]), expression("T"["X"]), expression("T"["Y"]),
                expression("D"["X"]), expression("D"["Y"]), expression("L"["X"]), expression("L"["Y"])))
#Add line at 0
abline(h = 0, lty = 2, lwd = 2)
#And add text on top
mtext(expression("LH"["M"]), side = 1, line = 3, at = 1.25, cex = 1.6)
mtext(expression(italic("Innisfail")), side = 1, line = 3, at = 3.75, cex = 1.6)
mtext(expression(italic("Odder")), side = 1, line = 3, at = 6.25, cex = 1.6)
mtext(expression(italic("Dahomey")), side = 1, line = 3, at = 8.75, cex = 1.6)
mtext(expression(italic("Tasmania")), side = 1, line = 3, at = 11.25, cex = 1.6)
#Now add arrows to show significance
points(0.5, 0.075, pch = "*", bg = "black", cex = 2.5)
points(1.5, 0.075, pch = "*", bg = "black", cex = 2.5)
points(3, 0.075, pch = "*", bg = "black", cex = 2.5)
points(4.5, 0.075, pch = "*", bg = "black", cex = 2.5)
points(5.5, 0.075, pch = "*", bg = "black", cex = 2.5)
points(6, 0.075, pch = "*", bg = "black", cex = 2.5)
points(8, 0.075, pch = "*", bg = "black", cex = 2.5)
points(9.5, 0.075, pch = "*", bg = "black", cex = 2.5)
points(10.5, 0.075, pch = "*", bg = "black", cex = 2.5)


