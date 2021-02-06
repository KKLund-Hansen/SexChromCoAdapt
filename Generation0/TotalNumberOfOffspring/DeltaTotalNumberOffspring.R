################################################################################################
################################## ΔTOTAL NUMBER OF OFFSPRING ##################################
################################################################################################

#Set up environment
library(Hmisc)

#Read in csv file with data
deltaTNO.data <- read.table(file = "TotalNumberOffspring.csv", h = T, sep = ",")


##########################################  STATISTIC  #########################################


### ΔTOTALOFFSPRING CALCULATIONS ###
#To test if the change of sex chromosomes is significantly from the wild type population, 
#we do a bootstrap model to generate CI. If theses don't overlap 0 there has been a significant change
Delta.TNO <- as.numeric(tapply(deltaTNO.data$totaloffspring, deltaTNO.data$population, mean, na.rm = T))
#View Delta.TNO
Delta.TNO

#Δtotaloffspring LHm-Tx - LHm
Delta.TNO[2] - Delta.TNO[1]
# -36.71429

#Δtotaloffspring LHm-Ty - LHm
Delta.TNO[3] - Delta.TNO[1]
# -4.851648

#Δtotaloffspring LHm-Ix - LHm
Delta.TNO[4] - Delta.TNO[1]
# -33

#Δtotaloffspring LHm-Iy - LHm
Delta.TNO[5] - Delta.TNO[1]
# -24.57143

#Δtotaloffspring Inn-Lx - Innisfail
Delta.TNO[7] - Delta.TNO[6]
# -24.07143

#Δtotaloffspring Inn-Ly - Innisfail
Delta.TNO[8] - Delta.TNO[6]
# -8.571429

#Δtotaloffspring Inn-Ox - Innisfail
Delta.TNO[9] - Delta.TNO[6]
# -9.571429

#Δtotaloffspring Inn-Oy - Innisfail
Delta.TNO[10] - Delta.TNO[6]
# -43.07143

#Δtotaloffspring Odd-Ix - Odder
Delta.TNO[12] - Delta.TNO[11]
# -6.785714

#Δtotaloffspring Odd-Iy - Odder
Delta.TNO[13] - Delta.TNO[11]
# 21.78571

#Δtotaloffspring Odd-Dx - Odder
Delta.TNO[14] - Delta.TNO[11]
# -32.57143

#Δtotaloffspring Odd-Dy - Odder
Delta.TNO[15] - Delta.TNO[11]
# 5

#Δtotaloffspring Dah-Ox - Dahomey
Delta.TNO[17] - Delta.TNO[16]
# -86.07143

#Δtotaloffspring Dah-Oy - Dahomey
Delta.TNO[18] - Delta.TNO[16]
# -69.92857

#Δtotaloffspring Dah-Tx - Dahomey
Delta.TNO[19] - Delta.TNO[16]
# -48

#Δtotaloffspring Dah-Ty - Dahomey
Delta.TNO[20] - Delta.TNO[16]
# -31.42857

#Δtotaloffspring Tas-Dx - Tasmania
Delta.TNO[22] - Delta.TNO[21]
# -12.92857

#Δtotaloffspring Tas-Dy - Tasmania
Delta.TNO[23] - Delta.TNO[21]
# -19.28571

#Δtotaloffspring Tas-Lx - Tasmania
Delta.TNO[24] - Delta.TNO[21]
# -29.71429

#Δtotaloffspring Tas-Ly - Tasmania
Delta.TNO[25] - Delta.TNO[21]
# 3.914286


#TEST OF PROBAILITY OF SUCCESS. THE PROBABILITY OF POSITIVE VALUE
binom.test(3, 20, p = 0.5,  alternative = "two.sided")
#Significant, P = 0.002577


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


#Then we set up a bootstrap that resampels the data from 14 data points to calculate a new mean everytime for 10000 times
for (i in 1:10000){
  DATA <- do.call(rbind, lapply(split(deltaTNO.data, deltaTNO.data$population), function(x) x[sample(14, replace = T),])) 
  Delta.TNO <- as.numeric(tapply(DATA$totaloffspring, DATA$population, mean, na.rm = T))
  TxL[i] <- Delta.TNO[2] - Delta.TNO[1]  
  TyL[i] <- Delta.TNO[3] - Delta.TNO[1]
  IxL[i] <- Delta.TNO[4] - Delta.TNO[1]
  IyL[i] <- Delta.TNO[5] - Delta.TNO[1]
  LxI[i] <- Delta.TNO[7] - Delta.TNO[6]
  LyI[i] <- Delta.TNO[8] - Delta.TNO[6]
  OxI[i] <- Delta.TNO[9] - Delta.TNO[6]
  OyI[i] <- Delta.TNO[10] - Delta.TNO[6]
  IxO[i] <- Delta.TNO[12] - Delta.TNO[11]
  IyO[i] <- Delta.TNO[13] - Delta.TNO[11]
  DxO[i] <- Delta.TNO[14] - Delta.TNO[11]
  DyO[i] <- Delta.TNO[15] - Delta.TNO[11]
  OxD[i] <- Delta.TNO[17] - Delta.TNO[16]
  OyD[i] <- Delta.TNO[18] - Delta.TNO[16]
  TxD[i] <- Delta.TNO[19] - Delta.TNO[16]
  TyD[i] <- Delta.TNO[20] - Delta.TNO[16]
  DxT[i] <- Delta.TNO[22] - Delta.TNO[21] 
  DyT[i] <- Delta.TNO[23] - Delta.TNO[21] 
  LxT[i] <- Delta.TNO[24] - Delta.TNO[21]
  LyT[i] <- Delta.TNO[25] - Delta.TNO[21]  }
#Run the calculation


#95% CI for TxL
mean(TxL)
# -36.78346
mean(TxL) - (1.96 * sd(TxL))
# -76.86679
mean(TxL) + (1.96 * sd(TxL))
# 3.299862

#95% CI for TyL
mean(TyL)
# -4.9157
mean(TyL) - (1.96 * sd(TyL))
# -50.07679
mean(TyL) + (1.96 * sd(TyL))
# 40.24539

#95% CI for LxI
mean(LxI)
# -23.80489
mean(LxI) - (1.96 * sd(LxI))
# -74.36563
mean(LxI) + (1.96 * sd(LxI))
# 26.75585

#95% CI for IxL
mean(IxL)
# -33.17567
mean(IxL) - (1.96 * sd(IxL))
# -76.0868
mean(IxL) + (1.96 * sd(IxL))
# 9.735454

#95% CI for IyL
mean(IyL)
# -24.38087
mean(IyL) - (1.96 * sd(IyL))
# -73.21773
mean(IyL) + (1.96 * sd(IyL))
# 24.45598

#95% CI for LyI
mean(LyI)
# -8.684393
mean(LyI) - (1.96 * sd(LyI))
# -76.17019
mean(LyI) + (1.96 * sd(LyI))
# 58.80141

#95% CI for OxI
mean(OxI)
# -9.258586
mean(OxI) - (1.96 * sd(OxI))
# -59.10976
mean(OxI) + (1.96 * sd(OxI))
# 40.59259

#95% CI for OyI
mean(OyI)
# -42.72323
mean(OyI) - (1.96 * sd(OyI))
# -104.4915
mean(OyI) + (1.96 * sd(OyI))
# 19.045

#95% CI for IxO
mean(IxO)
# -7.202971
mean(IxO) - (1.96 * sd(IxO))
# -67.71962
mean(IxO) + (1.96 * sd(IxO))
# 53.31368

#95% CI for IyO
mean(IyO)
# 21.76034
mean(IyO) - (1.96 * sd(IyO))
# -31.65694
mean(IyO) + (1.96 * sd(IyO))
# 75.17763

#95% CI for DxO
mean(DxO)
# -32.82243
mean(DxO) - (1.96 * sd(DxO))
# -106.7041
mean(DxO) + (1.96 * sd(DxO))
# 41.05928

#95% CI for DyO
mean(DyO)
# 5.048029
mean(DyO) - (1.96 * sd(DyO))
# -56.59903
mean(DyO) + (1.96 * sd(DyO))
# 66.69509

#95% CI for OxD
mean(OxD)
# -86.18716
mean(OxD) - (1.96 * sd(OxD))
# -141.7169
mean(OxD) + (1.96 * sd(OxD))
# -30.65743

#95% CI for OyD
mean(OyD)
# -69.68046
mean(OyD) - (1.96 * sd(OyD))
# -126.6728
mean(OyD) + (1.96 * sd(OyD))
# -12.68808

#95% CI for TxD
mean(TxD)
# -47.49759
mean(TxD) - (1.96 * sd(TxD))
# -110.1723
mean(TxD) + (1.96 * sd(TxD))
# 15.17709

#95% CI for TyD
mean(TyD)
# -31.25119
mean(TyD) - (1.96 * sd(TyD))
# -93.48486
mean(TyD) + (1.96 * sd(TyD))
# 30.98249

#95% CI for DxT
mean(DxT)
# -12.50296
mean(DxT) - (1.96 * sd(DxT))
# -81.98265
mean(DxT) + (1.96 * sd(DxT))
# 56.97673

#95% CI for DyT
mean(DyT)
# -19.25586
mean(DyT) - (1.96 * sd(DyT))
# -81.32143
mean(DyT) + (1.96 * sd(DyT))
# 42.8097

#95% CI for LxT
mean(LxT)
# -29.36675
mean(LxT) - (1.96 * sd(LxT))
# -86.22828
mean(LxT) + (1.96 * sd(LxT))
# 27.49478

#95% CI for LyT
mean(LyT)
# 4.301448
mean(LyT) - (1.96 * sd(LyT))
# -57.89822
mean(LyT) + (1.96 * sd(LyT))
# 66.50111



#########################################  PLOT DATA  #########################################


#To be able to plot the bootstrap data as boxplots I create a new data frame with the data

#First I make a vector with the populations
population <- c( rep("aTasX-LHm", 10000), rep("bTasY-LHm", 10000), rep("cInnX-LHm", 10000), rep("dInnY-LHm", 10000),
                 rep("eLHmX-Inn", 10000), rep("fLHmY-Inn", 10000), rep("gOddX-Inn", 10000), rep("hOddY-Inn", 10000),
                 rep("iInnX-Odd", 10000), rep("jInnY-Odd", 10000), rep("kDahX-Odd", 10000), rep("lDahY-Odd", 10000),
                 rep("mOddX-Dah", 10000), rep("nOddY-Dah", 10000), rep("oTasX-Dah", 10000), rep("pTasY-Dah", 10000),
                 rep("qDahX-Tas", 10000), rep("rDahY-Tas", 10000), rep("sLHmX-Tas", 10000), rep("tLHmY-Tas", 10000))

#Then I collcet all the bootstrap data in a new vector
deltaTotalOff <- c(TxL, TyL, IxL, IyL, LxI, LyI, OxI, OyI,
                   IxO, IyO, DxO, DyO, OxD, OyD, TxD, TyD, DxT, DyT, LxT, LyT)

#Then it's all collceted in a new data frame
DTNO <- data.frame(population, deltaTotalOff)

#And write it into a new file
write.csv(DTNO, file = "DeltaTotalNumberOffspringPlot.csv") 



#Read in csv file with data
DTNOplot.data <- read.table(file = "DeltaTotalNumberOffspringPlot.csv", h = T, sep = ",")

#MEAN
meanDTNO <- tapply(DTNOplot.data$deltaTotalOff, DTNOplot.data$population, mean, na.rm = T)
#SD
sdDTNO <- tapply(DTNOplot.data$deltaTotalOff, DTNOplot.data$population, sd, na.rm = T)


#Plot
par(mar = c(6, 5, 2, 2))
#Plot errorbars
xDTNO <- c(0.5,1,1.5,2, 3,3.5,4,4.5, 5.5,6,6.5,7, 8,8.5,9,9.5, 10.5,11,11.5,12)
errbar(xDTNO, meanDTNO, meanDTNO + (1.96 * sdDTNO), meanDTNO - (1.96 * sdDTNO), 
       xlim = c(0.3, 12.2), xlab = "", xaxt = "n", ylim = c(-150, 100), ylab = expression(Delta~"Total number of offspring"), 
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
mtext(expression("LH"["M"]), side = 1, line = 3, at = 1.25, cex = 1.6)
mtext(expression(italic("Innisfail")), side = 1, line = 3, at = 3.75, cex = 1.6)
mtext(expression(italic("Odder")), side = 1, line = 3, at = 6.25, cex = 1.6)
mtext(expression(italic("Dahomey")), side = 1, line = 3, at = 8.75, cex = 1.6)
mtext(expression(italic("Tasmania")), side = 1, line = 3, at = 11.25, cex = 1.6)
#Now add arrows to show significance
points(8, 95, pch = "*", bg = "black", cex = 2.5)
points(8.5, 95, pch = "*", bg = "black", cex = 2.5)


