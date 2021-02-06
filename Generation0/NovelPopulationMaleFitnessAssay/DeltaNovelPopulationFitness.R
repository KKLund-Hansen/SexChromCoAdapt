################################################################################################
############################# ΔNOVEL POPULATION MALE FITNESS ASSAY #############################
################################################################################################

#Set up environment
library(Hmisc)

#Read in csv file with data
deltaNPMFA.data <- read.table(file = "NovelPopMaleFitnessAssay.csv", h = T, sep = ",")


##########################################  STATISTIC  #########################################


### CALCULATE RELATIVE FITNESS ###

#Calculate the proportion of red eyed offspring
deltaNPMFA.data$prop_red <- deltaNPMFA.data$red / deltaNPMFA.data$total
#Now divid each proportion red by 5 to get the number for each male
deltaNPMFA.data$prop_red_male <- deltaNPMFA.data$prop_red / 5
#Find maxium
maxDeltaNPMFA <- max(deltaNPMFA.data$prop_red_male, na.rm = T)
#Calculate relative fecundity by dividing each proportion by the maximum
deltaNPMFA.data$relative_fit <- deltaNPMFA.data$prop_red_male / maxDeltaNPMFA


### ΔFITNESS CALCULATIONS ###
#To test if the change of sex chromosomes is significantly from the wild type population, 
#we do a bootstrap model to generate CI. If theses don't overlap 0 there has been a significant change
Delta.NPMFA <- as.numeric(tapply(deltaNPMFA.data$relative_fit, deltaNPMFA.data$population, mean, na.rm = T))
#View Delta.NPMFA
Delta.NPMFA

#Δfitness LHm-Tx - LHm
Delta.NPMFA[2] - Delta.NPMFA[1]
# -0.04866418

#Δfitness LHm-Ty - LHm
Delta.NPMFA[3] - Delta.NPMFA[1]
# -0.04279381

#Δfitness LHm-Ix - LHm
Delta.NPMFA[4] - Delta.NPMFA[1]
# -0.0006907959

#Δfitness LHm-Iy - LHm
Delta.NPMFA[5] - Delta.NPMFA[1]
# 0.1415805

#Δfitness Inn-Lx - Innisfail
Delta.NPMFA[7] - Delta.NPMFA[6]
# 0.1649185

#Δfitness Inn-Ly - Innisfail
Delta.NPMFA[8] - Delta.NPMFA[6]
# 0.2190241

#Δfitness Inn-Ox - Innisfail
Delta.NPMFA[9] - Delta.NPMFA[6]
# 0.2691877

#Δfitness Inn-Oy - Innisfail
Delta.NPMFA[10] - Delta.NPMFA[6]
# 0.1094397

#Δfitness Odd-Ix - Odder
Delta.NPMFA[12] - Delta.NPMFA[11]
# 0.1489163

#Δfitness Odd-Iy - Odder
Delta.NPMFA[13] - Delta.NPMFA[11]
# 0.1446568

#Δfitness Odd-Dx - Odder
Delta.NPMFA[14] - Delta.NPMFA[11]
# 0.2692053

#Δfitness Odd-Dy - Odder
Delta.NPMFA[15] - Delta.NPMFA[11]
# 0.09538792

#Δfitness Dah-Ox - Dahomey
Delta.NPMFA[17] - Delta.NPMFA[16]
# -0.008030504

#Δfitness Dah-Oy - Dahomey
Delta.NPMFA[18] - Delta.NPMFA[16]
# 0.1007429

#Δfitness Dah-Tx - Dahomey
Delta.NPMFA[19] - Delta.NPMFA[16]
# 0.04386647

#Δfitness Dah-Ty - Dahomey
Delta.NPMFA[20] - Delta.NPMFA[16]
# 0.01889888

#Δfitness Tas-Dx - Tasmania
Delta.NPMFA[22] - Delta.NPMFA[21]
# 0.094155

#Δfitness Tas-Dy - Tasmania
Delta.NPMFA[23] - Delta.NPMFA[21]
# 0.01696604

#Δfitness Tas-Lx - Tasmania
Delta.NPMFA[24] - Delta.NPMFA[21]
# 0.06723024

#Δfitness Tas-Ly - Tasmania
Delta.NPMFA[25] - Delta.NPMFA[21]
# 0.003904319


#TEST OF PROBABILITY OF SUCCESS. THE PROBABILITY OF POSITIVE VALUE
binom.test(16, 20, p = 0.5,  alternative = "two.sided")
#Significant, P = 0.01182


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
  DATA <- do.call(rbind, lapply(split(deltaNPMFA.data, deltaNPMFA.data$population), function(x) x[sample(14, replace = T),])) 
  Delta.NPMFA <- as.numeric(tapply(DATA$relative_fit, DATA$population, mean, na.rm = T))
  TxL[i] <- Delta.NPMFA[2] - Delta.NPMFA[1]
  TyL[i] <- Delta.NPMFA[3] - Delta.NPMFA[1]
  IxL[i] <- Delta.NPMFA[4] - Delta.NPMFA[1]
  IyL[i] <- Delta.NPMFA[5] - Delta.NPMFA[1]
  LxI[i] <- Delta.NPMFA[7] - Delta.NPMFA[6]
  LyI[i] <- Delta.NPMFA[8] - Delta.NPMFA[6]
  OxI[i] <- Delta.NPMFA[9] - Delta.NPMFA[6]
  OyI[i] <- Delta.NPMFA[10] - Delta.NPMFA[6]
  IxO[i] <- Delta.NPMFA[12] - Delta.NPMFA[11]
  IyO[i] <- Delta.NPMFA[13] - Delta.NPMFA[11]
  DxO[i] <- Delta.NPMFA[14] - Delta.NPMFA[11]
  DyO[i] <- Delta.NPMFA[15] - Delta.NPMFA[11]
  OxD[i] <- Delta.NPMFA[17] - Delta.NPMFA[16]
  OyD[i] <- Delta.NPMFA[18] - Delta.NPMFA[16]
  TxD[i] <- Delta.NPMFA[19] - Delta.NPMFA[16]
  TyD[i] <- Delta.NPMFA[20] - Delta.NPMFA[16]
  DxT[i] <- Delta.NPMFA[22] - Delta.NPMFA[21] 
  DyT[i] <- Delta.NPMFA[23] - Delta.NPMFA[21]
  LxT[i] <- Delta.NPMFA[24] - Delta.NPMFA[21]
  LyT[i] <- Delta.NPMFA[25] - Delta.NPMFA[21]  }
#Run the calculation


#95% CI for TxL
mean(TxL)
# -0.04948905
mean(TxL) - (1.96 * sd(TxL))
# -0.1497056
mean(TxL) + (1.96 * sd(TxL))
# 0.05072747

#95% CI for TyL
mean(TyL)
# -0.04319904
mean(TyL) - (1.96 * sd(TyL))
# -0.165779
mean(TyL) + (1.96 * sd(TyL))
# 0.07938089

#95% CI for IxL
mean(IxL)
# -0.001122934
mean(IxL) - (1.96 * sd(IxL))
# -0.1052037
mean(IxL) + (1.96 * sd(IxL))
# 0.1029578

#95% CI for IyL
mean(IyL)
# 0.140847
mean(IyL) - (1.96 * sd(IyL))
# 0.003456123
mean(IyL) + (1.96 * sd(IyL))
# 0.2782379

#95% CI for LxI
mean(LxI)
# 0.1651164
mean(LxI) - (1.96 * sd(LxI))
# 0.08308853
mean(LxI) + (1.96 * sd(LxI))
# 0.2471443

#95% CI for LyI
mean(LyI)
# 0.2183714
mean(LyI) - (1.96 * sd(LyI))
# 0.1187812
mean(LyI) + (1.96 * sd(LyI))
# 0.3179616

#95% CI for OxI
mean(OxI)
# 0.2688582
mean(OxI) - (1.96 * sd(OxI))
# 0.1696033
mean(OxI) + (1.96 * sd(OxI))
# 0.3681132

#95% CI for OyI
mean(OyI)
# 0.1092678
mean(OyI) - (1.96 * sd(OyI))
# 0.02242589
mean(OyI) + (1.96 * sd(OyI))
# 0.1961098

#95% CI for IxO
mean(IxO)
# 0.1484633
mean(IxO) - (1.96 * sd(IxO))
# 0.06301941
mean(IxO) + (1.96 * sd(IxO))
# 0.2339072

#95% CI for IyO
mean(IyO)
# 0.1436338
mean(IyO) - (1.96 * sd(IyO))
# 0.05236349
mean(IyO) + (1.96 * sd(IyO))
# 0.234904

#95% CI for DxO
mean(DxO)
# 0.2688785
mean(DxO) - (1.96 * sd(DxO))
# 0.1625217
mean(DxO) + (1.96 * sd(DxO))
# 0.3752353

#95% CI for DyO
mean(DyO)
# 0.09533576
mean(DyO) - (1.96 * sd(DyO))
# 0.01075828
mean(DyO) + (1.96 * sd(DyO))
# 0.1799132

#95% CI for OxD
mean(OxD)
# -0.007890688
mean(OxD) - (1.96 * sd(OxD))
# -0.1087376
mean(OxD) + (1.96 * sd(OxD))
# 0.09295622

#95% CI for OyD
mean(OyD)
# 0.1007924
mean(OyD) - (1.96 * sd(OyD))
# -0.004269481
mean(OyD) + (1.96 * sd(OyD))
# 0.2058542

#95% CI for TxD
mean(TxD)
# 0.04430345
mean(TxD) - (1.96 * sd(TxD))
# -0.05350775
mean(TxD) + (1.96 * sd(TxD))
# 0.1421147

#95% CI for TyD
mean(TyD)
# 0.01917914
mean(TyD) - (1.96 * sd(TyD))
# -0.08053894
mean(TyD) + (1.96 * sd(TyD))
# 0.1188972

#95% CI for DxT
mean(DxT)
# 0.09495636
mean(DxT) - (1.96 * sd(DxT))
# -0.06894009
mean(DxT) + (1.96 * sd(DxT))
# 0.2588528

#95% CI for DyT
mean(DyT)
# 0.01750676
mean(DyT) - (1.96 * sd(DyT))
# -0.1384714
mean(DyT) + (1.96 * sd(DyT))
# 0.1734849

#95% CI for LxT
mean(LxT)
# 0.06763113
mean(LxT) - (1.96 * sd(LxT))
# -0.08080128
mean(LxT) + (1.96 * sd(LxT))
# 0.2160635

#95% CI for LyT
mean(LyT)
# 0.004324323
mean(LyT) - (1.96 * sd(LyT))
# -0.1563697
mean(LyT) + (1.96 * sd(LyT))
# 0.1650184


#########################################  PLOT DATA  #########################################


#To be able to plot the bootstrap data as boxplots I create a new data frame with the data

#First I make a vector with the populations
population <- c( rep("aTasX-LHm", 10000), rep("bTasY-LHm", 10000), rep("cInnX-LHm", 10000), rep("dInnY-LHm", 10000), 
                 rep("eLHmX-Inn", 10000), rep("fLHmY-Inn", 10000), rep("gOddX-Inn", 10000), rep("hOddY-Inn", 10000),
                 rep("iInnX-Odd", 10000), rep("jInnY-Odd", 10000), rep("kDahX-Odd", 10000), rep("lDahY-Odd", 10000),
                 rep("mOddX-Dah", 10000), rep("nOddY-Dah", 10000), rep("oTasX-Dah", 10000), rep("pTasY-Dah", 10000),
                 rep("qDahX-Tas", 10000), rep("rDahY-Tas", 10000), rep("sLHmX-Tas", 10000), rep("tLHmY-Tas", 10000))

#Then I collcet all the bootstrap data in a new vector
deltafitness <- c(TxL, TyL, IxL, IyL, LxI, LyI, OxI, OyI,
                  IxO, IyO, DxO, DyO, OxD, OyD, TxD, TyD, DxT, DyT, LxT, LyT)

#Then it's all collceted in a new data frame
DFA <- data.frame(population, deltafitness)

#And write it into a new file
write.csv(DFA, file = "DeltaNovelPopulationFitnessPlot.csv") 


#Read in csv file with data
DNPMFAplot.data <- read.table(file = "DeltaNovelPopulationFitnessPlot.csv", h = T, sep = ",")

#MEAN
meanDNPMFA <- tapply(DNPMFAplot.data$deltafitness, DNPMFAplot.data$population, mean, na.rm = T)
#SD
sdDNPMFA <- tapply(DNPMFAplot.data$deltafitness, DNPMFAplot.data$population, sd, na.rm = T)


#Plot
par(mar = c(6, 5, 2, 2))
#Plot errorbars
xDNPMFA <- c(0.5,1,1.5,2, 3,3.5,4,4.5, 5.5,6,6.5,7, 8,8.5,9,9.5, 10.5,11,11.5,12)
errbar(xDNPMFA, meanDNPMFA, meanDNPMFA + (1.96 * sdDNPMFA), meanDNPMFA - (1.96 * sdDNPMFA), 
       xlim = c(0.3, 12.2), xlab = "", xaxt = "n", ylim = c(-0.2, 0.4), ylab = expression(Delta~"Fitness"), 
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
points(2, 0.4, pch = "*", bg = "black", cex = 2.5)
points(3, 0.4, pch = "*", bg = "black", cex = 2.5)
points(3.5, 0.4, pch = "*", bg = "black", cex = 2.5)
points(4, 0.4, pch = "*", bg = "black", cex = 2.5)
points(4.5, 0.4, pch = "*", bg = "black", cex = 2.5)
points(5.5, 0.4, pch = "*", bg = "black", cex = 2.5)
points(6, 0.4, pch = "*", bg = "black", cex = 2.5)
points(6.5, 0.4, pch = "*", bg = "black", cex = 2.5)
points(7, 0.4, pch = "*", bg = "black", cex = 2.5)


