################################################################################################
###################################### ΔSPERM COMPETITION ######################################
################################################################################################

#Set up environment
library(Hmisc)

#Read in csv file with data
deltaSC.data <- read.table(file = "SpermCompetition.csv", h = T, sep = ",")


##########################################  STATISTIC  #########################################


### REMOVE OUTLIERS ###
outlierDeltaSC <- deltaSC.data$total[which((abs(deltaSC.data$total - median(deltaSC.data$total)) / mad(deltaSC.data$total)) > 2)]
deltaSC.data$total[deltaSC.data$total %in% outlierDeltaSC[8]] <- NA
deltaSC.data$red[is.na(deltaSC.data$total)] <- NA


### CALCULATE OFFSPRING PROPOTION ###
#Calculate the proportion of red eyed offspring
deltaSC.data$prop_red <- deltaSC.data$red / deltaSC.data$total



### ΔSPERM COMPETITION CALCULATIONS ###
#To test if the change of sex chromosomes is significantly from the wild type population, 
#we do a bootstrap model to generate CI. If theses don't overlap 0 there has been a significant change
Delta.SC <- as.numeric(tapply(deltaSC.data$prop_red, deltaSC.data$population, mean, na.rm = T))
#View Delta.XYSC
Delta.SC


#ΔSC Inn-Lx - Innisfail
Delta.SC[3] - Delta.SC[2]
# 0.08912923

#ΔSC Inn-Ly - Innisfail
Delta.SC[4] - Delta.SC[2]
# 0.08483919

#ΔSC Inn-Ox - Innisfail
Delta.SC[5] - Delta.SC[2]
# 0.04548587

#ΔSC Inn-Oy - Innisfail
Delta.SC[6] - Delta.SC[2]
# 0.07687127

#ΔSC Odd-Ix - Odder
Delta.SC[8] - Delta.SC[7]
# -0.04754365

#ΔSC Odd-Iy - Odder
Delta.SC[9] - Delta.SC[7]
# -0.03284103

#ΔSC Odd-Dx - Odder
Delta.SC[10] - Delta.SC[7]
# -6.694194e-05

#ΔSC Odd-Dy - Odder
Delta.SC[11] - Delta.SC[7]
# 0.0008132876


#TEST OF PROBABILITY OF SUCCESS. THE PROBABILITY OF POSITIVE VALUE
binom.test(5, 8, p = 0.5)
#Non-significant, P = 0.7266


#First we make new vector to collect the data
LxI <- numeric(10000)
LyI <- numeric(10000)
OxI <- numeric(10000)
OyI <- numeric(10000)
IxO <- numeric(10000)
IyO <- numeric(10000)
DxO <- numeric(10000)
DyO <- numeric(10000)


#Then we set up a bootstrap that resampels the data from 14 data points to calculate a new mean everytime for 10000 times
for (i in 1:10000){
  DATA <- do.call(rbind, lapply(split(deltaSC.data, deltaSC.data$population), function(x) x[sample(14, replace = T),])) 
  Delta.SC <- as.numeric(tapply(DATA$prop_red, DATA$population, mean, na.rm = T))
  LxI[i] <- Delta.SC[3] - Delta.SC[2]
  LyI[i] <- Delta.SC[4] - Delta.SC[2]
  OxI[i] <- Delta.SC[5] - Delta.SC[2] 
  OyI[i] <- Delta.SC[6] - Delta.SC[2]
  IxO[i] <- Delta.SC[8] - Delta.SC[7]
  IyO[i] <- Delta.SC[9] - Delta.SC[7]
  DxO[i] <- Delta.SC[10] - Delta.SC[7]
  DyO[i] <- Delta.SC[11] - Delta.SC[7]   }
#Run the calculation


#95% CI for LxI
mean(LxI)
# 0.08931466
mean(LxI) - (1.96 * sd(LxI))
# 0.0006723725
mean(LxI) + (1.96 * sd(LxI))
# 0.1779569

#95% CI for LyI
mean(LyI)
# 0.08484769
mean(LyI) - (1.96 * sd(LyI))
# -0.0005407847
mean(LyI) + (1.96 * sd(LyI))
# 0.1702362

#95% CI for OxI
mean(OxI)
# 0.04557752
mean(OxI) - (1.96 * sd(OxI))
# -0.03883099
mean(OxI) + (1.96 * sd(OxI))
# 0.129986

#95% CI for OyI
mean(OyI)
# 0.07679276
mean(OyI) - (1.96 * sd(OyI))
# -0.007294754
mean(OyI) + (1.96 * sd(OyI))
# 0.1608803

#95% CI for IxO
mean(IxO)
# -0.0463244
mean(IxO) - (1.96 * sd(IxO))
# -0.1172031
mean(IxO) + (1.96 * sd(IxO))
# 0.02455435

#95% CI for IyO
mean(IyO)
# -0.03282893
mean(IyO) - (1.96 * sd(IyO))
# -0.08597629
mean(IyO) + (1.96 * sd(IyO))
# 0.02031843

#95% CI for DxO
mean(DxO)
# -3.762709e-05
mean(DxO) - (1.96 * sd(DxO))
# -0.05257917
mean(DxO) + (1.96 * sd(DxO))
# 0.05250391

#95% CI for DyO
mean(DyO)
# 0.0008654817
mean(DyO) - (1.96 * sd(DyO))
# -0.04299034
mean(DyO) + (1.96 * sd(DyO))
# 0.04472131


#########################################  PLOT DATA  #########################################


#To be able to plot the bootstrap data as boxplots I create a new data frame with the data

#First I make a vector with the populations
population <- c( rep("aLHmX-Inn", 10000), rep("bLHmY-Inn", 10000), rep("cOddX-Inn", 10000), rep("dOddY-Inn", 10000),
                 rep("eInnX-Odd", 10000), rep("fInnY-Odd", 10000), rep("gDahX-Odd", 10000), rep("hDahY-Odd", 10000))

#Then I collcet all the bootstrap data in a new vector
deltaprop_red <- c(LxI, LyI, OxI, OyI, IxO, IyO, DxO, DyO)

#Then it's all collceted in a new data frame
DSC <- data.frame(population, deltaprop_red)

#And write it into a new file
write.csv(DSC, file = "DeltaSpermCompetitionPlot.csv") 



#Read in csv file with data
DSCplot.data <- read.table(file = "DeltaSpermCompetitionPlot.csv", h = T, sep = ",")

#MEAN
meanDSC <- tapply(DSCplot.data$deltaprop_red, DSCplot.data$population, mean, na.rm = T)
#SD
sdDSC <- tapply(DSCplot.data$deltaprop_red, DSCplot.data$population, sd, na.rm = T)

#Plot
par(mar = c(6, 5, 2, 2))
#Plot errorbars
xDSC <- c(0.5, 1, 1.5, 2, 3, 3.5, 4, 4.5)
errbar(xDSC, meanDSC, meanDSC + (1.96 * sdDSC), meanDSC - (1.96 * sdDSC), 
       xlim = c(0.3, 4.7), xlab = "", xaxt = "n", ylim = c(-0.15, 0.2), ylab = expression(Delta~"Proportion of offspring"), 
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


