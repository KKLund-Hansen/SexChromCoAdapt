################################################################################################
############################ ΔNOVEL XY POPULATION MALE FITNESS ASSAY ###########################
################################################################################################

#Set up environment
library(Hmisc)

#Read in csv file with data
deltaNXYMFA.data <- read.table(file = "NovelXYMaleFitnessAssay.csv", header = T, sep = ",")


##########################################  STATISTIC  #########################################


### CALCULATE RELATIVE FITNESS ###

#Calculate the proportion of red eyed offspring
deltaNXYMFA.data$prop_red <- deltaNXYMFA.data$red / deltaNXYMFA.data$total
#Now divid each proportion red by 5 to get the number for each male
deltaNXYMFA.data$prop_red_male <- deltaNXYMFA.data$prop_red / 5
#Find maxium
maxDeltaNXYMFA <- max(deltaNXYMFA.data$prop_red_male, na.rm = T)
#Calculate relative fecundity by dividing each proportion by the maximum
deltaNXYMFA.data$relative_fit <- deltaNXYMFA.data$prop_red_male / maxDeltaNXYMFA


### ΔFITNESS CALCULATIONS ###
#To test if the change of sex chromosomes is significantly from the wild type population, 
#we do a bootstrap model to generate CI. If theses don't overlap 0 there has been a significant change
Delta.NXYMFA <- as.numeric(tapply(deltaNXYMFA.data$relative_fit, deltaNXYMFA.data$population, mean, na.rm = T))
#View Delta.NXYMFA
Delta.NXYMFA

#Δfitness LHm-Ixy - LHm
Delta.NXYMFA[2] - Delta.NXYMFA[1]
# -0.06218053

#Δfitness Inn-Lxy - Innisfail
Delta.NXYMFA[4] - Delta.NXYMFA[3]
# -0.07223928

#Δfitness Inn-Oxy - Innisfail
Delta.NXYMFA[5] - Delta.NXYMFA[3]
# 0.05641598

#Δfitness Odd-Ixy - Odder
Delta.NXYMFA[7] - Delta.NXYMFA[6]
# -0.0864333

#Δfitness Odd-Dxy - Odder
Delta.NXYMFA[8] - Delta.NXYMFA[6]
# 0.01137604

#Δfitness Dah-Oxy - Dahomey
Delta.NXYMFA[10] - Delta.NXYMFA[9]
# 0.08387182


#TEST OF PROBABILITY OF SUCCESS. THE PROBABILITY OF POSITIVE VALUE
binom.test(3, 6, p = 0.5)
#Non-significant, P = 1


#First we make new vector to collect the data
IxyL <- numeric(10000)
LxyI <- numeric(10000)
OxyI <- numeric(10000)
IxyO <- numeric(10000)
DxyO <- numeric(10000)
OxyD <- numeric(10000)


#Then we set up a bootstrap that resampels the data from 10 data points to calculate a new mean everytime for 10000 times
for (i in 1:10000){
  DATA <- do.call(rbind, lapply(split(deltaNXYMFA.data,deltaNXYMFA.data$population), function(x) x[sample(10, replace = T),])) 
  Delta.NXYMFA <- as.numeric(tapply(DATA$relative_fit, DATA$population, mean, na.rm = T))
  IxyL[i] <- Delta.NXYMFA[2] - Delta.NXYMFA[1]
  LxyI[i] <- Delta.NXYMFA[4] - Delta.NXYMFA[3]
  OxyI[i] <- Delta.NXYMFA[5] - Delta.NXYMFA[3]
  IxyO[i] <- Delta.NXYMFA[7] - Delta.NXYMFA[6]
  DxyO[i] <- Delta.NXYMFA[8] - Delta.NXYMFA[6]
  OxyD[i] <- Delta.NXYMFA[10] - Delta.NXYMFA[9]  }
#Run the calculation

#95% CI for IxyL
mean(IxyL)
# -0.06272451
mean(IxyL) - (1.96 * sd(IxyL))
# -0.2462737
mean(IxyL) + (1.96 * sd(IxyL))
# 0.1208247

#95% CI for LxyI
mean(LxyI)
# -0.071664
mean(LxyI) - (1.96 * sd(LxyI))
# -0.2307042
mean(LxyI) + (1.96 * sd(LxyI))
# 0.08737622

#95% CI for OxyI
mean(OxyI)
# 0.05517487
mean(OxyI) - (1.96 * sd(OxyI))
# -0.08560979
mean(OxyI) + (1.96 * sd(OxyI))
# 0.1959595

#95% CI for IxyO
mean(IxyO)
# -0.08753541
mean(IxyO) - (1.96 * sd(IxyO))
# -0.2613764
mean(IxyO) + (1.96 * sd(IxyO))
# 0.08630562

#95% CI for DxyO
mean(DxyO)
# 0.01125927
mean(DxyO) - (1.96 * sd(DxyO))
# -0.1476572
mean(DxyO) + (1.96 * sd(DxyO))
# 0.1701757

#95% CI for OxyD
mean(OxyD)
# 0.08474183
mean(OxyD) - (1.96 * sd(OxyD))
# -0.06108011
mean(OxyD) + (1.96 * sd(OxyD))
# 0.2305638


#########################################  PLOT DATA  #########################################


#To be able to plot the bootstrap data as boxplots I create a new data frame with the data

#First I make a vector with the populations
population <- c(rep("aInnXY-LHm", 10000), rep("bLHmXY-Inn", 10000), rep("cOddXY-Inn", 10000),
               rep("dInnXY-Odd", 10000), rep("eDahXY-Odd", 10000), rep("fOddXY-Dah", 10000))

#Then I collcet all the bootstrap data in a new vector
deltafitness <- c(IxyL, LxyI, OxyI, IxyO, DxyO, OxyD)

#Then it's all collceted in a new data frame
DNXYMFA <- data.frame(population, deltafitness)

#And write it into a new file
write.csv(DNXYMFA, file = "DeltaNovelXYMaleFitnessAssay.csv") 


#Read in csv file with data
DeltaNXYMFAplot.data <- read.table(file = "DeltaNovelXYMaleFitnessAssay.csv", h = T, sep = ",")

#MEAN
meanDNXYMFA <- tapply(DeltaNXYMFAplot.data$deltafitness, DeltaNXYMFAplot.data$population, mean, na.rm = T)
#SD
sdDNXYMFA <- tapply(DeltaNXYMFAplot.data$deltafitness, DeltaNXYMFAplot.data$population, sd, na.rm = T)


#Plot
par(mar = c(6, 5, 2, 2))
#Plot errorbars
xDNXYMFA <- c(0.5, 1.5,2, 3,3.5, 4.5)
errbar(xDNXYMFA, meanDNXYMFA, meanDNXYMFA + (1.96 * sdDNXYMFA), meanDNXYMFA - (1.96 * sdDNXYMFA), 
       xlim = c(0.3, 4.7), xlab = "", xaxt = "n", ylim = c(-0.3, 0.3), ylab = expression(Delta~"Fitness"), 
       cex.axis = 1.2, cex.lab = 1.6, las = 1,  pch = 15, cex = 3, lwd = 3)
#AXIS
axis(1, at = c(0.5, 1.5,2, 3,3.5, 4.5), cex.axis = 1.5,
     labels = c(expression("I"["XY"]), expression("L"["XY"]), expression("O"["XY"]),
                expression("I"["XY"]), expression("D"["XY"]), expression("O"["XY"])))
#Add line at 0
abline(h = 0, lty = 2, lwd = 2)
#And add text on top
mtext(expression("LH"["M"]), side = 1, line = 3, at = 0.5, cex = 1.6)
mtext(expression(italic("Innisfail")), side = 1, line = 3, at = 1.75, cex = 1.6)
mtext(expression(italic("Odder")), side = 1, line = 3, at = 3.25, cex = 1.6)
mtext(expression(italic("Dahomey")), side = 1, line = 3, at = 4.5, cex = 1.6)


