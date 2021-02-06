################################################################################################
###################### ΔNOVEL POPULATION MALE FITNESS ASSAY GENERATION 25 ######################
################################################################################################

#Set up environment
library(Hmisc)

#Read in csv file with data
DeltaNPFAg25.data <- read.table(file = "NovelPopMaleFitnessAssayG25.csv", h = T, sep = ",")


##########################################  STATISTIC  #########################################


### CALCULATE RELATIVE FITNESS ###

#Calculate the proportion of red eyed offspring
DeltaNPFAg25.data$prop_red <- DeltaNPFAg25.data$red / DeltaNPFAg25.data$total
#Now divid each proportion red by 5 to get the number for each male
DeltaNPFAg25.data$prop_red_male <- DeltaNPFAg25.data$prop_red / 5
#Find maxium
maxDeltaNPFAg25 <- max(DeltaNPFAg25.data$prop_red_male, na.rm = T)
#Calculate relative fitness by dividing each proportion by the maximum
DeltaNPFAg25.data$relative_fit <- DeltaNPFAg25.data$prop_red_male / maxDeltaNPFAg25


### ΔFITNESS CALCULATIONS ###
#To test if the change of sex chromosomes is significantly from the wild type population, 
#we do a bootstrap model to generate CI. If theses don't overlap 0 there has been a significant change
Delta.NPFAg25 <- as.numeric(tapply(DeltaNPFAg25.data$relative_fit, DeltaNPFAg25.data$population, mean, na.rm = T))
#View Delta.NPFAg25
Delta.NPFAg25


#Δfitness Inn-Lx - Innisfail
Delta.NPFAg25[3] - Delta.NPFAg25[2]
# 0.07378461

#Δfitness Inn-Ly - Innisfail
Delta.NPFAg25[4] - Delta.NPFAg25[2]
# 0.08668395

#Δfitness Inn-Ox - Innisfail
Delta.NPFAg25[5] - Delta.NPFAg25[2]
# 0.1420969

#Δfitness Inn-Oy - Innisfail
Delta.NPFAg25[6] - Delta.NPFAg25[2]
# 0.100319

#Δfitness Odd-Ix - Odder
Delta.NPFAg25[8] - Delta.NPFAg25[7]
# -0.04548151

#Δfitness Odd-Iy - Odder
Delta.NPFAg25[9] - Delta.NPFAg25[7]
# 0.01631576

#Δfitness Odd-Dx - Odder
Delta.NPFAg25[10] - Delta.NPFAg25[7]
# 0.03387821

#Δfitness Odd-Dy - Odder
Delta.NPFAg25[11] - Delta.NPFAg25[7]
# 0.03722847


#TEST OF PROBABILITY OF SUCCESS. THE PROBABILITY OF POSITIVE VALUE
binom.test(7, 8, p = 0.5,  alternative = "two.sided")
#Not significant, P = 0.07031


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
  DATA <- do.call(rbind, lapply(split(DeltaNPFAg25.data, DeltaNPFAg25.data$population), function(x) x[sample(12, replace = T),])) 
  Delta.NPFAg25 <- as.numeric(tapply(DATA$relative_fit, DATA$population, mean, na.rm = T))
  LxI[i] <- Delta.NPFAg25[3] - Delta.NPFAg25[2]
  LyI[i] <- Delta.NPFAg25[4] - Delta.NPFAg25[2]
  OxI[i] <- Delta.NPFAg25[5] - Delta.NPFAg25[2]
  OyI[i] <- Delta.NPFAg25[6] - Delta.NPFAg25[2]
  IxO[i] <- Delta.NPFAg25[8] - Delta.NPFAg25[7]
  IyO[i] <- Delta.NPFAg25[9] - Delta.NPFAg25[7]
  DxO[i] <- Delta.NPFAg25[10] - Delta.NPFAg25[7]
  DyO[i] <- Delta.NPFAg25[11] - Delta.NPFAg25[7]  }
#Run the calculation


#95% CI for LxI
mean(LxI)
# 0.07305206
mean(LxI) - (1.96 * sd(LxI))
# -0.04072608
mean(LxI) + (1.96 * sd(LxI))
# 0.1868302

#95% CI for LyI
mean(LyI)
# 0.08624951
mean(LyI) - (1.96 * sd(LyI))
# -0.05693295
mean(LyI) + (1.96 * sd(LyI))
# 0.229432

#95% CI for OxI
mean(OxI)
# 0.1419263
mean(OxI) - (1.96 * sd(OxI))
# 0.01872792
mean(OxI) + (1.96 * sd(OxI))
# 0.2651247

#95% CI for OyI
mean(OyI)
# 0.1004432
mean(OyI) - (1.96 * sd(OyI))
# -0.02852504
mean(OyI) + (1.96 * sd(OyI))
# 0.2294115

#95% CI for IxO
mean(IxO)
# -0.0447177
mean(IxO) - (1.96 * sd(IxO))
# -0.1829805
mean(IxO) + (1.96 * sd(IxO))
# 0.09354508

#95% CI for IyO
mean(IyO)
# 0.01626032
mean(IyO) - (1.96 * sd(IyO))
# -0.1290477
mean(IyO) + (1.96 * sd(IyO))
# 0.1615684

#95% CI for DxO
mean(DxO)
# 0.03317775
mean(DxO) - (1.96 * sd(DxO))
# -0.08780523
mean(DxO) + (1.96 * sd(DxO))
# 0.1541607

#95% CI for DyO
mean(DyO)
# 0.03630816
mean(DyO) - (1.96 * sd(DyO))
# -0.09237264
mean(DyO) + (1.96 * sd(DyO))
# 0.164989


#########################################  PLOT DATA  #########################################


#To be able to plot the bootstrap data as boxplots I create a new data frame with the data

#First I make a vector with the populations
population <- c(rep("aLHmX_Inn", 10000), rep("bLHmY_Inn", 10000), rep("cOddX_Inn", 10000), rep("dOddY_Inn", 10000),
                rep("eInnX_Odd", 10000), rep("fInnY_Odd", 10000), rep("gDahX_Odd", 10000), rep("hDahY_Odd", 10000))

#Then I collcet all the bootstrap data in a new vector
deltafitness <- c(LxI, LyI, OxI, OyI, IxO, IyO, DxO, DyO)

#Then it's all collceted in a new data frame
DFg25 <- data.frame(population, deltafitness)

#And write it into a new file
write.csv(EvolDF, file = "DeltaNovelPopMaleFitnessAssayG25.csv")


#Read in csv file with data
DNPFAg25.data<- read.table(file = "DeltaNovelPopMaleFitnessAssayG25.csv", h = T, sep = ",")

#MEAN
meanDNPFAg25 <- tapply(DNPFAg25.data$deltafitness, DNPFAg25.data$population, mean)
#SD
sdDNPFAg25 <- tapply(DNPFAg25.data$deltafitness, DNPFAg25.data$population, sd)


#Plot
par(mar = c(6, 5, 2, 2))
#Plot errorbars
xDNPFAg25 <- c(0.5,1,1.5,2, 3,3.5,4,4.5)
errbar(xDNPFAg25, meanDNPFAg25, meanDNPFAg25 + (1.96 * sdDNPFAg25), meanDNPFAg25 - (1.96 * sdDNPFAg25), 
       xlim = c(0.3, 4.7), xlab = "", xaxt = "n", ylim = c(-0.2, 0.3), ylab = expression(Delta~"Fitness"), 
       cex.axis = 1.2, cex.lab = 1.5, las = 1,  pch = c(17, 18), cex = c(3, 3.5), lwd = 3)
#AXIS
axis(1, at = c(0.5,1,1.5,2, 3,3.5,4,4.5), cex.axis = 1.2,
     labels = c(expression("L"["X"]), expression("L"["Y"]), expression("O"["X"]), expression("O"["Y"]),
                expression("I"["X"]), expression("I"["Y"]), expression("D"["X"]), expression("D"["Y"])))
#Add line at 0
abline(h = 0, lty = 2, lwd = 2)
#And add text below
mtext(expression(italic("Innisfail")), side = 1, line = 3, at = 1.25, cex = 1.5)
mtext(expression(italic("Odder")), side = 1, line = 3, at = 3.75, cex = 1.5)
#Now add arrows to show significance
points(1.5, 0.3, pch = "*", bg = "black", cex = 1.5)


