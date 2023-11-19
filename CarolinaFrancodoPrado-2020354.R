#Load libraries
library(ggplot2)
library(robustbase)
library(gridExtra)

#Create subset "crimes"
crimes=  read.csv(file = "crimes_against_women_2001-2014.csv", stringsAsFactors =
                    FALSE)
head(crimes)
#Attributing other names to the columns

years <- crimes$Year
rape <- crimes$Rape
kidna <- crimes$Kidnapping.and.Abduction
dowry <- crimes$Dowry.Deaths
assault <- crimes$Assault.on.women.with.intent.to.outrage.her.modesty
insult <- crimes$Insult.to.modesty.of.Women
cruelty <- crimes$Cruelty.by.Husband.or.his.Relatives
importationg <- crimes$Importation.of.Girls

################### FIND MEAN

### years
result.meany <- mean(years)
print(result.meany)

### rape
result.meanrp <- mean(rape)
print(result.meanrp)

### kidnap
result.meankp <- mean(kidna)
print(result.meankp)

### dowry
result.meando <- mean(dowry)
print(result.meando)

### assault
result.meanas <- mean(assault)
print(result.meanas)

### insult
result.meanis <- mean(insult)
print(result.meanis)

### cruelty
result.meancr <- mean(cruelty)
print(result.meancr)

### importationg
result.meanig <- mean(importationg)
print(result.meanig)

################## FIND MEDIAN

### years
result.mediany <- median(years)
print(result.mediany)

### rape
result.medianrp <- median(rape)
print(result.medianrp)

### kidnap
result.mediankp <- median(kidna)
print(result.mediankp)

### dowry
result.mediando <- median(dowry)
print(result.mediando)

### assault
result.medianas <- median(assault)
print(result.medianas)

### insult
result.medianis <- median(insult)
print(result.medianis)

### cruelty
result.mediancr <- median(cruelty)
print(result.mediancr)

### importationg
result.medianig <- median(importationg)
print(result.medianig)

#####FIND MIN
### years
result.miny <- min(years)
print(result.miny)

### rape
result.minrp <- min(rape)
print(result.minrp)

### kidnap
result.minkp <- min(kidna)
print(result.minkp)

### dowry
result.mindo <- min(dowry)
print(result.mindo)

### assault
result.minas <- min(assault)
print(result.minas)

### insult
result.minis <- min(insult)
print(result.minis)

### cruelty
result.mincr <- min(cruelty)
print(result.mincr)

### importationg
result.minig <- min(importationg)
print(result.minig)

#######FIN MAX
### years
result.maxy <- max(years)
print(result.maxy)

### rape
result.maxrp <- max(rape)
print(result.maxrp)

### kidnap
result.maxkp <- max(kidna)
print(result.maxkp)

### dowry
result.maxdo <- max(dowry)
print(result.maxdo)

### assault
result.maxas <- max(assault)
print(result.maxas)

### insult
result.maxis <- max(insult)
print(result.maxis)

### cruelty
result.maxcr <- max(cruelty)
print(result.maxcr)

### importationg
result.maxig <- max(importationg)
print(result.maxig)

#######Standard Deviation
### years
result.sdy <- sd(years)
print(result.sdy)

### rape
result.sdrp <- sd(rape)
print(result.sdrp)

### kidnap
result.sdkp <- sd(kidna)
print(result.sdkp)

### dowry
result.sddo <- sd(dowry)
print(result.sddo)

### assault
result.sdas <- sd(assault)
print(result.sdas)

### insult
result.sdis <- sd(insult)
print(result.sdis)

### cruelty
result.sdcr <- sd(cruelty)
print(result.sdcr)

### importationg
result.sdig <- sd(importationg)
print(result.sdig)

####### Min-Max Scaling
normalizeMinMax <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
crimes_minmax <- crimes
crimes_minmax[,5:10] <- apply(crimes[,5:10], 2, normalizeMinMax)

head(crimes_minmax)

###### Z-Score Standardization
normalizeStandardized <- function(x) {
  return ((x - mean(x)) / sd(x))
}

crimes_standardized <- crimes
crimes_standardized[, 5:10] <- apply(crimes[, 5:10], 2, normalizeStandardized)

head(crimes_standardized)

####### Robust Scaling (Robust Standardization)
crimes_robust <- crimes
crimes_robust[, 5:10] <- scale(crimes[, 5:10], center = TRUE, scale = TRUE)

head(crimes_robust)

####Dummy coding
# The dataset used for the current analyses does not have a categorical variable
#that would justify using a Dummy coding, so for the present study was analysed the last 
#column "Importation of Girls" using the Dummy code if the crime happened or not.

chappen <- crimes$Importation.of.Girls!=0
NOThappen <- crimes$Importation.of.Girls==0
  
 impgh <-ifelse(chappen, 1,0)
 impgNh <-ifelse(NOThappen, 1,0)

crimeig_reg <- data.frame(impgh = impgh,
                          impgNh = impgNh)
crimeig_reg