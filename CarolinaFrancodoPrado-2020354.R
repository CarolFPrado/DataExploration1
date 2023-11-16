#Load libraries
library(ggplot2)
library(robustbase)
library(gridExtra)

#Create subset "crimes"
crimes= read.table("crimes_against_women_2001_2014.csv", header=TRUE)

#Attributing other names to the columns

years <- crimes_against_women_2001_2014$Year
rape <- crimes_against_women_2001_2014$Rape
kidna <- crimes_against_women_2001_2014$`Kidnapping and Abduction`
dowry <- crimes_against_women_2001_2014$`Dowry Deaths`
assault <- crimes_against_women_2001_2014$`Assault on women with intent to outrage her modesty`
insult <- crimes_against_women_2001_2014$`Insult to modesty of Women`
cruelty <- crimes_against_women_2001_2014$`Cruelty by Husband or his Relatives`
importationg <- crimes_against_women_2001_2014$`Importation of Girls`

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
####