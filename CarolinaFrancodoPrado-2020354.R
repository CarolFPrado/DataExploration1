#install package
install.packages(c("tidyverse","dummies","caTools"))
install.packages("factoextra")
install.packages("tidyr")
install.packages("fastDummies")

#Load libraries
library(ggplot2)
library(robustbase)
library(gridExtra)
library(tidyverse)
library(factoextra)
library(fastDummies)



#Create subset "crimes"
crimes=  read.csv(file = "crimes_against_women_2001-2014.csv", stringsAsFactors =
                    FALSE)
head(crimes)


###Attributing other names to the columns
#Categorical Nominal data
statein <- crimes$STATE.UT
district <- crimes$DISTRICT
#Discrete data
years <- as.integer(crimes$Year)
rape <- crimes$Rape
kidna <- crimes$Kidnapping.and.Abduction
dowry <- crimes$Dowry.Deaths
assault <- crimes$Assault.on.women.with.intent.to.outrage.her.modesty
insult <- crimes$Insult.to.modesty.of.Women
cruelty <- crimes$Cruelty.by.Husband.or.his.Relatives
importationg <- crimes$Importation.of.Girls

###Data plot
#Reports of crimes in each STATE per YEAR 
crimes%>%
  ggplot()+
  geom_bar(mapping = aes(x= years, fill = STATE.UT), color = "black")+
  labs(title = "Stacked Bar Chart of Crimes per States by Year", 
       x = "Years", y = "State") +
  coord_flip()
#Reports of RAPES per YEAR
plotr<-ggplot(data=crimes, aes(x=years, y=rape)) +
  geom_bar(stat="identity")
plotr
#Reports of KIDNAPPING per YEAR
plotk<-ggplot(data=crimes, aes(x=years, y=kidna)) +
  geom_bar(stat="identity")
plotk
#Reports of DOWRY per YEAR
plotD<-ggplot(data=crimes, aes(x=years, y=dowry)) +
  geom_bar(stat="identity")
plotD
#Reports of ASSAULT per YEAR
plotA<-ggplot(data=crimes, aes(x=years, y=assault)) +
  geom_bar(stat="identity")
plotA
#Reports of INSULT per YEAR
plotI<-ggplot(data=crimes, aes(x=years, y=insult)) +
  geom_bar(stat="identity")
plotI
#Reports of CRUELTY per YEAR
plotC<-ggplot(data=crimes, aes(x=years, y=cruelty)) +
  geom_bar(stat="identity")
plotC
#Reports of IMPORTATING OF GIRLS per YEAR
plotIM<-ggplot(data=crimes, aes(x=years, y=importationg)) +
  geom_bar(stat="identity")
plotIM
###Identify outliers
#Rapes
plot(rape, years,
     xlim = c(0,800),
     ylim = c(2000,2014),
     xlab = "Rapes",
     ylab = "Years",
     main = "Scatterplot of rapes by year",
     type = "p",
     pch = 5,
     col = "blue")
#Kidnapping
plot(kidna, years,
     xlim = c(0,800),
     ylim = c(2000,2014),
     xlab = "Kidnapping",
     ylab = "Years",
     main = "Scatterplot of kidnapping by year",
     type = "p",
     pch = 5,
     col = "blue")
#Dowry
plot(dowry, years,
     xlim = c(0,300),
     ylim = c(2000,2014),
     xlab = "Dowry",
     ylab = "Years",
     main = "Scatterplot of dowry by year",
     type = "p",
     pch = 5,
     col = "blue")
#Assault
plot(assault, years,
     xlim = c(0,1500),
     ylim = c(2000,2014),
     xlab = "Assault",
     ylab = "Years",
     main = "Scatterplot of assault by year",
     type = "p",
     pch = 5,
     col = "blue")
#Insult
plot(insult, years,
     xlim = c(0,2000),
     ylim = c(2000,2014),
     xlab = "Insult",
     ylab = "Years",
     main = "Scatterplot of insult by year",
     type = "p",
     pch = 5,
     col = "blue")
#Cruelty
plot(cruelty, years,
     xlim = c(0,4000),
     ylim = c(2000,2014),
     xlab = "Cruelty",
     ylab = "Years",
     main = "Scatterplot of cruelty by year",
     type = "p",
     pch = 5,
     col = "blue")
#Importation of Girls
plot(importationg, years,
     xlim = c(0,100),
     ylim = c(2000,2014),
     xlab = "Importation of Girls",
     ylab = "Years",
     main = "Scatterplot of importation of girls by year",
     type = "p",
     pch = 5,
     col = "blue")
################### FIND MEAN
#Follow below the mean for each numerical variable from the dataset
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

### importation of girls
result.meanig <- mean(importationg)
print(result.meanig)

################## FIND MEDIAN
#In the following lines will be presented the median from all the
#numerical variables from the dataset

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

### importation of girls
result.medianig <- median(importationg)
print(result.medianig)

#Follow below the MIN for each numerical variable from the dataset

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

### importation of girls
result.minig <- min(importationg)
print(result.minig)


#######FIN MAX
#Follow below the MAX for each numerical variable from the dataset

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

### importation of girls
result.maxig <- max(importationg)
print(result.maxig)


#######Standard Deviation
#Follow below the Standard Deviation for each numerical variable from the dataset

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

### importation of girls
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

# Original Data
p1 <- ggplot(crimes, aes(x = rape)) +
  geom_histogram(binwidth = 0.2, fill = "blue", color = "black") +
  labs(title = "Histogram of rape")
p2 <- ggplot(crimes_minmax, aes(x = rape)) +
  geom_histogram(binwidth = 0.2, fill = "green", color = "black") +
  labs(title = "Histogram of rape (Min-Max Scaled)")
p3 <- ggplot(crimes_standardized, aes(x = rape)) +
  geom_histogram(binwidth = 0.2, fill = "red", color = "black") +
  labs(title = "Histogram of Sepal rape (Standardized)")
p4 <- ggplot(crimes_robust, aes(x = rape)) +
  geom_histogram(binwidth = 0.2, fill = "purple", color = "black") +
  labs(title = "Histogram of rape (Robust Scaled)")
grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)

############################VISUALIZATION -- CRIMES THROUGH THE YEARS --

##Below will be shown the visualization of each crime by year
#which will be plotted a graph showing the improvement or reduction
#of each crime . The code below was based on the suggestion for a problem 
#solving in "Stackoverflow". [1]


#######Covariation
#Rapes and years
ggplot(data = crimes)+
  
  geom_point( mapping = aes(x  = years, y = rape))


# Kidnapping and years
ggplot(data = crimes)+
  
  geom_point( mapping = aes(x  = years, y = kidna))

# Dowry and years
ggplot(data = crimes)+
  
  geom_point( mapping = aes(x  = years, y = dowry))

# Insult and years
ggplot(data = crimes)+
  
  geom_point( mapping = aes(x  = years, y = insult))

# Assault and years
ggplot(data = crimes)+
  
  geom_point( mapping = aes(x  = years, y = assault))

# Importation of girls
ggplot(data = crimes)+
  
  geom_point( mapping = aes(x  = years, y = importationg))



####### Dummy coding
#Get summary statistics of Dataset
summary(crimes)

#Select specific columns in the data and get the summary of the selected columns.
select(crimes,STATE.UT,DISTRICT)

summary(select(crimes,STATE.UT,DISTRICT))

#Get all the values and their related counts for categorical features.
table(select(crimes, STATE.UT,DISTRICT))

# Make Dummy Variables in R
crimes <- dummy_cols(crimes,select_columns = c('STATE.UT','DISTRICT'))

head(crimes)

####### PCA
# The following steps used to present the PCA was based on the "Statistical tools
#for high-throughput data analysis"[2]

#extract data that will be used 
crimes.active <- crimes[2:20, 5:10]
head(crimes.active)

#Compute PCA
result.pca <- prcomp(crimes.active, scale = TRUE)

#Visualize eigenvalues (scree plot)
fviz_eig(result.pca)

#Graph of variables
fviz_pca_var(result.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

### Access to the PCA results
# Eigenvalues
eig.val <- get_eigenvalue(result.pca)
eig.val

#Results for Variables
result.var <- get_pca_var(result.pca)
result.var$coord          # Coordinates
result.var$contrib        # Contributions to the PCs
result.var$cos2           # Quality of representation 

# Results for district/year
result.ind <- get_pca_ind(result.pca)
result.ind$coord          # Coordinates
result.ind$contrib        # Contributions to the PCs
result.ind$cos2           # Quality of representation 


###### REFERENCES

#[1]https://stackoverflow.com/questions/39439806/how-to-plot-an-histogram-x-axis-year-on-ggplot
#[2]http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/
