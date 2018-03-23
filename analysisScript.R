library(lubridate)
library(tidyr)
library(dplyr)
if (Sys.info()['sysname'] == 'Darwin') {
  libjvm <- paste0(system2('/usr/libexec/java_home',stdout = TRUE)[1],'/jre/lib/server/libjvm.dylib')
  message (paste0('Load libjvm.dylib from: ',libjvm))
  dyn.load(libjvm)
}
library(rJava)
library(xlsx)

# Read in first mercury deposition file:
mercury9303 <- read.csv("PMRC Wet Deposition Hg 1999-2003.csv", header = T)

# Add the filename in for reference:
mercury9303$filename <- basename("PMRC Wet Deposition Hg 1999-2003")

# Convert "Date" to date variable
mercury9303$Date <- as.Date(mercury9303$Date, format = "%m/%e/%y")

# Rename columns
colnames(mercury9303) <- c("date","siteID","sampleNumber","precipSampVolML","precipAmountCM",
                          "hgConcentrationNGL","hgDepositionUGM2","filename")
# Write this file for storage:
write.csv(mercury9303, file = "mercury9303.csv", row.names = F)

# Read in second mercury deposition file:
mercury0416 <- read.csv("PMRC Wet Deposition Hg MDN 2004-16.csv", header = T, na.strings = "-9")

# Convert "Date" to date variable
mercury0416$DATEON <- as.Date(mercury0416$DATEON, format = "%m/%e/%y")
mercury0416$DATEOFF <- as.Date(mercury0416$DATEOFF, format = "%m/%e/%y")

# Add the filename in for reference:
mercury0416$filename <- basename("PMRC Wet Deposition MDN Hg 2004-2016")

# Rename columns. Note that "HgDep.ug.g" is apparently mislabeled, according to both the metadata
# and the data maintainers at FEMC. This value is apparently ug/m2, which matches the units in the 
# mercury9303 file.
colnames(mercury0416) <- c("siteID", "dateOn", "dateOff","rgpptMM","sVolML","subPptMM","hgConcentrationNGL",
                           "hgDepositionNGM2","hgDepositionUGM2","sampleType","qr","notes",
                           "yearMonth","dateMod","filename")

# This data frame has two date values, which I assume indicate the beginning and ending
# date of the sample period (note that the metadata column names don't match, so it is hard to 
# know for sure). To get a single date value comparable to the other data file, create a new variable
# that is the midpoint of the dateOn and dateOff values:
mercury0416$date <- mercury0416$dateOn + floor((mercury0416$dateOff-mercury0416$dateOn)/2) 

# Get rid of the samples with missing values:
mercury0416 <- mercury0416[complete.cases(mercury0416), TRUE]

# Write the second mercury file for storage:
write.csv(mercury0416, file = "mercury0416.csv", row.names = F)

# Join the key columns (date, mercury deposition) from the two datasets into a single data frame:
mercury <- rbind((select(mercury9303, siteID, date, hgDepositionUGM2, filename)),
                  select(mercury0416, siteID, date, hgDepositionUGM2, filename))

write.csv(mercury, file = "mercury.csv", row.names = F)

# Read in the thrush mercury data, leaving out the extraneous comment columns at the end:
thrush <- read.xlsx(file = "Hg blood Thrush Mansfield all.xlsx", colIndex = 1:18,sheetIndex = 1)

# Give the columns sensible names
colnames(thrush) <- c("bandNum","date","status","age","sex","time","loc","cp","bp","wg","wt",
                      "hgBloodID","species","hgID","tissueType","hgLevel","mehgLevel","hgLab")

# Error in "tissueType": sometimes "blood" is typed "Blood"
thrush$tissueType[thrush$tissueType == "Blood"] <- "blood"
thrush$tissueType <- as.factor(thrush$tissueType)
thrush <- droplevels(thrush)

# Correct the type of variable:
thrush$bandNum <- as.factor(thrush$bandNum)
thrush$status <- as.factor(thrush$status)
thrush$age <- as.factor(thrush$age)
thrush$sex <- as.factor(thrush$sex)
thrush$cp <- as.factor(thrush$cp)
thrush$bp <- as.factor(thrush$bp)
levels(thrush$bp) # there is a factor level "9"; not sure what this means

# Save the edited file.
write.csv(thrush, file = "thrush.df.csv", row.names = F)

# Import the saved relevant .csv files:
## Mercury
colClassesMercury <- as.vector(c("factor", "Date","numeric","factor"))
mercury.df <- read.csv("mercury.csv", colClasses = colClassesMercury)
colClassesThrush <- as.vector(c("factor","Date","factor","factor","factor","numeric","factor",
                                "factor","factor","numeric","numeric","factor","factor","factor",
                                "factor","numeric","numeric","factor"))
thrush.df <- read.csv("thrush.df.csv", colClasses = colClassesThrush)

# Clean up the workspace
rm(mercury, mercury0416, mercury9303, thrush)

# We need to create two mercury deposition variables: a mean value for the 
# past 6 months
for(i in 1:nrow(thrush.df)){
  thrush.df$hgDep6MO[i] <- mean(mercury.df$hgDepositionUGM2[which(mercury.df$date >= thrush.df$date[i]-180 & mercury.df$date <= thrush.df$date[i])], na.rm=T)
}

# and for the past 2 months:
for(i in 1:nrow(thrush.df)){
  thrush.df$hgDep2MO[i] <- mean(mercury.df$hgDepositionUGM2[which(mercury.df$date >= thrush.df$date[i]-60 & mercury.df$date <= thrush.df$date[i])], na.rm=T)
}

