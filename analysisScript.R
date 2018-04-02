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

# We need to create two mercury deposition variables: a mean value for Dec previous year - May current year
# This is just a way to create a date that can be used to calculate means for 6 months + 1 year prior:
thrush.df$mercuryDate <- paste("5","31",year(thrush.df$date), sep = "-")
thrush.df$mercuryDate <- as.Date(thrush.df$mercuryDate, format = "%m-%e-%Y")
thrush.df$year <- year(thrush.df$date)
thrush.df$jdate <- yday(thrush.df$date) # Creating a "day of the year" variable

for(i in 1:nrow(thrush.df)){
  thrush.df$hgDep6MO[i] <- mean(mercury.df$hgDepositionUGM2[which(mercury.df$date >= thrush.df$mercuryDate[i]-180 & mercury.df$date <= thrush.df$mercuryDate[i])], na.rm=T)
}

# and for the Jun 1 previous year - May 31 current year :
for(i in 1:nrow(thrush.df)){
  thrush.df$hgDep1Yr[i] <- mean(mercury.df$hgDepositionUGM2[which(mercury.df$date >= thrush.df$mercuryDate[i]-365 & mercury.df$date <= thrush.df$mercuryDate[i])], na.rm=T)
}

# In looking at the data, we have missing values for blood mercury:
summary(thrush.df)
# get rid of those:
thrush.df <- thrush.df[!is.na(thrush.df$hgLevel),]

# Exploratory analysis
## Looking at the mercury date, looks to have been a modest gain over time in deposition.
ggplot(mercury.df, aes(x = date, y = hgDepositionUGM2)) + geom_smooth() + geom_point(alpha = 0.1) + 
  coord_trans(y = "sqrt") + ylab("Mercury deposition (micrograms per square meter") + xlab("Date")

## But how much of this is due to different data sets? No obvious changes in the variability of estimates.
mercury.df %>%
  group_by(filename) %>%
  ggplot(., aes(x = filename, y = hgDepositionUGM2)) + geom_boxplot() + coord_trans(y = "sqrt") + 
  xlab(NULL) + ylab("Mercury deposition\n(micrograms per square meter)") + theme(axis.text.x = element_text(angle = 340,
                                                                                                           vjust = 0.5))

## Sample sizes over time:
thrush.df %>%
  group_by(year, species)%>%
  summarize(count = n_distinct(bandNum)) %>%
  ggplot(., aes(x = year, y = count)) + geom_col() + facet_wrap(~species) + 
  xlab("Year") + ylab("Number of unqiue individuals sampled") + geom_text(aes(label = count, vjust = -0.5), size = 3)

## Further break this down by sex:
thrush.df %>%
  group_by(year, species, sex)%>%
  summarize(count = n_distinct(bandNum)) %>%
  ggplot(., aes(x = year, y = count,fill = sex)) + geom_col() + facet_wrap(~species) + 
  xlab("Year") + ylab("Number of unqiue individuals sampled") + geom_text(aes(label = count),
                                                                          position = position_stack(0.9),
                                                                          size = 3)

## Overall,  by sex. For Swainson's Thrush, separating by sex isn't possible.
thrush.df %>%
  group_by(species, sex)%>%
  summarize(count = n_distinct(bandNum)) %>%
  ggplot(., aes(x = sex, y = count)) + geom_col() + facet_wrap(~species) + 
  xlab("Sex") + ylab("Number of unqiue individuals sampled") + geom_text(aes(label = count, vjust = -0.5),
                                                                          size = 3)

## Compare roughly among sexes.
## Male BITH (sex = 4) tend to have higher mercury levels and to exhibit greater variability in 
## mercury levels than females. 
ggplot(thrush.df, aes(x = sex, y = hgLevel)) + geom_boxplot() + facet_wrap(~species)

## Does body mass affect Hg?
thrush.df %>%
  group_by(species,bandNum) %>%
  summarize(meanHg = mean(hgLevel), meanMass = mean(wt)) %>%
  ggplot(., aes(x = meanMass, y = meanHg)) + geom_smooth() + geom_point() + facet_wrap(~species)

## A SWTH with wt = 68 g is obiously a mistake. Filter that to show that mass and mercury aren't associated:
thrush.df %>%
  filter(wt < 68) %>%
  group_by(species,bandNum) %>%
  summarize(meanHg = mean(hgLevel), meanMass = mean(wt)) %>%
  ggplot(., aes(x = meanMass, y = meanHg)) + geom_smooth() + geom_point(alpha = 0.25) + facet_wrap(~species) + 
  xlab("Mass (g)") + ylab("Mercury level")

## Change over day-of-the year shows a general decline in mercury over the course of the year:
## For BITH
thrush.df %>%
  filter(species == "BITH") %>%
  ggplot(., aes(x = yday(date), y = hgLevel)) + geom_smooth() + geom_point(alpha = 0.25) + facet_wrap(~year) + 
  xlab("Day of the year") + ylab("Mercury level") + ggtitle("Bicknell's Thrush mercury levels decrease\nover the year")

## And SWTH:
thrush.df %>%
  filter(species == "SWTH") %>%
  ggplot(., aes(x = yday(date), y = hgLevel)) + geom_smooth() + geom_point(alpha = 0.25) + facet_wrap(~year) + 
  xlab("Day of the year") + ylab("Mercury level") + ggtitle("Swainson's Thrush mercury levels\ndecrease over the year")

# Models.
## response variable = hgLevels.
## fixed effects: species, year, day-of-year, sex, hgDep1Yr, hgDep6mo
## random effects: individual (bandNum)

### Using the Lasso approach seems favored when considering variable selection.
#### 
#library(glmmLasso)
# scale and center all metric variables so that the starting values with glmmPQL are correct
thrush.df$yearSc <- scale(thrush.df$year, center = T, scale = T)
thrush.df$jdateSc <- scale(thrush.df$jdate, center = T, scale = T)
thrush.df$hgDep1YrSc <- scale(thrush.df$hgDep1Yr, center = T, scale = T)
thrush.df$hgDep6MOSc <- scale(thrush.df$hgDep6MO, center = T, scale = T)

lambda <- seq(500,0,by=-5)
library(MASS)
library(nlme)
PQL <- glmmPQL(hgLevel~1, random = ~1|bandNum, family = gaussian, data = thrush.df)
Delta.start <- c(as.numeric(PQL$coef$fixed, rep(0,6), as.numeric(t(PQL$coef$random$bandNum))))
Q.start <- as.numeric(VarCorr(PQL)[1,1])

for(j in 1:length(lambda)) {
  print(paste("Iteration ", j, sep = ""))
  glm1 <- try(glmmLasso(hgLevel ~ yearSc + jdateSc + hgDep1YrSc + hgDep6MOSc + as.factor(age) + 
                          as.factor(sex) + as.factor(species), rnd = list(bandNum=~1),
                        data = thrush.df, lambda = lambda[j], switch.NR = T,
                        final.re = TRUE, control = list(start = Delta.start, q_start = Q.start)), silent = FALSE)
  if(class(glm1) != "try-error")
  {
    BIC_vec[j] <- glm1$bic
  }
}

opt<-which.min(BIC_vec)

glm1_final <- glmmLasso(hgLevel ~ yearSc + jdateSc + hgDep1YrSc + hgDep6MOSc + as.factor(age) + 
                          as.factor(sex) + as.factor(species), rnd = list(bandNum=~1),
                        family = gaussian, data = thrush.df, lambda = lambda[opt], switch.NR = F, 
                        final.re = TRUE, control = list(start = Delta.start, q_start = Q.start))

#lambda 100: bic = 132.9129
#lambda 50: bic = 132.9129
#lambda 40: bic = 132.9129
#lambda 20: bic = 132.9129
#lambda 10: bic = 132.9129
#lambda 5: bic = 97.50503
#lambda 4: bic = 174.3837
#lambda 3: bic = 195.695
#lambda 2: bic = 137.1042
#lambda 1.5: bic = 121.9519
#lambda 1.1: bic = 120.1637
#lambda 1: bic = 57.78632
#lambda 0.9: bic = 63.66986

glm1_final <- glmmLasso(hgLevel ~ jdate + year, rnd = list(bandNum = ~1),
                        data = thrush.df, lambda = 100)
summary(glm1_final)
glm1_final$bic
