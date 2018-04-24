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

# and for a 2-year lag :
for(i in 1:nrow(thrush.df)){
  thrush.df$hgDep2Yr[i] <- mean(mercury.df$hgDepositionUGM2[which(mercury.df$date >= thrush.df$mercuryDate[i]-(365*2) & mercury.df$date <= thrush.df$mercuryDate[i])], na.rm=T)
}

# and for a 3-year lag :
for(i in 1:nrow(thrush.df)){
  thrush.df$hgDep3Yr[i] <- mean(mercury.df$hgDepositionUGM2[which(mercury.df$date >= thrush.df$mercuryDate[i]-(365*3) & mercury.df$date <= thrush.df$mercuryDate[i])], na.rm=T)
}

# Create a variable that lumps similar ages:
thrush.df$ageCat <- as.factor(ifelse(thrush.df$age == 2, "HY", ifelse(thrush.df$age == 4, "HY", ifelse(thrush.df$age == 5, "SY","ASY"))))

# Create a sensible sex value:
thrush.df <- 
  thrush.df %>%
  mutate(sexCat = ifelse(sex == 4,"Male",ifelse(sex == 5, "Female", "Unknown")))
# In looking at the data, we have missing values for blood mercury:
summary(thrush.df)
# get rid of those:
thrush.df <- thrush.df[!is.na(thrush.df$hgLevel),]


# Save the edited file.
write.csv(thrush.df, file = "thrush.df.csv", row.names = F)

# Re-import as a data frame for analysis:
colClassesThrush <- as.vector(c("factor","Date","factor","factor","factor","numeric","factor",
                                "factor","factor","numeric","numeric","factor","factor","factor",
                                "factor","numeric","numeric","factor", "numeric","numeric","Date",
                                "numeric","numeric","numeric","numeric","factor","factor"))
thrush.df <- read.csv("thrush.df.csv", colClasses = colClassesThrush)

# Exploratory analysis
## Looking at the mercury date, looks to have been a modest gain over time in deposition.
mercury.df %>%
filter(date > "1999-12-31") %>%
ggplot(., aes(x = date, y = hgDepositionUGM2)) + geom_smooth() + geom_point(alpha = 0.1) + 
  coord_trans(y = "sqrt") + ylab("Mercury deposition (micrograms per square meter") + xlab("Date")

mercury.df %>%
  filter(date > "1999-12-31") %>%
  ggplot(., aes(x = as.factor(month(date)), y = hgDepositionUGM2)) + geom_boxplot()

## But how much of this is due to different data sets? No obvious changes in the variability of estimates.
mercury.df %>%
  group_by(filename) %>%
  ggplot(., aes(x = filename, y = hgDepositionUGM2)) + geom_boxplot() + coord_trans(y = "sqrt") + 
  xlab(NULL) + ylab("Mercury deposition\n(micrograms per square meter)") + theme(axis.text.x = element_text(angle = 340,
                                                                                                           vjust = 0.5))
## Analyze the temporal variation in mercury:
library(mgcv)
library(viridis)
mercury.df$doy <- yday(mercury.df$date)
mercury.df$year <- year(mercury.df$date)
colnames(mercury.df)[6] <- "YEAR"
write.csv(mercury.df, "mercury.df.csv", row.names = F)
colClassesMercury <- as.vector(c("factor","Date","numeric","factor",
                                 "numeric","numeric","numeric"))
tmdf <- read.csv("mercury.df.csv", colClasses = colClassesMercury)

knots <- list(doy = c(0.5,366.5)) # set the endpoints of the annual cycle (with some leap years)

gam.m1 <- bam(hgDepositionUGM2 ~ s(doy, k = 30, bs = "cc") + s(YEAR, k = 24) + 
                ti(doy, YEAR, bs = c("cc","tp"), k = c(15,15)), data = mercury.df, 
              method = "fREML", knots = knots, discrete = TRUE)
summary(gam.m1)

gam.m2 <- bam(hgDepositionUGM2 ~ s(doy, k = 30, bs = "cc") + s(YEAR, k = 24),
              data = mercury.df, 
              method = "fREML", knots = knots, discrete = TRUE)
summary(gam.m2)
AIC(gam.m1,gam.m2) # delta AIC for the simpler model is ~6, so prefer the more complex model that 
# allows for an interaction between year*day-of-year (i.e., seasonal patterns of deposition vary over time)
gam.check(gam.m1)
plot(gam.m1, pages = 1, scheme = 2, shade = TRUE)

library(itsadug)
library(xtable)
gamtabs(gam.m1, caption = "Summary of generalized additive model", type = "html")

mercury.preds <- data.frame(predicted = gam.m1$fitted.values,
                            observed = gam.m1$model$hgDepositionUGM2,
                            year = gam.m1$model$YEAR,
                            date = gam.m1$model$doy)

mercury.preds <- gather(data = mercury.preds,key = group, value = measurement,1:2)
ggplot(data = mercury.preds, aes(x = date, y = measurement, group = group, color = group)) + 
  geom_point()

mercury.df$predicted <- gam.m1$fitted.values
ggplot(mercury.df, aes(x = doy, y = predicted)) + geom_line() + 
  geom_line(aes(x = doy, y = hgDepositionUGM2, color = "red", alpha = 0.2)) + coord_trans(y = "sqrt") + 
  facet_wrap(~YEAR)

#GAM plots, with values shifted by the value of the intercept to make
#the y-axis more interpretable.
par(mfrow = c(1,2), mgp = c(1.5,0.5,0))
plot(gam.m1, select = 2, shade = T, shift = 0.100684, xlab = "Year", 
     ylab = expression("Expected mercury concentration" ~ (mu ~ grams ~ m^{-2})))
text(2015,0.18,"A")
plot(gam.m1, rug = F, select = 1, shade = T, shift = 0.100684, xlab = "Day of the year", 
     ylab = expression("Expected mercury concentration" ~ (mu ~ grams ~ m^{-2})))
text(358,0.18,"B")

vis.gam(gam.m1,plot.type="contour",color="terrain", main="", xlab = "Day of the year",
        ylab = "Year")

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

## Compare among ages:
ggplot(thrush.df, aes(x = age, y = hgLevel)) + geom_boxplot() + facet_wrap(~species) 

## Estimate means by age and sex:
thrush.df %>%
  group_by(species, ageCat, sexCat) %>%
  summarize(meanHg = mean(hgLevel, na.rm = TRUE),
            sd = sd(hgLevel, na.rm =TRUE),
            n = n()) %>%
  mutate(se = sd/sqrt(n),
         lower.ci  = meanHg - qt(1-(0.05/2), n-1)*se,
         upper.ci = meanHg + qt(1-(0.05/2), n-1)*se)

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
  ggplot(., aes(x = yday(date), y = hgLevel)) + geom_smooth() + geom_point(alpha = 0.25) + 
  xlab("Day of the year") + ylab("Mercury level") + ggtitle("Bicknell's Thrush mercury levels decrease\nover the year")

thrush.df %>%
  group_by(bandNum) %>%
  filter(n()>1) %>%
  ggplot(., aes(x = yday(date), y = hgLevel)) + geom_line() + geom_point() + 
  facet_wrap(~bandNum) + theme(legend.position="none")

thrush.df %>%
  group_by(bandNum) %>%
  filter(n()>1) %>%
  ggplot(., aes(x = yday(date), y = hgLevel, color = bandNum)) + geom_smooth(method = "lm", se = FALSE) + 
  theme(legend.position="none")

## And SWTH:
thrush.df %>%
  filter(species == "SWTH") %>%
  ggplot(., aes(x = yday(date), y = hgLevel)) + geom_smooth() + geom_point(alpha = 0.25) + facet_wrap(~year) + 
  xlab("Day of the year") + ylab("Mercury level") + ggtitle("Swainson's Thrush mercury levels\ndecrease over the year")

# Correlations with atmospheric mercury:
thrush.df %>%
  filter(species == "BITH") %>%
  ggplot(., aes(x = hgDep6MO, y = hgLevel)) + geom_smooth() + geom_point(alpha = 0.25) + 
  xlab("Mercury deposition") + ylab("Mercury level") + 
  ggtitle("Correlation between Bicknell's Thrush mercury levels\nand mercury deposition during the past 6 months")
cor(thrush.df$hgLevel, thrush.df$hgDep6MO) # r = -0.08

thrush.df %>%
  filter(species == "BITH") %>%
  ggplot(., aes(x = hgDep1Yr, y = hgLevel)) + geom_smooth() + geom_point(alpha = 0.25) + 
  xlab("Mercury deposition") + ylab("Mercury level") + 
  ggtitle("Correlation between Bicknell's Thrush mercury levels\nand mercury deposition during the past year")
cor(thrush.df$hgLevel, thrush.df$hgDep1Yr) # r = -0.09


thrush.df %>%
  filter(species == "BITH") %>%
  ggplot(., aes(x = hgDep2Yr, y = hgLevel)) + geom_smooth() + geom_point(alpha = 0.25) + 
  xlab("Mercury deposition") + ylab("Mercury level") + 
  ggtitle("Correlation between Bicknell's Thrush mercury levels\nand mercury deposition during the past 2 years")
cor(thrush.df$hgLevel, thrush.df$hgDep2Yr) # r = -0.08


thrush.df %>%
  filter(species == "BITH") %>%
  ggplot(., aes(x = hgDep3Yr, y = hgLevel)) + geom_smooth() + geom_point(alpha = 0.25) + 
  xlab("Mercury deposition") + ylab("Mercury level") + 
  ggtitle("Correlation between Bicknell's Thrush mercury levels\nand mercury deposition during the past 3 years")
cor(thrush.df$hgLevel, thrush.df$hgDep3Yr) # r = -0.05

thrush.df %>%
  filter(ageCat == "ASY") %>%
  ggplot(., aes(x = as.factor(year), y = hgLevel)) + geom_boxplot() + coord_trans(y = "sqrt")

# Do levels change over time?
thrush.df %>%
  ggplot(., aes(y = hgLevel, x = as.factor(year))) + geom_boxplot() + ylab("Blood-mercury concentration in thrushes")
# Models.
## response variable = hgLevels.
## fixed effects: species, year, day-of-year, sex, hgDep1Yr, hgDep6mo
## random effects: individual (bandNum)

library(MuMIn)
library(lme4)

## We can also approach this from a model selection standpoint.
### Two previous papers (Rimmer et al. 2005, 2009) show a seasonal decline in blood mercury levels.
### One paper (Rimmer et al. 2005) also shows an effect of age (older birds have higher burdens) and
### sex (males have higher burdens then females), whereas the other (Rimmer et al. 2009) does not. 
### This suggests the best starting model is a model with an effect of date.
### The next model would include age + sex, to determine whether adding these variables provides a better fit.
### Of these two models, add the effect of species to see whether differences exist between BITH + SWTH.
### Next, consider the possibility of temporal trends by adding a year effect to the best model.
### Finally, ask whether any measure of atmospheric deposition improves model performance.
Cand.models <- list()
#Null
Cand.models[[1]]<- lmer(hgLevel ~ 1 + (1|bandNum), REML = FALSE, data = thrush.df)
summary(Cand.models[[1]])
Cand.models[[2]]<- lmer(hgLevel ~ jdate + (1|bandNum), REML = FALSE, data = thrush.df)
summary(Cand.models[[2]])
Cand.models[[3]]<- lmer(hgLevel ~ jdate + I(jdate^2) + (1|bandNum), REML = FALSE, data = thrush.df)
summary(Cand.models[[3]])
aictab(cand.set = Cand.models, modnames = c("Null","Date", "Quadratic date"), sort = TRUE, nobs = 235)

# We prefer a quadratic effect of date
Cand.models[[4]]<- lmer(hgLevel ~ jdate + I(jdate^2) + ageCat + sexCat + (1|bandNum), REML = FALSE, data = thrush.df)
summary(Cand.models[[4]])
aictab(cand.set = Cand.models, modnames = c("Null","Date", "Quadratic date", "Date+age+sex"), sort = TRUE, nobs = 235)

# We prefer adding effect of age and sex.

Cand.models[[5]]<- lmer(hgLevel ~ jdate + I(jdate^2) + ageCat + sexCat + species + (1|bandNum), REML = FALSE, data = thrush.df)
summary(Cand.models[[5]])
aictab(cand.set = Cand.models, modnames = c("Null","Date", "Quadratic date", "Date+age+sex", "Date+age+sex+species"), sort = TRUE, nobs = 235)

# Species is not a useful predictor.

Cand.models[[6]]<- lmer(hgLevel ~ jdate + I(jdate^2) + ageCat + sexCat + year + (1|bandNum), REML = FALSE, data = thrush.df)
summary(Cand.models[[6]])
aictab(cand.set = Cand.models, modnames = c("Null","Date", "Quadratic date",
                                            "Date+age+sex", "Date+age+sex+species",
                                            "Date+age+sex+year"), sort = TRUE, nobs = 235)

# Yearly trends are not evident.
Cand.models[[7]]<- lmer(hgLevel ~ jdate + I(jdate^2) + ageCat + sexCat + hgDep6MO + (1|bandNum), REML = FALSE, data = thrush.df)
summary(Cand.models[[7]])
Cand.models[[8]]<- lmer(hgLevel ~ jdate + I(jdate^2) + ageCat + sexCat + hgDep1Yr + (1|bandNum), REML = FALSE, data = thrush.df)
summary(Cand.models[[8]])
Cand.models[[9]]<- lmer(hgLevel ~ jdate + I(jdate^2) + ageCat + sexCat + hgDep2Yr + (1|bandNum), REML = FALSE, data = thrush.df)
summary(Cand.models[[9]])
Cand.models[[10]]<- lmer(hgLevel ~ jdate + I(jdate^2) + ageCat + sexCat + hgDep3Yr + (1|bandNum), REML = FALSE, data = thrush.df)
summary(Cand.models[[10]])

# Deposition effects are not important.

Modnames <- c("Null","Date", "Quadratic date",
              "Date+age+sex", "Date+age+sex+species",
              "Date+age+sex+year", "Date+age+sex+6-month dep",
              "Date+age+sex+1-year dep", "Date+age+sex+2-year dep",
              "Date+age+sex+3-year dep")
aictab(cand.set = Cand.models, modnames = Modnames, sort = TRUE, nobs = 235)

# The best model is the date + age + sex model. Adding an effect of species did not change the Log-likelihood.
# Adding effects of 2-year lagged deposition (delta = 1.91), 6-month lagged (delta = 2), 1-year (delta = 2.17),
# or 3-year (delta = 2.2) had approximately no effect on the log-likelihood.
r.squaredGLMM(Cand.models[[4]]) # The marginal (fixed-effects only) r2 = 0.38; the conditional r2 = 0.65.

##Residuals are centered on zero and error is largely similar across levels of the random effect:
plot(Cand.models[[4]], bandNum ~ resid(.), abline = 0)

#Residuals from the final weighted-least-squares regression of the IWLS procedure used to fit the model
##useful, for example, for detecting nonlinearity. 
plot(Cand.models[[4]], resid(., type = "working") ~ fitted(.)|bandNum, abline = 0)

# Can see the lack of a year effect clearly here:
boxplot((resid(Cand.models[[4]])) ~ thrush.df$year, xlab = "Year", ylab = "Residual blood mercury concentration")

##Normality of errors: symmetrical around zero, with heavier tails.
##Heavier tails tend to inflate estimate of within-group error,leading to 
##more conservative tests of fixed effects.
qqnorm(resid(Cand.models[[4]]))


###Errors distributed around zero with approximately constant variance
plot(Cand.models[[4]])
##Look at independence:
# i. plot residuals vs each covariate in the model
plot(Cand.models[[4]], resid(.) ~jdate|bandNum)
plot(Cand.models[[4]], sexCat~ resid(.), abline = 0)
plot(Cand.models[[4]], ageCat ~ resid(.), abline = 0)


##Look at predicted vs. observed
plot(predict(Cand.models[[4]]),thrush.df$hgLevel)
abline(0,1)

## Parameter estimates from the best model:
confint(Cand.models[[4]], level = 0.95, method = "profile")
library(lmerTest)
best.mod <- lmer(hgLevel ~ jdate + I(jdate^2) + ageCat + sexCat + (1|bandNum), REML = FALSE, data = thrush.df)
anova(best.mod)
summary(best.mod)

theta <- getME(Cand.models[[4]],"theta")
## diagonal elements are identifiable because they are fitted
##  with a lower bound of zero ...
diag.element <- getME(Cand.models[[4]],"lower")==0
any(theta[diag.element]<1e-5)


newdata <- data.frame(sexCat = c(rep("Male",360), rep("Female",360)),
                      ageCat = rep(c(rep("HY",120), rep("SY",120), rep("ASY",120)),2),
                      jdate = rep(seq(min(thrush.df$jdate), max(thrush.df$jdate),1),6),
                      bandNum = "999")

predictions <- as.data.frame(predict(Cand.models[[4]], newdata = newdata, re.form = ~(1|bandNum), 
        allow.new.levels = TRUE))
# No standard errors are provided with the "predict" function.
# 
bootCI <- function(model) {
  predict(model, newdata = newdata, re.form = ~(1|bandNum), 
          allow.new.levels = TRUE)
}
bootCI(Cand.models[[4]])  

CI <- bootMer(Cand.models[[4]], bootCI, nsim = 1000)
head(CI) 

predCL <- t(apply(CI$t, MARGIN = 2, FUN = quantile, probs = c(0.025, 0.975)))

predictions$lci <- predCL[, 1]
predictions$uci <- predCL[, 2]

predictions <- cbind(predictions, newdata)


colnames(predictions) <- c("predicted","lower95CI","upper95CI","sex","age","date",
                           "bandNum")
predictions$age <- factor(predictions$age, levels = c("HY","SY", "ASY"))
colnames(predictions)[4] <- "Sex"

ggplot(predictions, aes(x = date, y = predicted, color = Sex, fill = Sex)) + geom_ribbon(aes(ymin = lower95CI,
                                                                             ymax = upper95CI, linetype = NA),
                                                                             alpha = 0.6) + 
  facet_wrap(~age) + ylab("Predicted blood-mercury concentration") + xlab("Date")

colnames(predictions)[5] <- "Age"

ggplot(predictions, aes(x = date, y = predicted, color = Age, fill = Age)) + geom_ribbon(aes(ymin = lower95CI,
                                                                                             ymax = upper95CI, linetype = NA),
                                                                                         alpha = 0.6) + 
  facet_wrap(~Sex) + ylab("Predicted blood-mercury concentration") + xlab("Date")