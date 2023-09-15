rm(list = ls())
graphics.off()

library(vars)
library(devtools)
install_github("dgrtwo/broom")
library(broom)


setwd("C:\\Users\\super\\OneDrive\\Desktop\\thesis clean\\by country")
data <- read.csv(file = "restricted_data.csv")


summary(data)
sapply(data, sd, na.rm=TRUE)

data[-c(1,16)]
drop
cor(data[-c(1,18)])
#regression
PDI + i_targeting + COR  + GDPC + DEV
lmodel <- lm(INEF ~ 0 + i_targeting + COR  + GDPC + DEV, data = data)
summary(lmodel)
lmodel$coefficients
lmodel$std

lmodel <- lm(INEF ~ 0 +  IND  + MAS + UAV +  LTO + IVR + i_targeting + GDPC + DEV + COR + SOC + CRISIS, data = data)
summary(lmodel)


#Upload the data

plot(data$INEF, pch = 16, col = "blue") #Plot the results
abline(lmodel) #Add a regression line

plot(lmodel$residuals, pch = 16, col = "red")

plot(cooks.distance(lmodel), pch = 16, col = "blue")

model.res <- resid(lmodel)

glsmodel <- gls(INEF ~ 0 + PDI + IND  + MAS + UAV +  LTO + IVR + i_targeting + COR + SOC + LAW + GDPC + DEV, data = data)
summary(glsmodel)

plot(data$IND, model.res)

colnames(data)

plot(data$INEF,data$IVR)

?plot

cor_data <- data
cor_data$Year <- NULL
cor_data$ï..Country <- NULL
cor <- cor(cor_data)
round(cor, 2)


#Robustness

setwd("C:\\Users\\super\\OneDrive\\Desktop\\thesis clean\\subsets")
crisis <- read.csv(file = "crisis.csv")
no_crisis <- read.csv(file = "no_crisis.csv")
target <- read.csv(file = "target.csv")
no_target <- read.csv(file = "no_target.csv")
developed <- read.csv(file = "developed.csv")
no_developed <- read.csv(file = "no_developed.csv")


lm_crisis <- lm(INEF ~ 0 + PDI + IND  + MAS + UAV +  LTO + IVR + i_targeting + COR + SOC + LAW  + DEV, data = crisis)
summary(lm_crisis)
lm_nocrisis <- lm(INEF ~ 0 + PDI + IND  + MAS + UAV +  LTO + IVR + i_targeting + COR + SOC + LAW  + DEV, data = no_crisis)
summary(lm_nocrisis)

lm_target <- lm(INEF ~ 0 + PDI + IND  + MAS + UAV +  LTO + IVR + COR + SOC + LAW + GDPC + DEV, data = target)
summary(lm_target)
lm_notarget <- lm(INEF ~ 0 + PDI + IND  + MAS + UAV +  LTO + IVR + COR + SOC + LAW + GDPC + DEV, data = no_target)
summary(lm_notarget)

lm_developed <- lm(INEF ~ 0 + PDI + IND  + MAS + UAV +  LTO + IVR + i_targeting + COR + SOC + LAW + GDPC, data = developed)
summary(lm_developed)
lm_nodeveloped <- lm(INEF ~ 0 + PDI + IND  + MAS + UAV +  LTO + IVR + i_targeting + COR + SOC + LAW + GDPC, data = no_developed)
summary(lm_nodeveloped)
