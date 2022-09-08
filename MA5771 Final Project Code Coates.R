#Week 6 Final Project Component
#MA5771
#Brandon Coates


#Load libraries
library(GLMsData)
library(statmod)
library(MASS)
library(rockchalk)
library(ggplot2)
library(ggpubr)


#Import Data
alone <- read.csv('FinalProjectData.csv')
head(alone)
str(alone)

#----------------------------------------------
# Data management
# ---------------------------------------------

#Convert levels of categorical variables to coded factors for ease of use
alone$gender <- factor(alone$gender)
levels(alone$gender)
levels(alone$gender) <- c("0", "1", "2", "3")

alone$bodyweight <- factor(alone$bodyweight)
levels(alone$bodyweight)
levels(alone$bodyweight) <- c("0", "1", "2", "3")
levels(alone$bodyweight)


alone$sexuallity <- factor(alone$sexuallity)
levels(alone$sexuallity) <- c("0", "1", "2")
levels(alone$sexuallity)

alone$virgin <- factor(alone$virgin)
levels(alone$virgin) <- c("0", "1")
levels(alone$virgin)

alone$social_fear <- factor(alone$social_fear)
levels(alone$social_fear) <- c("0", "1")
levels(alone$social_fear)

alone$depressed <- factor(alone$depressed)
levels(alone$depressed) <- c("0", "1")
levels(alone$depressed)

alone$attempt_suicide <- factor(alone$attempt_suicide)
levels(alone$attempt_suicide) <- c("0", "1")
levels(alone$attempt_suicide)

#Combine Levels of income to represent economic class
alone$income <- factor(alone$income)
levels(alone$income)
alone$income <- combineLevels(alone$income, levs = c("$0 ", "$1 to $10,000", "$10,000 to $19,999", "$20,000 to $29,999", "$30,000 to $39,999"), newLabel = "Low")
alone$income <- combineLevels(alone$income, levs = c("$40,000 to $49,999", "$50,000 to $74,999", "$50,000 to $74,999", "$75,000 to $99,999"), newLabel = "Middle")
alone$income <- combineLevels(alone$income, levs = c("$100,000 to $124,999", "$125,000 to $149,999", "$150,000 to $174,999", "$174,999 to $199,999", "$200,000 or more"), newLabel = "Upper")
levels(alone$income)

#Recode levels of income
levels(alone$income) <- c("0", "1", "2")
levels(alone$income)

#----------------------------------------------
# Data Exploration
# ---------------------------------------------
#Plot attempted suicide versus gender
gplot <- ggplot(alone, aes(gender))+
  geom_bar(aes(fill = attempt_suicide), position = "dodge")+
  labs(x = "Gender", y = "Count")+
  scale_y_continuous()+
  theme_classic()+
  theme(legend.position = "top")
#Plot attempted suicide versus sexuallity
sexplot <- ggplot(alone, aes(sexuallity))+
  geom_bar(aes(fill = attempt_suicide), position = "dodge")+
  labs(x = "Sexuallity", y = "Count")+
  scale_y_continuous()+
  theme_classic()+
  theme(legend.position = "top")
#Plot attempted suicide versus income
incomeplot <-ggplot(alone, aes(income))+
  geom_bar(aes(fill = attempt_suicide), position = "dodge")+
  labs(x = "Income", y = "Count")+
  scale_y_continuous()+
  theme_classic()+
  theme(legend.position = "top")
#Plot Age versus attempted suicide responses
ageplot <- ggplot(alone, aes(age))+
  geom_bar(aes(fill = attempt_suicide), position = "dodge")+
  labs(x = "Age", y = "Count")+
  scale_y_continuous()+
  theme_classic()+
  theme(legend.position = "top")
#Plot Friends versus attempted suicide response
friendplot <- ggplot(alone, aes(friends))+
  geom_freqpoly()
  labs(x = "Number of Friends", y = "Count")+
  ylim(0,30)+
  xlim(0,600)+
  theme_classic()+
  theme(legend.position = "top")
#Plot depressed against attempted suicide response
depressedplot <- ggplot(alone, aes(depressed))+
  geom_bar(aes(fill = attempt_suicide), position = "dodge")+
  labs(x = "Depressed", y = "Count")+
  scale_y_continuous()+
  theme_classic()+
  theme(legend.position = "top")

ggarrange(gplot, sexplot, incomeplot, ageplot, friendplot, depressedplot,
          ncol=2, nrow = 3)

#----------------------------------------------
# GLM fitting
# ---------------------------------------------
#Fit full model with all explanatory variables
mod.full <- glm(attempt_suicide ~ gender + sexuallity + age + income + bodyweight + virgin + friends + social_fear + depressed, data = alone, family = binomial())
summary(mod.full)

# Pearson stat for goodness-of-fit
df.res <- df.residual(mod.full)
gm.pearson <- sum(resid(mod.full, type = "pearson")^2) # deviance
pearson.p <- 1 - pchisq(gm.pearson, df = df.res)
c(df.res = df.res, pearson = gm.pearson, p = pearson.p)

#Check Assumptions of full model
#Create quantile residuals and transformed fitted values
rQ <- qresid(mod.full)
x.u <- asin(sqrt(fitted(mod.full, type = "response")))
mu <- fitted(mod.full)
par(mfrow = c(2, 2))

#Create plot for standardized deviance residuals versus constant-information scale
scatter.smooth(rQ ~ x.u, ylab = "Quantile Residuals",
               xlab = expression(paste(sin^{-1},"(",sqrt(hat(mu)),")")), main = "A: Quantile Residuals\n against Scaled Fitted Values", las = 1)

#Create plot of working responses against linear predictors
rW <- resid(mod.full, type = "working") # working residuals
lp <- predict(mod.full, type = "link") # linear predictor
z <- rW + lp # working response
scatter.smooth(z ~ lp, las =  1, ylab = "Working responses", 
               xlab = "Linear predictor", main = "B: Working Responses versus\n Linear Predictors")

#Create quantile-quantile plot
qqnorm(rQ, las = 1)
qqline(rQ)

#Create scatterplot of the Cook's Distance
plot(cooks.distance(mod.full), ylab = "Cook's distance", 
     las = 1, type = "h", main = "D: Cook's Distance Index")


#Remove least signficant variable to reduce model
mod.1 <- glm(attempt_suicide ~ gender + sexuallity + income + bodyweight + virgin + friends + social_fear + depressed, data = alone, family = binomial)
summary(mod.1)

mod.2 <- glm(attempt_suicide ~ gender + sexuallity + income + bodyweight + virgin + friends + depressed, data = alone, family = binomial)
summary(mod.2)

mod.3 <- glm(attempt_suicide ~ gender + sexuallity + bodyweight + virgin + friends + depressed, data = alone, family = binomial)
summary(mod.3)

mod.4 <- glm(attempt_suicide ~ gender + sexuallity + bodyweight + friends + depressed, data = alone, family = binomial)
summary(mod.4)

mod.5 <- glm(attempt_suicide ~ gender + sexuallity + friends + depressed, data = alone, family = binomial)
summary(mod.5)

mod.6 <- glm(attempt_suicide ~ gender + sexuallity + depressed, data = alone, family = binomial)
summary(mod.6)

#Use ANOVA to check models
anova(mod.1, mod.full, test = "Chisq")
anova(mod.2, mod.1, test = "Chisq")
anova(mod.3, mod.2, test = "Chisq")
anova(mod.4, mod.3, test = "Chisq")
anova(mod.5, mod.4, test = "Chisq")
anova(mod.6, mod.5, test = "Chisq")

#Check models using AIC and BIC
c(AIC.m0 = AIC(mod.full), AIC.m1 = AIC(mod.1), AIC.m2 = AIC(mod.2), AIC.m3 = AIC(mod.3), AIC.m4 = AIC(mod.4), AIC.m5 = AIC(mod.5))
c(BIC.m0 = BIC(mod.full), BIC.m1 = BIC(mod.1), BIC.m2 = BIC(mod.2), BIC.m3 = BIC(mod.3), BIC.m4 = BIC(mod.4), BIC.m5 = BIC(mod.5))

#Rename significant model as final model
mod.final <- mod.5
summary(mod.final)

#Check Assumptions of final
#Create quantile residuals and transformed fitted values
rQ <- qresid(mod.final)
x.u <- asin(sqrt(fitted(mod.final, type = "response")))
mu <- fitted(mod.final)
par(mfrow = c(2, 2))

#Create plot for standardized deviance residuals versus constant-information scale
scatter.smooth(rQ ~ x.u, ylab = "Quantile Residuals",
               xlab = expression(paste(sin^{-1},"(",sqrt(hat(mu)),")")), main = "A: Quantile Residuals\n against Scaled Fitted Values", las = 1)

#Create plot of working responses against linear predictors
rW <- resid(mod.final, type = "working") # working residuals
lp <- predict(mod.final, type = "link") # linear predictor
z <- rW + lp # working response
scatter.smooth(z ~ lp, las =  1, ylab = "Working responses", 
               xlab = "Linear predictor", main = "B: Working Responses versus\n Linear Predictors")

#Create quantile-quantile plot
qqnorm(rQ, las = 1)
qqline(rQ)

#Create scatterplot of the Cook's Distance
plot(cooks.distance(mod.final), ylab = "Cook's distance", 
     las = 1, type = "h", main = "D: Cook's Distance Index")

