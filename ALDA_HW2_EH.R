

library(lme4)
library(tidyverse)
library(broom)

PDS_data_mlm <- read.csv(file = "~/Documents/PDS_project/Longitudinal_project/datasets/PDS_long_hw.csv")
#View(PDS_data_mlm)
PDS_data_mlm$AGEMOSCAN <- as.numeric(PDS_data_mlm$AGEMOSCAN)
PDS_data_mlm$ADHDsum <- as.numeric(PDS_data_mlm$ADHDsum)
PDS_data_mlm$ADHD_INsum <- as.numeric(PDS_data_mlm$ADHD_INsum)
PDS_data_mlm$ADHD_HYIMsum<- as.numeric(PDS_data_mlm$ADHD_HYIMsum)

#1.Run linear models on all of your subjects (a basic regression). What is the average intercept, 
#the average slope?
lm1 <- lm(ADHDsum ~ AGEMOSCAN, data = PDS_data_mlm)
summary(lm1)
anova(lm1)

#2. Now run a mlm/lmer model with only a random intercept. What is the ICC? 
#What does residual variance look like compared to linear model? Create a graph to show this effect.
mod.1 <- lmer(ADHDsum ~ 1 + (1 | Subid_fMRI), data = PDS_data_mlm)
summary(mod.1)
#the fixed effects is like basic regression, the random effects gives you a descriptive dispersion

#Hand calculate ICC
7.032/(7.032 + 4.360)
#or use this library(sjstats)
#icc(mod.1)

#remove missing data to plot residuals
library(dplyr) #can select specific variables
PDS_data_mlm_rm <- PDS_data_mlm %>% #only include selected variables in newdata
  select(Subid_fMRI, ADHDsum, CONP_EF_T, SAL_GE_K1to5, SAL_GE_K6to10)
PDS_data_mlm_rm <- na.omit(PDS_data_mlm_rm)
#View(PDS_data_mlm_rm)

mod.2 <- lmer(ADHDsum ~ 1 + (1 | Subid_fMRI), data = PDS_data_mlm_rm)
summary(mod.2)

####Scatterplot residuals of model 1
residual.adhdsum <- resid(mod.2)
PDS_data_mlm_rm$residual.adhdsum <- residual.adhdsum
#plotted residuals
ggplot(data = PDS_data_mlm_rm, aes(y = residual.adhdsum, x = Subid_fMRI)) + 
  geom_point() +
  geom_smooth(method = "lm",  color = "blue") +
  ggtitle("ADHD Residuals")

#3. Introduce a fixed slope term. What is the difference in terms of the fixed effects estimates between 
#this estimate and the previous? Of the residual standard error? Create a graph to show both fixed effects estimates and the CIs around them.
mod.2fixed <- lmer(ADHDsum ~ 1 + agemo_converted + (1 | Subid_fMRI), data = PDS_data_mlm)
summary(mod.2fixed)
#this adds in time (here age) to the fixed slope model. Allows us to see how ADHD symptoms are varying across age.
library(sjPlot)
sjp.lmer(mod.2fixed, facet.grid = FALSE, 
         sort = "sort.all")

#4. Run an additional model with a random slope. How does this change compare to the previous model? 
#Should you keep the random slope or not?
mod.3 <- lmer(ADHDsum ~ 1 + agemo_converted + (agemo_converted | Subid_fMRI), data=PDS_data)
summary(mod.3)

#5. Interpret the correlation between the slope and the intercept.
#negative correlation: as age increases, ADHD symptoms decrease

#6. Create a density plot of the random effects from your final model.
library(merTools)
re.sim <- REsim(mod.3)
head(re.sim)
p1.gg1 <- re.sim %>% 
  filter(term == "(Intercept)") 
###getting a class error here I don't understand
#Error: class(merMod) %in% c("lmerMod", "glmerMod", "blmerMod", "bglmerMod") is not TRUE
#had LmerTest loaded as a package; just get rid of this it overrides the class

ggplot(p1.gg1, aes(mean)) +
  geom_density()

#7. Create a catepillar plot of the random effects. Is there any person that seems odd in terms of a 
#large standard errors around intercept and slope estimates?

random_params <- tidy(mod.3, effect = "ran_modes")
head(random_params)
fe.sim <- FEsim(mod.3)
head(fe.sim)

(p1 <- plotREsim(re.sim))

#8. Create a plot of the trajectory, along with a spaghetti plot of each person’s individual slope. 
#Set the alpha level (transparency) on the individual slopes to make them easier to see.

gg2 <- ggplot(PDS_data, aes(x = AGEMOSCAN, y = ADHDsum, group = Subid_fMRI)) + 
  geom_point() + 
  geom_line(alpha = .5) + 
  aes(colour = factor(Subid_fMRI)) + 
  guides(colour=FALSE) +
  theme_classic()
gg2
