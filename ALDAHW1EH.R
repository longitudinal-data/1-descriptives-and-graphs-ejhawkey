#############################################
#### Applied Longitudinal Data Analysis #####
#### Multilevel modeling PDS dataset ########
#### 9/5/2017; HW 1 Elizabeth Hawkey ########
#############################################

#load lme4 package: lme4 provides functions for fitting and analyzing mixed models: linear (lmer), 
#generalized linear (glmer) and nonlinear (nlmer.)
library(lme4)
library(tidyverse)
library(broom)
library(dplyr)
library(tidyr)

#load in newest dataset 
setwd("~/Documents/PDS_project/Longitudinal_project")
PDS_data_ALDA <- read.csv("~/Documents/PDS_project/Longitudinal_project/stats/PDS_dataset_stats_HW1.csv")
#View(PDS_data_ALDA)
print(names(PDS_data_ALDA))

#1 Move your data into a long format and a wide format. 
#Did you have any specific challenges that you encountered? If so, discuss them.
##I had to rename all of my variables to use a common separator and had to change the 
##type of variable to date prior to importing.

wide_to_long <- PDS_data_ALDA %>%
  gather(ADHDsum_1:age_3, key = "time", value = "value") 
#View(wide_to_long)

wide_to_long2 <- wide_to_long %>%
  separate(time, into = c("var", "scan", "omit"), sep = "_", convert = TRUE) %>%
  select(-omit) %>%
  spread(key = var, value = value) %>%
  arrange(Subid) 
#View(wide_to_long2)

#this is another way to use separate using a specific character location
#separate(time, into = c("var", "scan", "omit"), sep = c(8,9), convert = TRUE) %>%

long_to_wide <- wide_to_long %>%
   spread(time, value)


#2 Create a wave variable and date variable (if applicable).
##scan is the wave variable I will be using (created in the previous step), 
##date of assessment was included in my dataset.

#3 What is your sample size for each wave of assessment?
wide_to_long2$ADHDsum <- as.numeric(wide_to_long2$ADHDsum)
wide_to_long2$FPNGE15 <- as.numeric(wide_to_long2$FPNGE15)
wide_to_long2$age <- as.numeric(wide_to_long2$age)

library(psych)
describeBy(wide_to_long2$ADHDsum, wide_to_long2$scan, mat=TRUE)
#scan1 = 207
#scan2 = 190
#scan3 = 157

#4 Take the date variable and convert it to a different date format such as time in study 
#or age (if appropriate). What scale is most suitable for your analyses? (weeks/months/years?)
#convert imported character date variables to date format
DATE_1 <- as.Date(long_to_wide$DATE_1, format = "%m/%d/%y")
DATE_2 <- as.Date(long_to_wide$DATE_2, format = "%m/%d/%y")
DATE_3 <- as.Date(long_to_wide$DATE_3, format = "%m/%d/%y")
##if captial Y imports the year as: "0008"; for "2008" use lowercase y for current century
#strptime() for different classes

#difference in weeks between each assessment
datediff_1 <- formatC(difftime(DATE_1, DATE_1, units = "weeks")/100)
datediff_2 <- formatC(difftime(DATE_2, DATE_1, units = "weeks")/100)
datediff_3 <- formatC(difftime(DATE_3, DATE_1, units = "weeks")/100)

#add time variable back to wide data frame
long_to_wide$datediff_1 <- as.numeric(datediff_1)
long_to_wide$datediff_2 <- as.numeric(datediff_2)
long_to_wide$datediff_3 <- as.numeric(datediff_3)
#View(long_to_wide)

#return to long format for plotting
plot_long <- long_to_wide %>%
  gather(ADHDsum_1:datediff_3, key = "time", value = "value") 
#View(plot_long)

plot_long2 <- plot_long %>%
  separate(time, into = c("var", "scan", "omit"), sep = "_", convert = TRUE) %>%
  select(-omit) %>%
  spread(key = var, value = value) %>%
  arrange(Subid) 
#View(plot_long2)

#4 Graph your data using the different time metrics, fitting individual curves for each person.
library(ggplot2)
#subset of participants to view graphs
plot_long3 <- subset(plot_long2, FPNGE15 > 0)
plot_long3$datediff <- as.numeric(plot_long3$datediff)
plot_long3$age <- as.numeric(plot_long3$age)
plot_long3$FPNGE15 <- as.numeric(plot_long3$FPNGE15)
#View(plot_long3)

gg1 <- ggplot(data = plot_long3,  
              aes(x = age, y = FPNGE15, group = Subid)) + geom_point()
gg1

gg1x <- ggplot(data = plot_long3,  
              aes(x = datediff, y = FPNGE15, group = Subid)) + geom_point()
gg1x

gg2 <- ggplot(data = plot_long3,
              aes(x = datediff, y = FPNGE15, group = Subid)) + geom_line() +
              aes(colour = factor(Subid)) + guides(colour=FALSE) 
gg2

#5 Create an overall average trend of your data (split up into groups if appropriate). 
#Attempt to color your individual data points and/or shade different lines (highlight some 
#particiapnts,highlight the average trend line but not the individual level lines)
gg3 <- ggplot(plot_long3, aes(x = age, y = FPNGE15)) + geom_point() + stat_smooth()
gg3
gg4 <- ggplot(plot_long3, aes(x = age, y = FPNGE15)) + geom_point() + stat_smooth(aes(colour = Subid), method = "lm", se = TRUE) +
  aes(colour = factor(Subid)) + guides(colour=FALSE) 
gg4

#6 Look at the correlations of your DV across time
cor.dv <- subset(PDS_data_ALDA, select = c(FPNGE15_1, FPNGE15_2, FPNGE15_3))
cor(cor.dv, use = "pairwise.complete.obs")

