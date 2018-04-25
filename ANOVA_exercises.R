# ANOVA Exercises ---------------------------------------------------------------

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggpubr)
library(ggplot2)
library(Rmisc)

# Exercise 1  -------------------------------------------------------------

# Enter the mass at the end of the experiment
feed_1 <- c(60.8, 57.0, 65.0, 58.6, 61.7)
feed_2 <- c(68.7, 67.7, 74.0, 66.3, 69.8)
feed_3 <- c(102.6, 102.1, 100.2, 96.5)
feed_4 <- c(87.9, 84.2, 83.1, 85.7, 90.3)

# Make a dataframe
bacon <- as.tibble(data.frame(
  feed = c(
    rep("Feed 1", length(feed_1)),
    rep("Feed 2", length(feed_2)),
    rep("Feed 3", length(feed_3)),
    rep("Feed 4", length(feed_4))
  ),
  mass = c(feed_1, feed_2, feed_3, feed_4)
))

bacon <- as_tibble(bacon)
bacon_1 <- aov(mass ~ feed, data = bacon)
summary(bacon_1)

ggplot(data = bacon_1, aes(x = feed, y = mass)) +
  geom_boxplot(aes(fill = feed), notch = TRUE)

# Concluding sentence -----------------------------------------------------

# There is a significant differnce between feed type and the mass of pigs,
# therefore, feed type does affect the mass of pigs 

# Exercise 2 --------------------------------------------------------------

 teeth <- datasets::ToothGrowth

# Hypotheis 
# H0: There is no significant difference in the delivery method
# with respect to the dose level of vitamin C
# H1: There is a significant difference in the delivery method
# with respect to the dose level of vitamin C 
tooth.summary <- ToothGrowth %>% 
  group_by(supp) %>% 
  summarise(mean_dose = mean(dose),
            sd_dose = sd(dose)) %>% 
  ungroup()
tooth.summary

# Test a hypothesis -------------------------------------------------------
# First calculate SE and CI
tooth.summary2 <- Rmisc::summarySE(data = ToothGrowth,
                                    measurevar = "dose",
                                    groupvars = c("supp"))

# Then visualise the data - box plot
ggplot(data = ToothGrowth, aes(x = supp, y = dose)) +
  geom_segment(data = tooth.summary2, aes(x = supp, xend = supp, y = dose - ci, yend = dose + ci, colour = supp),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = supp), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)

# ANOVA
tooth.supp.aov <- aov(dose ~ supp, data = ToothGrowth) 
summary(tooth.supp.aov)

#Concluding Statement 
# Do not reject null hypothesis
# There is no significant difference in the delivery method
# with respect to the dose level of vitamin C
# They are the same p = 1 

# Testing assumptions afterwards 
# First vusualise normality of data 
ToothGrowth.residuals <- residuals(tooth.supp.aov)
hist(ToothGrowth.residuals)

# Then visualise homoscedasticity of results 
plot(fitted(tooth.supp.aov), residuals(tooth.supp.aov))

# Check Tukey results 
ToothGrowth.tukey <- TukeyHSD(tooth.supp.aov, which = "supp")
plot(ToothGrowth.tukey)

# Exercise 3  -------------------------------------------------------------

biomass2 <- read_csv2("biomass2.csv")

# Hypothesis 
# There is no significant difference between sites with respect to biomass
# There is a significant difference between sites with respect to biomass
Isolepis.summary <- biomass2 %>% 
  group_by(Site) %>% 
  summarise(mean_Biomass = mean(Biomass),
            sd_Biomass = sd(Biomass)) %>% 
  ungroup()
Isolepis.summary

# Test a hypothesis -------------------------------------------------------
# First calculate SE and CI
Isolepis.summary2 <- Rmisc::summarySE(data = biomass2,
                                   measurevar = "Biomass",
                                   groupvars = c("Site"))

# Then visualise the data - box plot
ggplot(data = biomass2, aes(x = Site, y = Biomass)) +
  geom_segment(data = Isolepis.summary2, aes(x = Site, xend = Site, y = Biomass - ci, yend = Biomass + ci, colour = Site),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = Site), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)

# Two-way ANOVA
Isolepis.aov <- aov(Biomass ~ Site + Depth, data = biomass2)
summary(Isolepis.aov)

# Do not reject null hypothesis 
# There is no significant difference between sites or depth 
# with respect to biomass

# Testing assumptions afterwards 
# First vusualise normality of data 
Isolepis.residuals <- residuals(Isolepis.aov)
hist(Isolepis.residuals)

# Then visualise homoscedasticity of results 
plot(fitted(Isolepis.aov), residuals(Isolepis.aov))

# Check Tukey results 
Isolepis.tukey <- TukeyHSD(Isolepis.aov, which = "Site")
plot(Isolepis.tukey)

#The end of the ANOVA exercises 















