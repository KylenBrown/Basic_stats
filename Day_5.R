# Day_5.R
# The fifth day of the class 
# Purpose: ANOVA Day 2 
# 20 April 2018 

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggpubr)
library(Rmisc)

# Load data ---------------------------------------------------------------
snakes <- read_csv("snakes.csv") %>% 
  mutate(day = as.factor(day))
# OR
# snakes$day = as.factor(snakes$day)

# Summarise the data  ------------------------------------------------------
snakes.summary <- snakes %>% 
  group_by(day) %>% # Counts how many snakes occur in a day - only have one snake a day(Na)
  summarise(mean_openings = mean(openings),
            sd_openings = sd(openings)) %>% 
  ungroup()
snakes.summary

# Assuption of ANOVA violated
# Data not independent
# used the same snakes every day

# H0: There is no difference between the number of snake openings 
# for snakes to become habituated from day to day 
# H1: There is a difference between the number of snake openings 
# for snakes to become habituated from day to day 

# Test a hypothesis -------------------------------------------------------
# First calculate SE and CI
snakes.summary2 <- summarySE(data = snakes,
                             measurevar = "openings",
                             groupvars = c("day"))

# Then visualise the data - box plot
ggplot(data = snakes, aes(x = day, y = openings)) +
  geom_segment(data = snakes.summary2, aes(x = day, xend = day, y = openings - ci, yend = openings + ci, colour = day),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = day), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)

# But wait, we will have two factors, so we need another null hypothesis

# H0: There is no difference between snakes with respect to 
# the number of openings at which they habituate.
# H0: There is no difference between days in terms of 
# the number of openings at which the snakes habituate.

snakes.day.aov <- aov(openings ~ day, data = snakes) # Analysis of Variance , Y as a function of X
summary(snakes.day.aov)

# Test both hypothesis 
snakes.all.aov <- aov(openings ~ day + snake, data = snakes)
summary(snakes.all.aov)
# Residuals - show amount of error in the data - unexplained variation 

# Testing assumptions afterwards ------------------------------------------
# First vusualise normality of data 
snakes.residuals <- residuals(snakes.all.aov)
hist(snakes.residuals)

# Then visualise homoscedasticity of results 
plot(fitted(snakes.all.aov), residuals(snakes.all.aov))

# Check Tukey results 
snakes.tukey <- TukeyHSD(snakes.all.aov, which = "day")
plot(snakes.tukey)

# Visualise the factor interaction 
ggplot(data = snakes, aes(x = as.numeric(day),
                          y = openings,
                          colour = snake)) +
  geom_line(size = 3) +
  geom_point(size = 4)

# Exercises ---------------------------------------------------------------
# Get the moth data from Github
# Run a two-way ANOVA on them 
# Put moths in a tree - recorded number of moths traps caught 
# traps- scent,sugar and chemical - idea to see which traps are effective 
# and also where is the best place to put the traps 
# Do Numerical and graphical summaries 
# Generate hypotheses 

# Load data 
moths <- read_csv("moths_traps.csv") %>% 
  gather(key = "trap", value = "count", -Location)

# Summarise the data for both Location and trap  ------------------------------------------------------
moths.Location.summary <- moths %>% 
  group_by(Location) %>% 
  summarise(mean_count = mean(count),
            sd_count = sd(count)) %>% 
  ungroup()
moths.Location.summary

moths.trap.summary <- moths %>% 
  group_by(trap) %>% 
  summarise(mean_count = mean(count),
            sd_count = sd(count)) %>% 
  ungroup()
moths.trap.summary

# Hypotheses
# H0: There is no difference between moths caught with respect to 
# location of trap
# H0: There is no difference between moths caught with respect to 
# the type of trap 

moths.Location.summary2 <- summarySE(data = moths,
                             measurevar = "count",
                             groupvars = c("Location"))
moths.trap.summary2 <- summarySE(data = moths,
                                 measurevar = "count",
                                 groupvars = c("trap"))

# Then visualise the data - box plot
Location <- ggplot(data = moths, aes(x = Location, y = count)) +
  geom_segment(data = moths.Location.summary2, aes(x = Location, xend = Location, y = count - ci, yend = count + ci, colour = Location),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = Location), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)
Location

Trap <- ggplot(data = moths, aes(x = trap, y = count)) +
  geom_segment(data = moths.trap.summary2, aes(x = trap, xend = trap, y = count - ci, yend = count + ci, colour = trap),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = trap), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)
Trap

Final <- ggarrange(Location,Trap,
                   ncol = 2, nrow = 1,
                   labels = "AUTO",common.legend = TRUE)
Final

# ANOVA
moths.Location.aov <- aov(count ~ Location, data = moths) 
summary(moths.Location.aov)
moths.trap.aov <- aov(count ~ trap, data = moths) 
summary(moths.trap.aov)
# Testing both together 
moths.all.aov <- aov(count ~ Location + trap, data = moths)
summary(moths.all.aov)

# Visualise normality of data 
moths.residuals <- residuals(moths.all.aov)
hist(moths.residuals)

# Visualise homoscedasticity of results 
plot(fitted(moths.all.aov), residuals(moths.all.aov))

# Check Tukey results 
moths.tukey <- TukeyHSD(moths.all.aov, which = "Location")
plot(moths.tukey)

moths.trap.tukey <- TukeyHSD(moths.all.aov, which = "trap")
plot(moths.trap.tukey)

# LOcation:Reject null hypothesis , there is a difference 
# between moths caught with respect to location of trap
# Trap:Do not reject null hypothesis , there is no difference 
# between moths caught with respect to the type of trap 

# Regressions -------------------------------------------------------------
# For the explanation of this statistical analysis 
# We are going to use eruption data from Ol faithful
head(faithful)

# Plot a quick scatterplot 
ggplot(data = faithful, aes(x= waiting, y = eruptions)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, colour = "hotpink")

# Form a hypotheis 
# Ho : Waiting time does not influence the duration of an eruption 
# H1 : Waiting time does influence the duration of an eruption 

# Test a hypothesis  ------------------------------------------------------
faithful_lm <- lm(eruptions ~ waiting, data = faithful)
summary(faithful_lm) # co-effiecient - no signifint difference between the slope of the line and 0

# Reject null hypothesis, waiting time does influence 
# the duration of an eruption 

# Correlations ------------------------------------------------------------
# Load libraries
library(tidyverse)
library(ggpubr)
library(corrplot)

# Load data
ecklonia <- read_csv("ecklonia.csv")

# Formulate a hypothesis 

# H0: There is no relationship between frond length and frond mass
# for the kelp Ecklonia maxima 
# H1: There is a realtionship between frond length and frond mass
# for the kelp Ecklonia maxima 
# Choose a different variable for your own test 

# Testing a hypothesis  ---------------------------------------------------

cor.test(ecklonia$frond_length, ecklonia$frond_mass)

# Reject null hypotheis, there is a realtionship between 
# frond length and frond mass for the kelp Ecklonia maxima

#Visualise the data 
ggplot(data = ecklonia, aes(x = frond_length, y = frond_mass))+
  geom_point()

# Run hecka tests at once  ------------------------------------------------
 ecklonia_sub <- ecklonia %>% 
  select(stipe_length:epiphyte_length)

ecklonia_cor <- cor(ecklonia_sub)
ecklonia_cor

# Spearman rank test  -----------------------------------------------------
# Used for ordinal data 
# There is no relationship between the order that we choose to sample kelp 
# and the size of the kelp for example 

ecklonia$length <- as.numeric(cut((ecklonia$stipe_length + ecklonia$frond_length),3))
ecklonia$length

# Then run a Spearman test
cor.test(ecklonia$length, ecklonia$primary_blade_length, method = "spearman")

# Kendall data 
cor.test(ecklonia$primary_blade_length, ecklonia$primary_blade_width, method = "kendall")

# Visualise all the things  -----------------------------------------------
ecklonia_pearson <- cor(ecklonia_sub)
ecklonia_pearson
corrplot(ecklonia_cor, method = "circle")

# Produce a heat map for bonus  ------------------------------------------------------
# If data is not in matrix form , then create a matrix
# ecklonia_matrix <- data.matrix(ecklonia_pearson) for example 

# Create a palette 
ecklonia_palette <- colorRampPalette(c("yellow", "red", "darkgreen"))(n = 299)

# Produce heat map 
ecklonia_heat <- heatmap(ecklonia_pearson, Rowv=NA, Colv=NA,
                         col = ecklonia_palette, scale="column",
                         margins=c(13,8))

# The end of the fifth day


























