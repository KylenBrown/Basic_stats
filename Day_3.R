# Day_3.R
# The third day of the class 
# Purpose: Generate a Cullen and Frey graph and do t-tests on data 
# 17 April 2018 

# Load libraries ----------------------------------------------------------
library(fitdistrplus)
library(logspline)
library(tidyverse)
library(ggpubr)

# Generate log-normal data
r_norm <- rnorm(n = 1000, mean = 13, sd = 1)

hist(r_norm)
descdist(r_norm, discrete = FALSE, boot = 100)

# uniformly distributed data
y <- runif(100)
par(mfrow = c(1, 1))
plot(x = c(1:100), y = y)
hist(y)
descdist(y, discrete = FALSE)

# Load additional libraries----------------------------------------------------------
library(tidyverse)

# t-tests -----------------------------------------------------------------
# comparing two things 
# ANOVA - comparing more than two things 

# Load data 
r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3),
                            rnorm(n = 1000, mean = 8, sd = 2)),
                    sample = c(rep("A", 1000), rep("B", 1000)))

# Check assumptions -------------------------------------------------------
# NOrmality
# For this we may use the Shapiro-Wilk test 
# value less 0.05 - data not normally distributed 
shapiro.test(r_dat$dat)

# But that is testing all of the data together
# We must be a bit more cever about how we make this test 
r_dat %>% 
  group_by(sample) %>% 
  summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2]))
# Remember, the data are normal when p > 0.05 
# The data are non-normal when p <= 0.05

# Check homoscedasticity ----------------------------------------------------------

# There are many ways to check for homoscedasticity
# Which is the similarity of variance between sample sets
# for now we will simply say that this assumption is met when 
# the variance of the samples are not more than 2 - 4 times greater 
# than one another 

# Check variance all at once...
# WRONG 
var(r_dat$dat)

# or do it the tidy way 
r_dat %>% 
  group_by(sample) %>% 
  summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2]),
            r_norm_var = var(dat))


# A one sample t-test -----------------------------------------------------
r_one <- data.frame(dat = rnorm(n = 20, mean = 20, sd = 5),
                    sample = "A")

# Test normality of distributon 
shapiro.test(r_one$dat)

# Visulaise this using a histogram or density plot

# Density plot 
ggplot(data = r_one, aes(x = dat)) +
  geom_density(aes(fill = sample)) +
  labs( title = "Density plot showing normality of distribution")
                 
# Run the test
t.test(r_one$dat, mu = 20) # mean population known as mu- one sample t-test

# Run a test we know will produce a significant result 
t.test(r_one$dat, mu = 30)

# Pick a side  ------------------------------------------------------------

# Are these data SMALLER/LESS than the population mean
t.test(r_one$dat, mu = 20, alternative = "less")
# or GREATER 
t.test(r_one$dat, mu = 20, alternative = "greater")

# But what about for the larger population mean?
# Are the samples less than the population of 30?
t.test(r_one$dat, mu = 30, alternative = "less")
# What about greater than?
t.test(r_one$dat, mu = 30, alternative = "greater")

# Two sample t-tests -------------------------------------------------------

# Create another dataframe
r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1),
                            rnorm(n = 20, mean = 5, sd = 1)),
                    sample = c(rep("A", 20), rep("B", 20)))

# Run a default/basic test 
t.test(dat ~ sample, data = r_two, var.equal = TRUE)

# Pick a side 
t.test(dat ~ sample ,data = r_two, var.equal = TRUE, alternative = "less")
# Is A greater than B?
t.test(dat ~ sample ,data = r_two, var.equal = TRUE, alternative = "greater")

# Run exercises # Run different variables compared to the ones run # make graphs 
# Start from the workflows 
# Go to 6.6 - do an analysis using one kelp variable , produce graphs 
# Do a t-test using another variable than the one in the book 
# Do exercise 1 

# Bonus exercises

# A t-test workflow  ------------------------------------------------------

# Load data 
ecklonia <- read_csv("ecklonia.csv") %>% 
  gather(key = "variable", value = "value", -species, -site, -ID)

# Visualising the data in order to form a hypothesis  ---------------------

ggplot(data = ecklonia, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = " Morphometric properties of Ecklonia maxima", x= "Morphological variable" , y = "Value")

# Look at epiphyte length at the different sites - pull out data 
# filter the data
ecklonia_sub <- ecklonia %>% 
  filter(variable == "epiphyte_length")

# create a new box plot
ggplot(data = ecklonia_sub, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "Epiphyte length (cm)", x = "") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# HYPOTHESIS : “Are the epiphyte lengths at Batsata Rock 
#               greater than at Boulders Beach?”
# HO:epiphyte length not greater p > 0.05
# H1:epiphyte length greater p < 0.05

# Choosing a test  --------------------------------------------------------

# T-test - comparing two variables 
# One-sided t-test - want to see if epiphyte length is greater at one site 

# Meeting assumptions of a t-test -----------------------------------------

# Normality
ecklonia_sub %>% 
  group_by(site) %>% 
  summarise(epiphyte_mass_var = as.numeric(shapiro.test(value)[1]))

# Homoscedasticity
ecklonia_sub %>% 
  group_by(site) %>% 
  summarise(epiphyte_mass_var = as.numeric(shapiro.test(value)[1]),
            epiphyte_mass_norm = as.numeric(shapiro.test(value)[2]))

# Traditional output 
t.test(value ~ site, data = ecklonia_sub, var.equal = TRUE, alternative = "greater")

# Dataframe output
compare_means(value ~ site, data = ecklonia_sub, method = "t.test", var.equal = TRUE, alternative = "greater")

# Reject the null hypothesis that the epiphyte length of kelps 
# at Batsata Rock are not greater than at Boulders Beach
# if our t-test returns a p-value ≤ 0.05

# Concluding sentence -----------------------------------------------------

# The epiphye length (cm) of the kelp Ecklonia maxima was found 
# to be significantly greater at Batsata Rock than at Boulders Beach 
# (p = 0.003, t = 3,08, df = 24)

# Exercise 1 --------------------------------------------------------------

biomass <- read_csv2("biomass.csv")

ggplot(data = biomass, aes(x = Depth, y = Biomass, fill = Site)) +
  geom_boxplot() +
  coord_flip() +
  labs( title = "Biomass at different depths across two sites")

# Filter the data
biomass_sub <- biomass %>% 
  filter(Depth == "Shallow")

# then create a new boxplot
ggplot(data = biomass_sub, aes(x = Depth, y = Biomass, fill = Site)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "Shallow biomass (g)", x = "") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# HYPOTHESIS : Is the biomass of shallow Isolepis digitata samples
#              greater at site 1 than at site 2 
# HO:shallow biomass not greater p > 0.05
# H1:shallow biomass greater p < 0.05

# Choosing a test  --------------------------------------------------------

# T-test - comparing two variables 
# One-sided t-test - want to see if biomass is greater at one site 

# Meeting assumptions of a t-test -----------------------------------------

# Meeting assumptions of a t-test -----------------------------------------

# Normality
biomass_sub %>% 
  group_by(Site) %>% 
  summarise(Depth_var = as.numeric(shapiro.test(Biomass)[1]))

# Homoscedasticity
biomass_sub %>% 
  group_by(Site) %>% 
  summarise(Depth_var = as.numeric(shapiro.test(Biomass)[1]),
            Depth_norm = as.numeric(shapiro.test(Biomass)[2]))

# Traditional output 
t.test(Biomass ~ Site, data = biomass_sub, var.equal = TRUE, alternative = "greater")

# Dataframe output
compare_means(Biomass ~ Site, data = biomass_sub, method = "t.test", var.equal = TRUE, alternative = "greater")

# Concluding statement  ---------------------------------------------------


# The biomass (g) of shallow Isolepis digitata samples was 
# not found to be significantly greater at site 1 than at site 2 
# (p = 0.22, t = 0.81, df = 8).

# The end of the third session 
