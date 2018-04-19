# Day_4.R
# The fourth day of the class 
# Purpose: ANOVA
# 19 April 2018 

# NOTES -------------------------------------------------------------------
# More repeated t-tests - increases probability of type-1 error


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(ggpubr)

# First grab the data
chicks <- as_tibble(ChickWeight)

# Then subset out only the sample sets to be compared
chicks_sub <- chicks %>% 
  filter(Diet %in% c(1, 2), Time == 21) # Pull out diet 1 and 2 from big data

# Are the diets resuting in different chicken sizes 
# t-test
t.test(weight ~ Diet, data = chicks_sub)

# We do not reject null hypothesis (accept)

# Is there a significant difference in the means of the four diets 

# 1-way ANOVA -------------------------------------------------------------
#  Research question: Is there a difference in the chicken mass at 
# day 21 as a result of them eating 4 difference diets

# H0: There is no difference in the chicken mass after 21 days as 
# a result of being fed 1 of 4 different diets 
# H1: There is a difference in the chicken mass after 21 days as 
# a result of being fed 1 of 4 different diets

# Filter out data 
chicks_21 <- chicks %>%
  filter(Time == 21)

chicks.aov1 <- aov(weight ~ Diet, data = chicks_21)
summary(chicks.aov1)

# Pr- degrees of table in f- table- probability less than 0.05
# We reject the null hypothsis - accept alternative hypothesis 
# There is a difference
# Which ones are different- 4 means compared with each other 
# Where is the difference? 

# Notched box plot
ggplot(data = chicks.aov1, aes(x = Diet, y = weight)) +
  geom_boxplot(aes(fill = Diet), notch = TRUE) 

# Notches don't overlap - which shows that they are different 
# Diet 1 and 4 don't overlap , 1 and 3 don't overlap - there is a difference
# Diet 2 and 3 there is no difference - notches overlap 
# Little triangles - box is wider than 3rd quartile = so that distances are the same 
# Width twice the times of IQR 

# Tukey HSD test -------------------------------------------------------------------
# p adj = significant difference or not
# lwr is positive - significant difference 
TukeyHSD(chicks.aov1)


# Visuals  ----------------------------------------------------------------

chicks_21 <- ChickWeight %>% 
  filter(Time == 21)

# Box plot 
ggplot(data = chicks_21, aes(x = Diet, y = weight, fill = Diet)) +
  geom_boxplot(notch = TRUE, colour = "grey50") +
  geom_segment(aes(x = Diet, xend = Diet, y = weight, yend = weight+2))

# Segements showing confidence intervals 
# Dataframe of segments 
chicks_Tukey <- as.data.frame(TukeyHSD(aov(weight ~ Diet, data = chicks_21))$Diet)
chicks_Tukey$pairs <- as.factor(row.names(chicks_Tukey))

# Significant if it doesn't cross 0 line 

ggplot(data = chicks_Tukey, aes(x = diff, y = pairs)) +
  geom_segment(aes(x = lwr, xend = upr, y = pairs, yend = pairs)) +
  geom_abline(mapping = NULL, data = NULL, slope = 90, intercept = 0,linetype = "dotted")

# Or just plot confidence intervals the base R way...
# shame 
plot(TukeyHSD(aov(weight ~ Diet, data = chicks_21)))

# Multiple factor ANOVA ---------------------------------------------------
# Ho: There is no change in chicken mass (kg) from day 0 to day 21.

chicks_0_21 <- ChickWeight %>% 
  filter(Time  %in% c(0, 2, 21))

ggplot(data= chicks_0_21, aes(x = Time, y = weight)) +
  geom_boxplot(notch = T, aes(fill = as.factor(Time)))

# Run an ANOVA
summary(aov(weight ~ as.factor(Time), data = chicks_0_21))

# Perform a Tukey post-hoc test
TukeyHSD(aov(weight ~ as.factor(Time), data = chicks_0_21))

# Look at the confidence intervals
plot(TukeyHSD((aov(weight ~ as.factor(Time), data = chicks_0_21))))

# Running repeated ANOVAS - increases errors just like repeated t-tests 
# Do the chickens weigh more at day 21 than day 0 (main effect-weight as a function of diet ) 
summary(aov(weight ~ Diet + as.factor(Time), data = filter(chicks, Time %in% c(0, 21))))  
  
# Or simply look at ALL of the Time 
# ...which is NOT the hypothesis 
summary(aov(weight ~ Diet + as.factor(Time), data = ChickWeight))
# Note the increase in the degrees of freedom for the time factor 
# But no increase for the d.f. for Diet 

# Now to look at interactions BETWEEN factors 
summary(aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21))))

# Lets's look at the Tukey results 
TukeyHSD(aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21))))
plot(TukeyHSD(aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21)))))

# Create a line graph to help explain this concept
# First create mean values by Time and Diet 
chicks_mean <- ChickWeight %>% 
  group_by(Diet, Time) %>% 
  summarise(weight_mean = mean(weight, na.rm = T))

ggplot(data = chicks_mean, aes(x = Time, y = weight_mean, colour = Diet)) +
  geom_line(size = 2) +
  geom_point(shape = 15, size = 5)

# Non-parametric tests ----------------------------------------------------
# But what if...
# ...we don't have normal data?
# For a t-test we rather use Wilcox rank sum test
wilcox.test() # And then fills this in the same as for t-test()

# And now for the Kruskall-Wallis ,non - parametic , three 
kruskal.test(weight ~ Diet, data = chicks_0_21)
library(pgirmess)
kruskalmc(weight ~ Diet, data = chicks_0_21)


# Bonus question -------------------------------------------------------------------

# Exercise 1 

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
# therefore, feed type does effect the mass of pigs 


# The end of the fourth day updated  --------------------------------------

















  
  



























