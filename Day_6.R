# Day_6.R
# The sixth day of the class 
# Purpose:Confidence Intervals and  Transformations 
# 26 April 2018 

# Notes 
# Range of estimates of the mean that represent 95% 
# of the means that we have calculated 
library(rcompanion)
library(tidyverse)
library(ggplot2)
library(ggpubr)

Input <- ("
Student  Sex     Teacher  Steps  Rating
          a        female  Jacob    8000   7
          b        female  Jacob    9000  10
          c        female  Jacob   10000   9
          d        female  Jacob    7000   5
          e        female  Jacob    6000   4
          f        female  Jacob    8000   8
          g        male    Jacob    7000   6
          h        male    Jacob    5000   5
          i        male    Jacob    9000  10
          j        male    Jacob    7000   8
          k        female  Sadam    8000   7
          l        female  Sadam    9000   8
          m        female  Sadam    9000   8
          n        female  Sadam    8000   9
          o        male    Sadam    6000   5
          p        male    Sadam    8000   9
          q        male    Sadam    7000   6
          r        female  Donald   10000  10
          s        female  Donald    9000  10
          t        female  Donald    8000   8
          u        female  Donald    8000   7
          v        female  Donald    6000   7
          w        male    Donald    6000   8
          x        male    Donald    8000  10
          y        male    Donald    7000   7
          z        male    Donald    7000   7
          ")

data <- read.table(textConnection(Input),header = TRUE)
summary(data)

library(rcompanion)
# ungrouped data is indicated with a 1 on the right side of the formula, or the group = NULL argument.
groupwiseMean(Steps ~ 1,data = data, conf = 0.95, digits = 3)

# one-way data
groupwiseMean(Steps ~ Sex, data = data, conf = 0.95,digits = 3)

# two-way data
dat1 <- groupwiseMean(Steps ~ Teacher + Sex, data = data, conf = 0.95,digits = 3)

# Plot mean as a point and around that the confidence intervals
# The effect of teacher and sex on mean and CI
library(rcompanion)
dat1 <- groupwiseMean(Steps ~ Teacher + Sex, data = data, conf = 0.95,digits = 3)

# Create the graph --------------------------------------------------------
library(ggplot2)
ggplot(data = dat1, aes(y = Mean, x = Sex)) +
  geom_point(aes(colour = Teacher)) +
  geom_errorbar(aes(ymin = Mean - Trad.lower,
                    ymax = Mean + Trad.upper,
                    colour = Teacher)) +
  facet_wrap(~Teacher)

# by bootstrapping
groupwiseMean(Steps ~ Sex,
              data = data,
              conf = 0.95,
              digits = 3,
              R = 10000,
              boot = TRUE,
              traditional = FALSE,
              normal = FALSE,
              basic = FALSE,
              percentile = FALSE,
              bca = TRUE)


# Testing assumptions ------------------------------------------------------
# IID- Independent identical distribution

# Null hypothesis : There is no difference 

# Log transform data in Teacher data 
# Create a column for log tranformed data
# Natural log 
# Cube root 
# Histogram for each column 
log_data <- data %>%
  mutate(log10 = log10(Steps)) %>% 
  mutate(log = log(Steps)) %>%
  mutate(cuberoot = (Steps)) %>% 
  mutate(sqrt = sqrt(Steps))

# Plot histogram of all of the columns ------------------------------------

plot1 <- ggplot(data = log_data, aes(x = log10, fill = Teacher))+
  geom_histogram(aes(fill = Teacher), position = "dodge")

plot2 <- ggplot(data = log_data, aes(x = log, fill = Teacher))+
  geom_histogram(aes(fill = Teacher), position = "dodge")

plot3 <- ggplot(data = log_data, aes(x = cuberoot, fill = Teacher))+
  geom_histogram(aes(fill = Teacher), position = "dodge")

plot4 <- ggplot(data = log_data, aes(x = sqrt, fill = Teacher))+
  geom_histogram(aes(fill = Teacher), position = "dodge")

final <- ggarrange(plot1, plot2, plot3, plot4)
final

# OR
#dat2<-  data %>% 
#mutate(ln.step = log(Steps),
       #log10.step = log10(Steps),
       #cube.step = Steps^(1/3),
       #sqrt.step = (Steps)) %>% 
  #select(-Student,-Rating)%>% 
  #gather(key = "data.type",value = "trans.data",
         ##-Sex, -Teacher) %>% 
  #mutate(data.type = as.factor(data.type))

#head(dat2)
#tail(dat2)

# Create histogram for all of these values 
#plot1 <- ggplot(data = filter(dat2, data.type == "cube.step"), aes(X = trans.data))+
  #geom_histogram(aes(fill = Sex), position = "dodge")
         
#plot2 <- ggplot(data = filter(dat2, data.type == "ln.step"), aes(X = trans.data))+
  #geom_histogram(aes(fill = Sex), position = "dodge")        
       
#plot3 <- ggplot(data = filter(dat2, data.type == "log10.step"), aes(X = trans.data))+
  #geom_histogram(aes(fill = Sex), position = "dodge")     
       
#plot4 <- ggplot(data = filter(dat2, data.type == "sqrt.step"), aes(X = trans.data))+
  #geom_histogram(aes(fill = Sex), position = "dodge") 

#Combined_histograms <- ggarrange(plot1, plot2, plot3, plot4)
#Combined_histograms

# Revision ----------------------------------------------------------------
# A dataset to see if we can do an analysis of variance on 
iris.dat <- as.tibble(iris)

# Ho: There is not a significant difference in Petal.Length 
# between the three iris species 
shapiro.test(iris$Petal.Width) # Shows whether data are normal 
# If the p-value is < 0.05 then data is considered non-Normal
# If the p-value is > 0.05 then data is considered Normal

iris %>% 
  group_by(Species) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(Petal.Width)[2])) # Determine for each set of species if data is normal 

# We find that some of the data are non-normal 

# Do a Kruskal-Wallis test instead of an ANOVA 
kruskal.test(Petal.Width ~ Species, data = iris)

# Do transformation exercises 
# Do confidence intervals exercises
# Exercises will be sent 
# Test - choose to answer only certain questions 
# OR
# Assignment in Rmarkdown 

# Exercises 11.3 ---------------------------------------------------------------

# Exercise 1 
chicks <- as_tibble(ChickWeight)

# Whole data set 
shapiro.test(chicks$weight) # Not normally distributed 

# Visualise the distribution 
ggplot(data = chicks, aes(x = weight)) +
  geom_density(aes(fill = Diet)) +
  labs( title = "Density plot  of distribution")

# Filter the data based on the different diets for only the 
# weights taken on day 2
# Shapiro.test
chicks_2 <- chicks %>% 
  filter(Time == 2) %>% 
  group_by(Diet) %>% 
  summarise(norm_wt = as.numeric(shapiro.test(weight)[2]))

# Check Homoscedasticity
chicks_2 <- chicks %>% 
  filter(Time == 2) %>% 
  group_by(Diet) %>% 
  summarise(var_wt = var(weight))

# ChickWeight data does not pass the assumptions of normality on Day 2 

# Filter the data based on the different diets for only the 
# weights taken on day 20
# Shapiro.test
chicks_20 <- chicks %>% 
  filter(Time == 20) %>% 
  group_by(Diet) %>% 
  summarise(norm_wt = as.numeric(shapiro.test(weight)[2]))

# Check Homoscedasticity
chicks_20 <- chicks %>% 
  filter(Time == 20) %>% 
  group_by(Diet) %>% 
  summarise(var_wt = var(weight))

# ChickWeight data does pass the assumptions of normality on Day 20 

# Exercise 2 
# Log-transform data to make it normal on Day 2 
log_chicks_2 <- chicks %>%
  mutate(log10 = log10(weight)) %>% 
  mutate(log = log(weight))

# Shapiro.test  
log_chicks_2 %>%
  filter(Time == 2) %>% 
  group_by(Diet) %>% 
  summarise(norm_wt = as.numeric(shapiro.test(weight)[2]))

# Check Homoscedasticity
log_chicks_2 %>% 
  filter(Time == 2) %>% 
  group_by(Diet) %>% 
  summarise(var_wt = var(weight))

# ? data still does not pass assumptions after log transformation for day 2 

# The end of the sixth day 


































