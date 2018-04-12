# Day1.R
# The first day of the stats class 
# Purpose: to practise some of the concepts that we will encounter 
# 12 April 2018 

# 

# Load libraries  ---------------------------------------------------------

library(tidyverse)
library(ggplot2)

# Integers  ---------------------------------------------------------------

# Generate some integer data - describes distinct things - must be the same length 
integer_r <- as.integer(seq(5, 14, by = 1))

# Look at summary of them 
summary(integer_r)


# Continuous  -------------------------------------------------------------

# Generate a sequence of numeric values 
numeric_r <- seq(23, 43, length.out = 10) #length.out short function 


# Dates - distinct from other data  ---------------------------------------


# One may perform some arithmetic with dates 
seq(as.Date("2005-12-31") - as.Date("2005-12-12"))
# or for example 
dates_r <- seq(as.Date("2005-12-16"), as.Date("2005-12-25"), by = "day")
# There is much more 
summary(dates_r)


# Dataframes  -------------------------------------------------------------

df_r <- data.frame(integers = integer_r,
                   numeric = numeric_r,
                   dates = dates_r)

 #Then upgrage it to a tibble 
df_r <- as.tibble(df_r)
summary(df_r)


# Categories  -------------------------------------------------------
# as.factor: encodes a vector as a factor 
# Electronics 
elec_r <- as.factor(c("laptop",
                      "desktops",
                      "cell phones"))

# People 
people_r <- as.factor(c("funnyhair",
                        "beautiful",
                        "beanies"))

# Colours 
colour_r <- as.factor(c("red", "blue"))


# Ordinal data ------------------------------------------------------------
# Factored data 
# Here we still have qualitative data 
# but with some sort of order 

colour_qual <- ordered(c("blue", "green",
                         "yellow", "orange",
                         "red") ,
                         levels = c("blue", "green",
                                    "yellow", "orange",
                                    "red"))


# Binary ------------------------------------------------------------------
# take on one or two measurements 
# These are generally represented as: TRUE or FALSE 
binary_r <- c(TRUE, FALSE, TRUE, TRUE)
summary(binary_r)


# Characters  -------------------------------------------------------------

sites_r <- c("Yztervarkpunt", "Betty's Bay",
             "Gaansbaai", "Sea Point")



# Missing value  ----------------------------------------------------------
# umber of eggs recorded in a nest
# The NA shows a nest that was not able to be sampled 
chicks_nest <- c(3, 2, 0, 10, 5, 6, 8, 2, 4, NA)
# Summary
summary(chicks_nest)
# The mean 
mean(chicks_nest)
#The standard deviation
sd(chicks_nest)

summary(ChickWeight)
chicks <- ChickWeight
head(chicks,7) #top 7 rows
tail(chicks, 15) #bottom 15 rows  

ChickWeight[c(1,54,61,12),2] 


# Descriptive statistics  -------------------------------------------------

# First create a dataframe 
chicks <- as_tibble(ChickWeight)

# Count the data
chicks %>% 
  summarise(chicken_count = n())
# or
nrow(chicks)


# Measures of central tendency  -------------------------------------------

# Calculate mean weight 
chicks %>%
summarise(mean_wt = mean(weight))

# Be more specific
chicks  %>%
filter(Time == 21) %>% 
group_by(Diet) %>% 
  summarise(mean_wt = mean(weight),
            median_wt = median(weight)) # If mean and median similar - evenly distributed data 

# Visualise the density of the data 
ggplot(data = filter(chicks, Time == 21), aes(x = weight, fill = Diet)) +
  geom_density(alpha = 0.4) 

# Right skewed data - mean skewed to the right of median  


# Skewness ----------------------------------------------------------------

# Calculate the numeric value 
# First load libraries 
library(e1071)

# Compare difference in mean and median against skeweness
chicks %>%
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mean_wt = mean(weight),
            median_wt = median(weight),
            skew_wt = skewness(weight))



# Kurtosis ----------------------------------------------------------------

# Calculate the kurtosis of the tails of a districution
chicks %>%
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mean_wt = mean(weight),
            median_wt = median(weight),
            skew_wt = skewness(weight))

exp_r <- data.frame(dat = rexp(n = 500),
                    sample = "A")

ggplot(data = exp_r, aes(x = dat))+
  geom_density()

kurtosis(exp_r$dat)





# Measures of variability  ---------------------------------------------------------------
# Below is a summary of many different statistical properties
wt_summary <- chicks %>%
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(wt_mean = mean(weight),
            wt_median = median(weight),
            wt_var = var(weight),
            wt_sd = sd(weight),
            wt_min = min(weight),
            wt_quart1 = quantile(weight, 0.25),
            wt_quart2 = quantile(weight, 0.5),
            wt_quart3 = quantile(weight, 0.75))

# end of day 1

































