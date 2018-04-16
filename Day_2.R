# Day_2.R
# The second day of the class 
# Purpose: To practise some concepts we will encounter 
# 13 April 2018 


# Load libraries  ---------------------------------------------------------

library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)


# Manual calculations -----------------------------------------------------

# Generate some random data 
r_dat <- data.frame(dat = rnorm(n = 600, mean = 372, sd = 50),
                    sample = "A") # Generate random normal data , number of arguments


# Quick visualisation -----------------------------------------------------
ggplot(data = r_dat, aes(x = dat)) +
  geom_density() # Create data frame first 

# The mean 
# The sum of all the points 
# divided by 
# the number of all the points 
r_dat %>% 
  summarise(r_sum = sum(dat),
            r_n = n(),
            r_mean = r_sum/r_n,
            r_mean_func = mean(dat)) # Pulls out little things out of big data(summarise function)

# The median 
# Brute force with base R
order(r_dat$dat)[length(r_dat$dat)/2]
# or the tidy 
r_dat %>% 
  arrange(dat) %>% 
  slice(n()/2)

# or the tidy automagic way
r_dat %>% 
  summarise(r_median = median(dat)) # Two different values - values not ordered 
  
# Variance 
# The sum of
 # each value 
  #minus 
   #the mean,
    # squared 
# divided by 
 # the count of sample minus one 

r_dat %>% 
  mutate(r_error = dat-mean(dat), # Use mutate function to add column
         r_error_square = r_error * r_error) %>% 
  summarise(r_squared_sum = sum(r_error_square),
            r_var = r_squared_sum/(n()-1))
            # OR use the built in function 
            r_var_func = var(dat))
  
            

# The standard deviation 
 r_dat %>% 
   summarise(r_var = var(dat),
             r_sd = sqrt(r_var),
             r_sd_func = sd(dat))

# Calculate the kurtosis and skewness 
# Load library
library(e1071)

kurtosis(r_dat$dat) # Fat-tailed distribution 
skewness(r_dat$dat) # Mean less than median - left-skewed 
              
# Use summarise() approach and construct a data summary with exactly the same
# summary statistics
# for weight as that which summary() returns

summary(ChickWeight$Weight)

ChickWeight %>%
  summarise(min_weight = min(weight),
            quart_1 = quantile(weight, 0.25),
            med_weight = median(weight),
            mean_weight= mean(weight),
            quart_3 = quantile(weight, 0.75),
            max_weight = max(weight))

# Visualisations -----------------------------------------------------------
# Load additional libraries 
# These are a few packages that contain most necessary functions
# to make publication ready figures 
library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(ggplot2)
library(viridisLite)


# Qualitative -------------------------------------------------------------

# Load our SA time data
sa_time <- read.csv("SA_time.csv")

# Edit our data 
sa_time <- sa_time %>% 
  mutate(human = seq(1, n(), 1),
         geo = c(rep(c("Cape Town","George","PE"), times = 6),
                 rep("Joburg", 2)))

sa_long <- sa_time %>% 
  gather(key = "time_type", value = "minutes", -human)


# Create a count of qualitative values 
sa_count <- sa_long %>%
  count(time_type) %>% 
  mutate(prop = n/sum(n))

# Stacked bar graphs
ggplot(data = sa_count, aes(x = "", y = n, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  labs(title = "Stacked bar graph", subtitle = "cumulative sum",
       x = NULL, y = "Count") +
  theme_minimal()

# Stacked proportion bar graph 
ggplot(data = sa_count, aes(x = "", y = prop, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  scale_y_continuous(breaks = c(0.00, 0.33, 0.66, 1.00)) +
  labs(title = "Stacked bar graph", subtitle = "relative proportions",
       x = NULL, y = "Proportion") +
  theme_minimal()

# Side-by-side bars  
ggplot(data = sa_count, aes(x = time_type, fill = time_type)) +
  geom_bar(show.legend = FALSE) +
  labs(title = "Side-by-side bars", subtitle = "n per species", y = "Count") +
  theme_minimal()

# Pie chart 
ggplot(data = sa_count, aes(x = "", y = prop, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
   labs(title = "Pie chart", subtitle = "but why though...",
       x = NULL, y = NULL) +
  coord_polar("y", start = 0) +
  theme_minimal()


# Continuous data ---------------------------------------------------------

# Histograms 
ggplot(data = sa_long, aes(x = minutes)) +
  geom_histogram()

# oh no !
# Let's get rid of that one value...(n/A value)

sa_clean <- sa_long %>% 
  filter(minutes < 300)


# A faceted histogram 
ggplot(data = sa_clean, aes(x = minutes)) +
  geom_histogram(aes(y= fill = time_type), position = "dodge") +
  facet_wrap(~time_type, ncol = 1, scales = "free_x")

# Relative proportion histogram 
ggplot(data = sa_clean, aes(x = minutes)) +
  geom_histogram(aes(y= ..density.. ,fill = time_type),
                 position = "dodge", binwidth = 1) +
  facet_wrap(~time_type, ncol = 1, scales = "free_x")

# Boxplots 
 ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type)) 
 
# Interquartile range - measures of central tendency of your data 
# Higher IQR - More variability in the data 
# Central black line - median 
# Dot- Outlier
# Boxplot can show distribution of data 
 
# Notched boxplot
 ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
   geom_boxplot(aes(fill = time_type), notch = TRUE) 
 
# If notches ovelap - no statistical difference - not significant(Visually seen)  
 
# Calculate summary stats for plotting over the boxplots
 sa_summary_stats <- sa_clean %>% 
   group_by(time_type) %>% 
   summarise(time_type_mean = mean(minutes))

# plot these means over the boxplots
 ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
   geom_boxplot(aes(fill = time_type), notch = TRUE) +
   geom_point(data = sa_summary_stats, size = 6, shape = 18,
              aes(y = time_type_mean, colour = "goldenrod"))


# Relationships -----------------------------------------------------------

# A basic scatterplot

ggplot(data = sa_time, aes(y = now_now, x = just_now)) +
   geom_point() +
   coord_equal(xlim = c(0, 60), ylim = c(0, 60))
 
# Adding trend lines 
 ggplot(data = sa_time, aes(y = now_now, x = just_now)) +
   geom_point(aes(colour = geo))+
   geom_smooth(aes(colour = geo), method = "lm") +
   coord_equal(xlim = c(0, 60), ylim = c(0, 60))
 
 
# End of day 2 with changes

































            
            
            















































