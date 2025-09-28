### code for Biol 402 lab
### loading data, visualizing density treatments
### this code file is your assignment. Turn this in with the graphs as directed below. There are two problems in this assignment. 


# Preparation and set up --------------------------------------------------


## packages
library(tidyverse)
library(ggplot2)

## load data
#Read in data ----
growth <- read.csv(file = "./growth_class.csv")

#View data
View(growth)
names(growth)


# Part 1. Shoot morphology ------------------------------------------------

# Problem 1. Describe the shoot size data: 

shoot_length <- ggplot(growth, aes(x = shoot_length)) +
  geom_histogram(binwidth=1) +
  geom_vline(aes(xintercept = mean(shoot_length))) +
  geom_vline(aes(xintercept = mean(shoot_length) - sd(shoot_length)), linetype = "dashed") +
  geom_vline(aes(xintercept = mean(shoot_length) + sd(shoot_length)), linetype = "dashed") +
  xlab("Shoot Length") +
  ylab("Frequency")
shoot_length

ggsave("Shoot Length.jpg", shoot_length)

# to get the values: 
growth %>% 
  summarise(mean_shoot_length = mean(shoot_length, na.rm = TRUE))  

growth %>%
  summarise(sd_shoot_length = sd(shoot_length, na.rm = TRUE))

#repeat the three commands above for sheath length, number of leaves and shoot width. Provide these histograms and the mean and sd for each variable in a table. 

# 1C. to plot a regression, you can use a simple linear model and a summary: 

mod1 <- lm(shoot_length ~ sheath_length, data = growth)
summary(mod1) # gives you the regression analysis

Plot1 <- ggplot(growth, aes(x = sheath_length, y = shoot_length)) +
  geom_point() +
  xlab("Sheath length") +
  ylab("Shoot length") +
  ggtitle("Shoot length and Sheath Length") +
  geom_abline(intercept = 67.5581, slope = 0.8564) # you can put the results of the regression model in here for XX and YY to plot the line on the graph. 

Plot1

ggsave("Sheath length by shoot length.jpg", Plot1)

# Report the statistical results in your assignment.  



# Part 2. Shoot Growth ----------------------------------------------------

# calculate shoot growth using three metrics. The first, total leaf extension (Total_extension) is already calculated in the dataset. Use that and the equations from lab to calculate relative growth rate (RGR) and plastochrone interval here. Use these results to answer the questions in the assignment. 

Tot_ext <- ggplot(growth, aes(x = Total_extension)) +
  geom_histogram(binwidth=2) +
  geom_vline(aes(xintercept = mean(Total_extension))) +
  geom_vline(aes(xintercept = mean(Total_extension) - sd(Total_extension)), linetype = "dashed") +
  geom_vline(aes(xintercept = mean(Total_extension) + sd(Total_extension)), linetype = "dashed") +
  xlab("Total exentsion") +
  ylab("Frequency")
Tot_ext

growth %>% 
  summarise(mean_LE = mean(Total_extension, na.rm = TRUE))  

growth %>%
  summarise(sd_LE = sd(Total_extension, na.rm = TRUE))

# plot total extension as a function of morphology by modifying the code above. 

## Relative Growth Rate. Add a column to the data that is relative growth rate. To do this you have to look up the formula from lab 2 and type that in here. Replace the words 'TYPE THE FORMULA FOR RGR HERE' below with the formula that references the correct column names in the datafile. 

growth <- growth %>%
  mutate(RGR = TYPE THE FORMULA FOR RGR HERE)

#check that the column is there and looks good. 
View(growth)

# estimate mean and sd for RGR using code above.

## are there any correlates between RGR and shoot morphology? use the graphing and regression code above to test for these. 


## daily growth rate:

growth <- growth %>%
  mutate(RGRd = TYPE THE FORMULA FOR DAILY RGR HERE)

## Plastochrone Interval

growth <- growth %>%
  mutate(PI = TYPE THE FORMULA FOR PLASTOCHRONE INTERVAL HERE)

View(growth)



# Part 3: Density ---------------------------------------------------------
# you don't have the treatment codes  here yet, but you do have the density values. Does growth vary by density? 

