### code for Biol 402 lab
### loading data, visualizing density treatments
### this code file is your assignment. Turn this in with the graphs as directed below. There are two problems in this assignment. 


# Preparation and set up --------------------------------------------------


## packages
library(tidyverse)
library(ggplot2)

## load data
#Read in data ----
density <- read.csv(file = "./density_class.csv")

#look at the data
View(density)

#Let's think about the experiment. We aimed to have four density treatments. What did those end up looking like?

# find the column in the data that gives the treatments. 

# use GGPLOT2 to plot the data


# Problem 1: Visualize the treatment densities.  --------------------------


## 1A: What are the treatment densities at the start of the experiment?  These would be the last measurements made on September 6th. This code will produce a graph of the treatment densities. Replace the 'XXXXX' and 'YYYYYY' with the correct column names from the datafile. 

treatments_initial <- ggplot(density, aes(x = XXXXXXX, y = YYYYYY)) +
  ylim(0, 175) +
  geom_boxplot() +
  geom_point() +
  xlab("Density Treatment") +
  ylab("Full plot density") +
  ggtitle("Treatments Sept 6")
treatments_initial

ggsave("Sept_6_Densities.jpg", treatments_initial)

## 1B. What were the final densities? Visualize the densities from Sept 16th by adapting the code from above. Cut and paste it below, and change the y-axis data and the figure title. 










## 2. Adapting the code above, graph a) macroalgae and b) detritus in the treatments on Sept 16th. Be sure to change the y-axis and figure titles. 

# a) macroalgae graph



# b) detritus graph



