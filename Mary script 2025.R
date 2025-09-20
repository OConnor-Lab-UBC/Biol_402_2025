## Biol 402 data 2025
## mary's script working things out

## packages
library(tidyverse)
library(nlme)
library(MuMIn)
library(car)
library(ggplot2)
library(effects)


#Read in data ----
density <- read.csv(file = "./density_2025.csv")

density <- density %>%
  unite("Trt.comb", c(Trt_density, Treament)) %>%
  mutate(density_diff = sept_16_density - sept_6_densityF)

View(density)
names(density)

write.csv(density, "density_class.csv")

#Visualization


# does subsampling sufficiently approximate the full plot? These is using the quadrat and full data from Sept 6th. 
subsampling <- ggplot(density, aes(x = Sept.6.quadrat, y = Sept.6.density)) +
  geom_point() +
  xlab("Subsample count (1 quadrant)") +
  ylab("Full plot count") +
  ggtitle("Does subsampling work?")
  #geom_abline(intercept = 0, slope = 1)
subsampling

## what are the treatment densities - sept 6th?
treatments_initial <- ggplot(density, aes(x = Trt.comb, y = sept_6_densityF)) +
  ylim(0, 175) +
  geom_boxplot() +
  geom_point() +
  xlab("Density Treatment") +
  ylab("Full plot density") +
  ggtitle("Treatments Sept 6")
treatments_initial

treatments_final <- ggplot(density, aes(x = Trt.comb, y = sept_16_density)) +
  ylim(0, 175) +
  geom_boxplot() +
  geom_point() +
  xlab("Density Treatment") +
  ylab("Full plot density") +
  ggtitle("Treatments Sept 16")
treatments_final

density_change <- ggplot(density, aes(x = sept_6_densityF, y = sept_16_density)) +
  geom_point() +
  xlab("Density start") +
  ylab("Density end") +
  ggtitle("Density change") +
  geom_abline(intercept = 0, slope = 1)
density_change

detritus <- ggplot(density, aes(x = Trt.comb, y = sept_16_detritus)) +
  geom_point() +
  xlab("Density end") +
  ylab("Detritus end") +
  ggtitle("Detritus")
detritus


macroalgae <- ggplot(density, aes(x = Trt.comb, y = sept_16_macro)) +
  geom_point() +
  xlab("Treatment") +
  ylab("Macro") +
  ggtitle("Macro")
macroalgae

density_diff <- ggplot(density, aes(x = Trt.comb, y = density_diff)) +
  geom_point() +
  xlab("Treatment") +
  ylab("density diff") +
  ggtitle("Density Diff")
density_diff

density_diff2 <- ggplot(density, aes(x = sept_5_density1, y = density_diff)) +
  geom_point() +
  xlab("Sept_5") +
  ylab("density diff") +
  ggtitle("Density Diff")
density_diff2

density_diff2 <- ggplot(density, aes(x = sept_6_density1, y = density_diff)) +
  geom_point() +
  xlab("Sept_6") +
  ylab("density diff") +
  ggtitle("Density Diff")  +
  geom_abline(intercept = 0, slope = 0)
density_diff2

density_diff3 <- ggplot(density, aes(x = sept_16_density, y = density_diff)) +
  geom_point() +
  xlab("Sept_16") +
  ylab("density diff") +
  ggtitle("Density Diff")  +
  geom_abline(intercept = 0, slope = 0)
density_diff3
