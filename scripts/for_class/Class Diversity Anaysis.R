### code for Biol 402 
### Epifauna diversity analyses
### this is not an assignment, but a reference if you would like to work with the diversity data
## based on: https://rpubs.com/CPEL/NMDS

## packages
library(tidyverse)
library(ggplot2)
library(effects)
library(vegan)

#data
epifauna_shoots <- read.csv(file = "./data/epifauna_shoots.csv")
epifauna <- read.csv(file = "./data/epifauna.csv")
density <- read.csv(file = "./data/density_2025.csv")

### add treatment combined term, and density difference term, rename plot number. 
density1 <- density %>%
  mutate(Trt.comb = paste(Trt_density, Treament, sep = "_")) %>%
  mutate(density_diff = sept_16_density - sept_6_density) %>%
  rename(Pl.no = plot) %>%
  mutate(Pl.no = as.character(Pl.no))

# we want a dataframe with mean shoot metrics so we can estimate LAI. 
epifauna_shoots1 <- epifauna_shoots %>%
  group_by(plot) %>%
  summarize(mean_length = mean(shoot_length, na.rm = TRUE),
            mean_width = mean(shoot_width, na.rm = TRUE),
            mean_leaves = mean(no_leaves, na.rm = TRUE),
            density = mean(number_shoots, na.rm = TRUE)) %>%
  mutate(LAI = (((mean_length*mean_width*mean_leaves) * 2)*density)/625) %>%
  rename(Pl.no = plot) %>%
  mutate(Pl.no = as.character(Pl.no))

#merge plot density and LAI with invert diversity metrics
epifauna_density <- left_join(epifauna_shoots1, density1, by ="Pl.no")

# rename columns and calculate total abundance
epifauna1 <- epifauna %>%
  select(-c("X", "X.1", "X.2", "plot")) %>%
  rowwise() %>%
  mutate(abundance = sum(c_across(Amphipods:mite))) %>%
  ungroup()
# %>%
#rename(Pl.no = plot) %>%
#mutate(Pl.no = as.character(Pl.no))

## code to calculate single metrics and merge back with other datafiles
Pl.no = factor(epifauna$plot)
Abundance = epifauna1$abundance

Shannon = diversity(epifauna1, index = "shannon")

epifauna_div <- data.frame(Pl.no = Pl.no, Shannon = Shannon, Abundance = Abundance)

epifauna2 <- left_join(epifauna_div, density1, by ="Pl.no") 
epifauna3 <- left_join(epifauna2, epifauna_shoots1)

View(epifauna3)


## nmds plots
# start with a species-plot matrix
View(epifauna1)
#groups for the plots

#convert data to relative abundance
epifauna.rel <-         
  decostand(epifauna1, method = "total")

# Calculate distance matrix
epifauna_distmat <- 
  vegdist(epifauna.rel, method = "bray")


epifauna_distmat <- 
  as.matrix(epifauna_distmat, labels = T)
write.csv(epifauna_distmat, "epifauna_distmat.csv")

# create grouping variable
groups_epifauna <- as.data.frame(density1$Trt.comb) 
groups_epifauna <- groups_epifauna[-23,]

# Running NMDS in vegan (metaMDS)
epifauna_NMS <-
  metaMDS(epifauna_distmat,
          distance = "bray",
          k = 3,
          maxit = 999, 
          trymax = 500,
          wascores = TRUE)

# Plotting points in ordination space
plot(epifauna_NMS, "sites", pch = "")   # Produces distance among 'sites', which for us is plots; made points invisible so we an see plot numbers with next command
orditorp(epifauna_NMS, "sites", air = 1)   # Gives points labels

#Create convex hulls that highlight point clusters based on grouping dataframe
colvec <- c("gray0", "gray0", "gray49", "gray49")   
# Identifies colors for group assignments
pchvec <- c(21, 21, 22, 22)

levels(factor(groups_epifauna)) #what order are the treatments in:
[1] "control_high" "control_low"  "exp_high"     "exp_low" 


ordihull(
  epifauna_NMS,
  groups_epifauna,
  display = "sites",
  draw = c("polygon"),
  col = NULL,
  border = c("black", "black", "gray48", "gray48"),
  lty = c(1, 2, 1, 2),
  lwd = 2.5
)

# so black solid line is control high, solid dahsed is control low; solid gray is expt high, dashed grey is control low

### species abundance distribution
library(sads)

sad_object <- sad(as.numeric(epifauna[1, 2:22]))

## or: 

hist(as.numeric(all1),
     breaks = c(0, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048), # Example log2-scale breaks
     main = "Species Abundance Distribution (SAD)",
     xlab = "Abundance (Individuals per Species)",
     ylab = "Number of Species",
     col = "skyblue",
     border = "black")



df <- data.frame(all1)
ggplot(df, aes(x = Abundance)) +
  geom_histogram(binwidth = .2, fill = "skyblue", color = "black") + # Adjust binwidth as needed
  #scale_x_log10() + # Optional: use a log scale for the x-axis
  scale_x_continuous(trans = "log2") +
  labs(title = "Species Abundance Distribution",
       x = "Abundance (Individuals per Species)",
       y = "Number of Species") +
  theme_minimal()


### Rank abundance distributions
# for plot 1: 
plot1 <- as.numeric(epifauna[1, 2:22])
P1_sorted <- sort(plot1, decreasing = TRUE)

plot2 <- as.numeric(epifauna[2, 2:22])
P2_sorted <- sort(plot2, decreasing = TRUE)

plot3 <- as.numeric(epifauna[3, 2:22])
P3_sorted <- sort(plot3, decreasing = TRUE)

plot4 <- as.numeric(epifauna[4, 2:22])
P4_sorted <- sort(plot4, decreasing = TRUE)

all <- colSums(epifauna, na.rm = TRUE)
all1 <- as.numeric(colSums(epifauna, na.rm = TRUE))
all_sorted <- sort(all1, decreasing = TRUE)

df <- data.frame(Rank = 1:length(all_sorted), Abundance = all_sorted)

ggplot(df, aes(x = Rank, y = Abundance)) +
  geom_point() +
  scale_y_log10() + # Log-transform y-axis
  labs(x = "Species Rank", y = "Abundance (log transformed)",
       title = "Rank Abundance Curve")
