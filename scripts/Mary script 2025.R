## Biol 402 data 2025
## mary's script working things out

## packages
library(tidyverse)
library(nlme)
#library(MuMIn)
library(car)
library(ggplot2)
library(effects)
library(vegan)


#Read in data ----
density <- read.csv(file = "./data/density_2025.csv")
growth <- read.csv(file = "./data/biol402_growth_shoot_data.csv")
epiphytes <- read.csv(file = "./data/epiphytes.csv")
epifauna_shoots <- read.csv(file = "./data/epifauna_shoots.csv")
epifauna <- read.csv(file = "./data/epifauna.csv")
hobo_summary <- read.csv(file = "./data/light_temp_plot.csv")

# Cleaning data ----------------------------------------------------------

## clean data
### add treatment combined term, and density difference term, rename plot number. 
density1 <- density %>%
  mutate(Trt.comb = paste(Trt_density, Treament, sep = "_")) %>%
  mutate(density_diff = sept_16_density - sept_6_density) %>%
  rename(Pl.no = plot) %>%
  mutate(Pl.no = as.character(Pl.no))

#add treatment names, replace NAs with 0, make all data columns numeric
growth1 <- growth %>%
  mutate(Trt.comb = paste(trt_density, trt, sep = "_")) %>%
  mutate(across(lf1_full:bblf_3, .fns = as.numeric)) %>%
  mutate(across(internode1:internode23, .fns = as.numeric)) %>%
  mutate_all(~replace_na(., 0))

## calculate leaf extension from data to include baby leaves and reference length. what to do with ref_length? i think our measurements (leaf length, baby leaves) do not include it... but the diagram suggests that leaf length should include it. 
growth2 <- growth1 %>%
  mutate(total.ext.calc = lf1_new + lf2_new + lf3_new + lf4_new + lf5_new + lf6_new + lf7_new + lf8_new + bblf_1 + bblf_2 + bblf_3 + ref_length) %>%
  mutate(RGR.calc = total.ext.calc / sheath_length) %>%
  mutate(RGR.day.calc = RGR.calc/growth_days) 

#fix mysterious problem with plot heading, count baby leaves, leaves and internodes. 
growth2 <- growth2 %>%
  rename(Pl.no = plot) %>%
  mutate(Pl.no = as.character(Pl.no)) %>%
  rowwise() %>%
  mutate(bblf_count = sum(c_across(bblf_1:bblf_3) > 0, na.rm = TRUE)) %>%
  mutate(no_leaves = sum(c(lf1_full, lf2_full, lf3_full, lf4_full, lf5_full, lf6_full, lf7_full, lf8_full, lf9_full) > 0, na.rm = TRUE)) %>%
  mutate(no_rhiz_int = sum(c_across(internode1:internode23) > 0, na.rm = TRUE)) %>%
  mutate(plt_int = growth_days/bblf_count)

## results of quality control (later in code)
## i have most confidence in shoot length and sheath length, which would have been measured before the leaves were cut. 
## total shoot length would be node 0 to tip of longest leaf. So that should be the same as longest leaf length plus sheath length.
## total leaf extension estimate should include ref_length. 

## rows to remove b/c of possible errors in how leaf length was estimated 
#plot 12 shoots 2,3; plot 9 shoot 2 (TA data), plot 21 shoot 3 (TA); plot 20 something is wrong; plot 10 shoot 2, plot 6 shoot 1, plot 8 shoot 2

## rows with problems, based on comparing max shoot length to leaf lengths

growth3 <- growth2 %>%
  filter(!(Pl.no == "2" & shoot == "2")) %>%
  filter(!(Pl.no == "9" & shoot == "2")) %>%
  filter(!(Pl.no == "21" & shoot == "3")) %>%
  filter(!(Pl.no == "10" & shoot == "2")) %>%
  filter(!(Pl.no == "6" & shoot == "1")) %>%
  filter(!(Pl.no == "8" & shoot == "2"))
 # filter(!(Pl.no == "12" & shoot == "2")) %>%
 # filter(!(Pl.no == "12" & shoot == "3"))

# relative to max_leaf 3 estimate: 
# for plot 22: leaf length = leaf + sheath length
# for plot 1, plot 11 shoot 1, : leaf length = from node 0
# for plot 2: leaf + ref length?

growth4 <- growth3 %>%
  mutate(max_leaf = max(c_across(c(lf1_full, lf2_full, lf3_full, lf4_full, lf5_full, lf6_full, lf7_full, lf8_full)))) %>%
  mutate(max_leaf2 = max_leaf + sheath_length) %>%
  mutate(max_leaf3 = max_leaf + ref_length) %>% # shoot length
  mutate(max_leaf4 = ifelse(Pl.no == 1, max_leaf, max_leaf3)) %>%
  mutate(max_leaf4 = ifelse((Pl.no == 11 & shoot == 1), max_leaf, max_leaf4)) %>%
  mutate(max_leaf4 = ifelse((Pl.no == 22), max_leaf2, max_leaf4))

# max_leaf4 is the best estimate for max leaf. to get leaf extension, i think i have to make those corrections to each leaf for those adjusted plots.   

growth5 <- growth4 %>%
  mutate(tot_ext_corr = ifelse((Pl.no == 11 & shoot == 1), total.ext.calc - no_leaves*ref_length, total.ext.calc)) %>%
  mutate(tot_ext_corr = ifelse(Pl.no == 1, total.ext.calc - ref_length, total.ext.calc)) %>%
  mutate(RGR.corr = tot_ext_corr / sheath_length) %>%
  mutate(RGR.day.corr = RGR.corr/growth_days) 

plot_summary_growth <- growth5 %>%
  group_by(Pl.no) %>%
  summarize(GS_length = mean(shoot_length, na.rm = TRUE),
            GS_width = mean(shoot_width, na.rm = TRUE),
            GS_leaves = mean(no_blades, na.rm = TRUE),
            RGR = mean(RGR.corr))

growth_for_class <- growth5 %>%
  select( -c("date_marked", "date_collected", "total_lf_ext", "rgr", "rgr_d", "RGR.calc", "RGR.day.calc", "plt_int", "max_leaf", "max_leaf2", "max_leaf3", "max_leaf4", "total.ext.calc", "RGR.corr", "RGR.day.corr", "sheath_width", "trt_density", "trt", "Trt.comb") ) %>%
  rename(Total_extension = tot_ext_corr) #%>%
  #rename(Treatment = Trt.comb)

View(density)
names(density)
View(growth2)

## epiphyte rows with problems due to missing data (so could be updated)
epiphytes1 <- epiphytes %>%
  filter(!(epi_shoot_length == "NA2")) %>%
  select(c("plot", "trt_density", "trt", "epi_shoot_length", "epi_shoot_width", "epi_shoot_no_blades", "chla_shoot_calc")) %>%
  rename(Pl.no = plot) %>%
  mutate(Pl.no = as.character(Pl.no)) %>%
  mutate(LA = epi_shoot_length * epi_shoot_width * epi_shoot_no_blades) %>%
  mutate(epi_shoot = chla_shoot_calc / LA)

## merge sept 6th density and sept 16th density in here
data1 <- left_join(density1, growth5, by = "Pl.no")
data1 <- data1 %>%
  rename(plot = Pl.no)

data_for_class <- left_join(density1, growth_for_class, by = "Pl.no")
data_for_class <- data_for_class %>%
  rename(plot = Pl.no) %>%
  rename(Treatment = Treament)
  #select( -c("Treament", "Trt_density", "Trt.comb")) 

epi_data_density <- left_join(epiphytes1, density1, by = "Pl.no") 
epi_class <- epi_data_density %>%
  rename(plot = Pl.no) %>%
  rename(Treatment = Treament)


View(epifauna_shoots)

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

groups_epifauna <- as.data.frame(density1$Trt.comb) 
groups_epifauna <- groups_epifauna[-23,]

epifauna1 <- epifauna %>%
  select(-c("X", "X.1", "X.2", "plot")) %>%
  rowwise() %>%
  mutate(abundance = sum(c_across(Amphipods:mite))) %>%
  ungroup()
# %>%
  #rename(Pl.no = plot) %>%
  #mutate(Pl.no = as.character(Pl.no))

Pl.no = factor(epifauna$plot)
Abundance = epifauna1$abundance

Shannon = diversity(epifauna1, index = "shannon")

epifauna_div <- data.frame(Pl.no = Pl.no, Shannon = Shannon, Abundance = Abundance)

epifauna2 <- left_join(epifauna_div, density1, by ="Pl.no") 
epifauna3 <- left_join(epifauna2, epifauna_shoots1)



## make class datafiles
epi_class <- epi_data_density %>%
  rename(plot = Pl.no) %>%
  rename(Treatment = Treament)

hobos <- hobo_summary %>%
  rename(Pl.no = plot) %>%
  mutate(Pl.no = as.character(Pl.no)) %>%
  select(c("Pl.no", "mean_temp", "mean_lux")) 

hobos_trt <- left_join(hobos, density1, by ="Pl.no") %>%
  select(c("Pl.no", "mean_temp", "mean_lux", "Treament", "Trt_density", "Trt.comb")) %>%
  rename(Treatment = Treament)

write.csv(hobos_trt, "hobos_trt.csv")


# final class summary data: 
# take plot number, treatments and sept 6 values from density 1
# take 

Summary_data <- density1 %>%
  select(Pl.no, Treament, Trt_density, Trt.comb, sept_16_density, sept_16_macro, sept_16_detritus, sept_16_zj)

# add in plot-level summaries of growth shoot RGR, length and width
Summary_data2 <- left_join(Summary_data, plot_summary_growth, by ="Pl.no")

# add in epi shoot morphology and epiphytes 
Summary_data3 <- left_join(Summary_data2, epiphytes1, by = "Pl.no") %>%
  select(-c("trt_density", "trt", "LA", "epi_shoot"))

# add in epifauna LAI and density
Summary_data4 <- left_join(Summary_data3, epifauna_shoots1, by = "Pl.no") %>%
  rename(EF_length = mean_length) %>%
  rename(EF_width = mean_width) %>%
  rename(EF_leaves = mean_leaves) %>%
  rename(EF_density = density) %>%
  rename(EF_LAI = LAI)

# add in epifauna 
Summary_data5 <- left_join(Summary_data4, epifauna_div, by = "Pl.no")

## add in light and temp
Summary_data_final <- left_join(Summary_data5, hobos, by = "Pl.no")
write.csv(Summary_data_final, "Biol_402_Seagrass_Summary_2025.csv")


write.csv(density1, "density_class.csv")
write.csv(data_for_class, "growth_class.csv")
write.csv(data1, "combined.csv")
write.csv(epi_class, "epiphytes_class.csv")


# #Visualization - density ------------------------------------------------

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



# #Visualization - growth -------------------------------------------------

# QA/QC: 



# 1. reference length should be a subset of sheath length, therefore ref_length < sheath length. 
  RGR.sheath.ref <- ggplot(growth3, aes(x = sheath_length, y = ref_length)) +
  geom_point() +
  xlab("sheath length") +
  ylab("reference length") +
  ggtitle("RGR.sheath.ref") +
  geom_abline(intercept = 0, slope = 1)
RGR.sheath.ref


length_test <- ggplot(growth5, aes(x = shoot_length, y = max_leaf4)) +
  #geom_point() +
  geom_text(aes(label = Pl.no)) +
  xlab("shoot_length") +
  ylab("max_leaf") +
  ggtitle("length test") +
  geom_abline(intercept = 0, slope = 1)
length_test

length_test2 <- ggplot(growth5, aes(x = total.ext.calc, y = tot_ext_corr)) +
  #geom_point() +
  geom_text(aes(label = Pl.no)) +
  xlab("total_ext_calc") +
  ylab("tot_ext_corr") +
  ggtitle("length test2") +
  geom_abline(intercept = 0, slope = 1)
length_test2

mod1 <- lm(shoot_length ~ sheath_length, data = growth)
summary(mod1) # gives you the regression analysis

sheath_shoot <- ggplot(growth, aes(x = sheath_length, y = shoot_length)) +
  geom_point() +
  xlab("Sheath length") +
  ylab("Shoot length") +
  ggtitle("Shoot length and Sheath Length") +
  geom_abline(intercept = 63.7606, slope = 0.9891) # you can put the results of the regression model in here for XX and YY to plot the line on the graph. 

#this above indicates that for most of them, they measured leaf length from the cut. i cleaned up the exceptions and removed plots i couldn't figure out. some can be restored if students answer questions and with TA data perhaps. 

## the implications of this are that i need to make the same corrections for all leaves as for max leaves. 


hist(growth1$RGR.calc)


RGR.test <- ggplot(growth5, aes(x = rgr, y = RGR.calc)) +
  geom_point() +
  xlab("RGR.excel") +
  ylab("RGR.script") +
  ggtitle("RGR.test")
RGR.test

RGR.test <- ggplot(growth5, aes(x = RGR.calc, y = RGR.corr)) +
  geom_point() +
  xlab("RGR.calc") +
  ylab("RGR.corr") +
  ggtitle("RGR.test")
RGR.test

# blades is what they entered in the datasheet. this sometimes (but one always) includes baby leaves. leaves is calculated from the data above using code, and does not include baby leaves. use leaves for analysis
blades <- ggplot(growth5, aes(x = no_blades, y = no_leaves)) +
  geom_point() +
  xlab("no_blades") +
  ylab("no_leaves") +
  ggtitle("blades and leaves") +
  geom_abline(intercept = 0, slope = 1)
blades

total_leaf_ext <- ggplot(growth5, aes(x = total_lf_ext, y = total.ext.calc)) +
  geom_point() +
  xlab("total ext lab sheet") +
  ylab("total ext calc") +
  ggtitle("total ext") +
  geom_abline(intercept = 0, slope = 1)
total_leaf_ext

shoot_length <- ggplot(growth, aes(x = shoot_length)) +
  geom_histogram(binwidth=1) +
  geom_vline(aes(xintercept = mean(shoot_length))) +
  geom_vline(aes(xintercept = mean(shoot_length) - sd(shoot_length)), linetype = "dashed") +
  geom_vline(aes(xintercept = mean(shoot_length) + sd(shoot_length)), linetype = "dashed") +
  xlab("Shoot Length") +
  ylab("Frequency")
shoot_length

no_leaves <- ggplot(growth, aes(x = no_leaves)) +
  geom_histogram(binwidth=.5) +
  geom_vline(aes(xintercept = mean(no_leaves))) +
  geom_vline(aes(xintercept = mean(no_leaves) - sd(no_leaves)), linetype = "dashed") +
  geom_vline(aes(xintercept = mean(no_leaves) + sd(no_leaves)), linetype = "dashed") +
  xlab("no_leaves") +
  ylab("Frequency")
no_leaves


# Visualization Epifauna Shoots -------------------------------------------
mod1 <- lm(shoot_length~sheath_length, data = epifauna_shoots)


sheath_shoot <- ggplot(epifauna_shoots, aes(x = sheath_length, y = shoot_length)) +
  geom_point() +
  xlab("Sheath length") +
  ylab("Shoot length") +
  ggtitle("Epifauna Shoot length and Sheath Length") +
  geom_abline(intercept = 57.6584, slope = 1.7)

density_size <- ggplot(epifauna_shoots, aes(x = sheath_length, y = number_shoots)) +
  geom_point() +
  xlab("Density") +
  ylab("Shoot length") +
  ggtitle("Epifauna Shoot length and Density") 

mod1 <- lme(shoot_length~number_shoots, data = epifauna_shoots, random = ~ 1 | plot)
summary(mod1)

density_epifauna_shoots <- ggplot(epifauna_density, aes(x = sept_16_density, y = density)) +
  geom_point() +
  xlab("sept_16_density") +
  ylab("Density") +
  ggtitle("Epifauna Shoot Density by plot density") +
  geom_abline(intercept = 57.6584, slope = 1.7)

mod2 <- lm(density~sept_16_density, data = epifauna_density)
summary(mod2)

mod3 <- lm(density~ Trt.comb, data = epifauna_density)
summary(mod3)



Epifauna_Shannon <- ggplot(epifauna2, aes(x = sept_16_density, y = Shannon)) +
  geom_point() +
  xlab("sept_16_density") +
  ylab("Shannon") +
  ggtitle("Epifauna Shannon diversity by plot density") +
  geom_abline(intercept = 57.6584, slope = 1.7)

Epifauna_Abundance <- ggplot(epifauna3, aes(x = LAI, y = Abundance)) +
  geom_point() +
  xlab("LAI") +
  ylab("Abundance") +
  ggtitle("Epifauna N by LAI") 

Epifauna_Abundance <- ggplot(epifauna3, aes(x = LAI, y = Shannon)) +
  geom_point() +
  xlab("LAI") +
  ylab("Shannon") +
  ggtitle("Epifauna S by LAI") 

Epifauna_Richness <- ggplot(epifauna3, aes(x = Abundance, y = Shannon)) +
  geom_point() +
  xlab("Abundance") +
  ylab("Shannon") +
  ggtitle("Epifauna S by N")

Epifauna_N_phyll <- ggplot(epifauna1, aes(x = (Abundance - Phyllaplysia), y = Phyllaplysia)) +
  geom_point() +
  xlab("Abundance") +
  ylab("Phyllaplysia") +
  ggtitle("Phyllaplysia")

# next steps: remove outliers for now, then create class exercise to describe growth and look for patterns in these data. 

hist(epi_class$epi_shoot)
epi_treatment <- ggplot(epi_class, aes(x = trt_density, y = epi_shoot)) +
  geom_point() +
  geom_boxplot() +
  xlab("Treatment") +
  ylab("Epiphytes / cm2") +
  ggtitle("epiphytes / cm2")
epi_treatment

mod_e <- lm(epi_shoot ~ trt_density*trt, data = epi_class)
summary(mod_e)

mod_e1 <- lm(epi_shoot ~ trt_density, data = epi_class)
summary(mod_e1)

mod_e2 <- lm(epi_shoot ~ sept_16_density, data = epi_class)
summary(mod_e2)

mod_e3 <- lm(epi_shoot_length ~ trt_density*trt, data = epi_class)
summary(mod_e3)

epi_density <- ggplot(epi_class, aes(x = sept_16_density, y = epi_shoot)) +
  geom_point() +
  xlab("density") +
  ylab("Epiphytes / cm2") +
  ggtitle("epiphytes / cm2")
epi_density

epi_per_shoot <- ggplot(epi_class, aes(x = epi_shoot_length, y = epi_shoot)) +
  geom_point() +
  xlab("shoot length") +
  ylab("Epiphytes / cm2") +
  ggtitle("epiphytes / cm2")
epi_per_shoot

epi_shoots <- ggplot(epiphytes, aes(x = epi_shoot_sheath, y = epi_shoot_length)) +
  geom_point() +
  xlab("Sheath") +
  ylab("Shoot") +
  ggtitle("Shoot by Sheath epiphytes")
epi_shoots

epi_shootL <- ggplot(epi_class, aes(x = Trt.comb, y = epi_shoot_length)) +
  geom_point() +
  xlab("Treatment") +
  ylab("Shoot") +
  ggtitle("Shoot by Sheath epiphytes")
epi_shootL

# ## exploring relationships among shoot size and growth rate -------------

RGR <- ggplot(growth5, aes(x = RGR.corr)) +
  geom_histogram(binwidth=1) +
  geom_vline(aes(xintercept = mean(RGR.corr))) +
  geom_vline(aes(xintercept = mean(RGR.corr) - sd(RGR.corr)), linetype = "dashed") +
  geom_vline(aes(xintercept = mean(RGR.corr) + sd(RGR.corr)), linetype = "dashed") +
  xlab("Relative Growth Rate") +
  ylab("Frequency")
RGR

ggsave("RGR.jpg", shoot_length)

Tot_ext <- ggplot(growth5, aes(x = total.ext.calc)) +
  geom_histogram(binwidth=2) +
  geom_vline(aes(xintercept = mean(total.ext.calc))) +
  geom_vline(aes(xintercept = mean(total.ext.calc) - sd(total.ext.calc)), linetype = "dashed") +
  geom_vline(aes(xintercept = mean(total.ext.calc) + sd(total.ext.calc)), linetype = "dashed") +
  xlab("Total exentsion") +
  ylab("Frequency")
RGR

RGR.sheath <- ggplot(growth5, aes(x = sheath_length, y = RGR.corr, color = Trt.comb)) +
  geom_point() +
  xlab("sheath length") +
  ylab("RGR") +
  ggtitle("RGR.sheath")
RGR.sheath

TE.sheath <- ggplot(growth5, aes(x = sheath_length, y = tot_ext_corr, color = Trt.comb)) +
  geom_point() +
  xlab("sheath length") +
  ylab("total_extension") +
  ggtitle("TE by sheath length") +
  geom_abline(intercept = 48.4606, slope = 1.7246)
TE.sheath

mod3c <- lm(tot_ext_corr ~ sheath_length, data = growth5)
summary(mod3c)

# with or without plot 12, no difference in pattern

RGR.width <- ggplot(growth5, aes(x = sheath_width, y = RGR.corr, color = Trt.comb)) +
  geom_point() +
  xlab("sheath width") +
  ylab("RGR") +
  ggtitle("RGR.width")
RGR.width

length.sheath <- ggplot(growth5, aes(x = sheath_length, y = (lf3_full+ ref_length), color = Trt.comb)) +
  geom_point() +
  xlab("sheath length") +
  ylab("Leaf 3 full") +
  ggtitle("length_sheath")
length.sheath

length.sheath <- ggplot(growth5, aes(x = sheath_length, y = (shoot_length), color = Trt.comb)) +
  #geom_point() +
  geom_text(aes(label = Pl.no)) +
  xlab("sheath length") +
  ylab("shoot length") +
  ggtitle("length_sheath")
length.sheath

leaves1 <- ggplot(growth5, aes(x = (lf3_full+ ref_length), y = (lf4_full+ ref_length))) +
  geom_point() +
  xlab("leaf 3") +
  ylab("leaf 4") +
  ggtitle("leaves 3 and 4")
leaves1

hist(data1$no_leaves)

hist(data1$plt_int)

## summary of morphology: 
hist(data1$RGR.calc)
hist(data1$RGR.day.calc)
hist(data1$total.ext.calc)

# ## testing for effects of experimental treatments -----------------------

density_d_RGR <- ggplot(data1, aes(x = density_diff, y = RGR.calc)) +
  geom_point() +
  xlab("Density diff") +
  ylab("RGR") +
  ggtitle("RGR")
density_d_RGR

density_RGR <- ggplot(data1, aes(x = sept_6_density, y = RGR.calc)) +
  geom_point() +
  xlab("Density Sept 6") +
  ylab("RGR") +
  ggtitle("RGR") 
density_RGR

density_RGR <- ggplot(Summary_data_final, aes(x = sept_16_density, y = RGR)) +
  geom_point() +
  xlab("Density Sept 16") +
  ylab("RGR") +
  ggtitle("RGR") 
density_RGR


density_F_RGR <- ggplot(data1, aes(x = sept_16_density, y = RGR.calc)) +
  geom_point() +
  xlab("Density Sept 16") +
  ylab("RGR") +
  ggtitle("RGR") +
  geom_abline(intercept = 6.36, slope = -0.02)
density_F_RGR


treatments_RGR <- ggplot(growth5, aes(x = Trt.comb, y = RGR.corr)) +
  geom_boxplot() +
  geom_point() +
  xlab("Density Treatment") +
  ylab("RGR") +
  ggtitle("RGR")
treatments_RGR

density_RGR <- ggplot(data1, aes(x = sept_16_density, y = RGR.corr)) +
  geom_point() +
  xlab("Density_Final") +
  ylab("RGR") +
  ggtitle("RGR")
density_RGR

density_TE <- ggplot(data1, aes(x = sept_16_density, y = total.ext.calc)) +
  geom_point() +
  xlab("Density_Final") +
  ylab("Tot_ext") +
  ggtitle("Total leaf Extension") +
geom_abline(intercept = 104.8242, slope = -.3557)
density_TE

treatments_RGRd <- ggplot(growth5, aes(x = Trt.comb, y = RGR.day.corr)) +
  geom_boxplot() +
  geom_point() +
  xlab("Density Treatment") +
  ylab("RGR") +
  ggtitle("RGR")
treatments_RGRd


treatment_sheath <- ggplot(growth5, aes(x = Trt.comb, y = sheath_length)) +
  geom_boxplot() +
  geom_point() +
  xlab("treatment") +
  ylab("sheath length") +
  ggtitle("sheath length")
treatment_sheath

treatment_tot_ext <- ggplot(growth5, aes(x = Trt.comb, y = tot_ext_corr)) +
  geom_boxplot() +
  geom_point() +
  xlab("treatment") +
  ylab("total exentsion") +
  ggtitle("total extension")
treatment_tot_ext

treatment_max_leaf <- ggplot(growth5, aes(x = Trt.comb, y = max_leaf4)) +
  geom_boxplot() +
  geom_point() +
  xlab("treatment") +
  ylab("max_leaf") +
  ggtitle("total extension")
treatment_max_leaf

treatment_shoot_length <- ggplot(growth5, aes(x = Trt.comb, y = shoot_length)) +
  geom_boxplot() +
  geom_point() +
  xlab("treatment") +
  ylab("shoot_length") +
  ggtitle("shoot_length")
treatment_shoot_length

treatment_baby_leaf <- ggplot(growth5, aes(x = Trt.comb, y = bblf_count)) +
  geom_boxplot() +
  geom_point() +
  xlab("treatment") +
  ylab("bb_leaf") +
  ggtitle("baby leaves")
treatment_baby_leaf


pl_int <- ggplot(data1, aes(x = Trt.comb.y, y = plt_int)) +
  #geom_boxplot() +
  geom_point() +
  xlab("treatment") +
  ylab("Pl interval") +
  ggtitle("pl_interval")
pl_int

## no effect of RGR and treatments
mod1 <- lm(RGR.corr ~ trt*trt_density, data = growth5)
summary(mod1)

mod1a <- lme(RGR.corr ~ trt*trt_density, random = ~ 1 | Pl.no, data = growth5)

## hint of an effect of sheath length and treatments
mod2 <- lm(sheath_length ~ trt*trt_density, data = growth5)
summary(mod2)

mod2b <- lm(sheath_width ~ trt*trt_density, data = growth5)
summary(mod2b)

mod2a <- lme(sheath_length ~ trt*trt_density, random = ~ 1 | Pl.no, data = growth5)
summary(mod2a)

## extension did not vary with treatments, but did vary with sheath length and treatment
mod3 <- lm(tot_ext_corr ~ trt*trt_density, data = growth5)
summary(mod3)

mod3d <- lm(tot_ext_corr ~ sept_16_density, data = data1)
summary(mod3d)

mod3a <- lme(tot_ext_corr ~ trt*trt_density, random = ~ 1 | Pl.no, data = growth5)
summary(mod3a)

mod3b <- lm(tot_ext_corr ~ sheath_length*trt, data = growth5)
summary(mod3b) ## check this in a figure, the coefs look weird

## total leaf extension depends on sheath length
mod3c <- lm(tot_ext_corr ~ sheath_length, data = growth5)
summary(mod3c)

# longer leaves in experimental treatments, but much shorter in low density natural
mod4 <- lm(max_leaf4 ~ trt*trt_density, data = growth5)
summary(mod4)

#as expected
mod5 <- lm(growth5$sheath_length ~ growth5$shoot_length)
summary(mod5)

#no effect of rgr with sheath length or number of leaves
mod6 <- lm(RGR.corr ~ sheath_length*no_leaves, data = growth5)
summary(mod6)

#no effect of rgr with density
mod7 <- lm(RGR.corr ~ sept_6_density, data = data1)
summary(mod7)

#RGR declined at higher density
mod8 <- lm(RGR.corr ~ sept_16_density*trt, data = data1)
summary(mod8)

mod8 <- lme(RGR.corr ~ sept_16_density*trt, random = ~ 1 | plot, data = data1)
summary(mod8)

## where am i at with this? 
## shoot length and longest leaf do vary among plots
## no clear role of sheath length, which is odd. there is a pretty clear correlation between sheath length and shoot length (as we would expect) with some exceptions and some of those are the plots where i identified some in consistencies. 
## i have highest confidence in shoot length and then sheath length data. it is possible they measured not the max sheath... but even so i think we can use this relationship. 
## worth doing leaf elongation of top 3 leaves?

## their activity can be make a few plots to explore correlations, describe average conditions and variation, explore plastochrone interval, leaf extenstion and RGR metrics. 


# midterm questions -------------------------------------------------------

Kd.func <- function (z) (1/z) * log(E.0 / E.z)
Ez_func <- function (K) E.0 * exp(-K*z)

z <- seq(1,100,1)
E.0 <- 2000
E.z <- 300

Kd.func(3)

z<- 30
Ez_func(.2)

treatment_baby_leaf <- ggplot(growth5, aes(x = Trt.comb, y = bblf_count)) +
  geom_boxplot() +
  geom_point() +
  xlab("treatment") +
  ylab("bb_leaf") +
  ggtitle("baby leaves")
treatment_baby_leaf
