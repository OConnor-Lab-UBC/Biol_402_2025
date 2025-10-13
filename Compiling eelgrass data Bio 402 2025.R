## Biol 402 data 2025
## Mary's script: data cleaning and compiling
## this is not the cleanest of code files, but happy to share if you are interested. 

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
  select(c("plot", "trt_density", "trt", "epi_shoot_length", "epi_shoot_width", "epi_shoot_no_blades", "chla_avg")) %>%
  rename(Pl.no = plot) %>%
  mutate(Pl.no = as.character(Pl.no)) %>%
  mutate(LA = epi_shoot_length * epi_shoot_width * epi_shoot_no_blades) %>%
  mutate(epi_shoot = chla_avg / LA)

## merge sept 6th density and sept 16th density in here
data1 <- left_join(density1, growth5, by = "Pl.no")
data1 <- data1 %>%
  rename(plot = Pl.no)

data_for_class <- left_join(density1, growth_for_class, by = "Pl.no")
data_for_class <- data_for_class %>%
  rename(plot = Pl.no) %>%
  select( -c("Treament", "Trt_density", "Trt.comb")) 

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


# Building final summary datafile -----------------------------------------
# take plot number, treatments and sept 6 values from density 1


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
