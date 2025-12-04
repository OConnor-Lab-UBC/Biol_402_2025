# ============================================
# TEAM SITES: More sites vs. power
# Relationship: fish richness ~ kelp_density
# ============================================

#loading in the necessary packages
#install.packages(pacman)
pacman::p_load(tidyverse, pwr)
#reading in the raw fish data (edit these file paths to be wherever you saved these csv files)
fish_raw <- read_csv("~/Downloads/pacific_rim_npr_coastalmarine_kelp_fish_community_2008-2016_data.csv") %>%
#reading in the kelp density (edit these file paths to be wherever you saved these csv files)
kelp <- read_csv("~/Downloads/pacific_rim_npr_coastalmarine_kelp_density_2013-2016_data.csv") %>%
  
#cleaning up the raw fish data
fish <- fish_raw %>% 
  #removing the first row because it's in French
  slice(-1) %>%
  #renaming all the variable names to be in a tidy format
  rename(site = `Site identification`,
         fish_species = `Species code`,
         fish_count = `Count`,
         year = Year,
         transect = Transect) %>%
  #selecting only the columns we need
  select(year, transect, site, fish_species, fish_count) %>%
  #grouping by site, year, and transect to ultimately get a richness across each site/year/transect combination
  group_by(site, year, transect) %>%
  summarise(richness = n_distinct(fish_species)) %>%
  mutate(richness=as.numeric(richness))

#cleaning up the raw kelp data
kelp <- kelp_raw %>% 
  #removing the first row because it's in French
  slice(-1) %>%
  #renaming all the variable names to be in a tidy format
  rename(site = `Site identification`,
         kelp_species = `Species Code`, 
         kelp_count = `Count (#/m2)`, 
         year=Year, 
         transect = Transect) %>%
  #selecting only the columns we need
  select(year, transect, site, kelp_species, kelp_count) %>%
  #grouping by site, year, and transect to ultimately get a richness across each site/year/transect combination
  group_by(site, year, transect) %>%
  mutate(kelp_count=as.numeric(kelp_count)) %>%
  summarise(total_kelp_density = sum(kelp_count))

#joining this into a dataset with both fish richness and kelp density
complete_data <- left_join(kelp, fish, by=c("site", "year", "transect")) 

### DIY ###
# 1. Visualize the "complete_data" to look at trends and patterns across time
# 2. Hypothesize mechanisms for why you think this pattern might exist
# 3. Revisit these mechanisms after we do some brief analysis and a power analysis


# step 1: look at the relationship with 2 sites in 2015
# dataframe just two sites
two_sites <- complete_data %>% 
  filter(year == 2015) %>%
  filter(site %in% c("DK", "EBK"))
# linear model with just two sites
two_sites_lm <- lm(richness ~ total_kelp_density, data = two_sites)
# looking at the output of this linear model 
# what is the slope of the relationship (check out the estimate column and the number next to "total_kelp density")
summary(two_sites_lm)
# assessing this linear model
# what does the fit look like (what is the r2?)
r2_two_sites <- summary(two_sites_lm)$r.squared 
# what is the effects size
f2_two_sites <- r2_two_sites / (1 - r2_two_sites)
# how many samples would you need to see a significant result?
pwr.f2.test(u = 1, f2 = f2_two_sites, sig.level = 0.05, power = 0.8)


# step 2: look at the relationship with 4 sites in 2015
# just four sites
four_sites <- complete_data %>% 
  filter(year == 2015) %>%
  filter(site %in% c("DK", "EBK", "HIK", "IHK"))
# linear model with just four sites
four_sites_lm<- lm(richness ~ total_kelp_density, data = four_sites)
# looking at the output of this linear model 
# what is the slope of the relationship (check out the estimate column and the number next to "total_kelp density")
summary(four_sites_lm)
# assessing this linear model
# what does the fit look like (what is the r2?)
r2_four_sites <- summary(four_sites_lm)$r.squared 
# what is the effects size
f2_four_sites <- r2_four_sites / (1 - r2_four_sites)
# how many samples would you need to see a significant result?
pwr.f2.test(u = 1, f2 = f2_four_sites, sig.level = 0.05, power = 0.8)

# step 3: look at the relationship with 6 sites in 2015
six_sites <- complete_data %>% 
  filter(year == 2015)
# linear model with just four sites
six_sites_lm<- lm(richness ~ total_kelp_density, data = six_sites)
# looking at the output of this linear model 
# what is the slope of the relationship (check out the estimate column and the number next to "total_kelp density")
summary(six_sites_lm)
# assessing this linear model
# what does the fit look like (what is the r2?)
r2_six_sites <- summary(six_sites_lm)$r.squared 
# what is the effects size
f2_six_sites <- r2_six_sites / (1 - r2_six_sites)
# how many samples would you need to see a significant result?
pwr.f2.test(u = 1, f2 = f2_six_sites, sig.level = 0.05, power = 0.8)





 