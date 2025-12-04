# ============================================
# TEAM TIME: More time points vs. power
# Relationship: fish richness ~ kelp_density
# ============================================

#loading in the necessary packages
#install.packages(pacman)
pacman::p_load(tidyverse, pwr)
#reading in the raw fish data (edit these file paths to be wherever you saved these csv files)
fish_raw <- read_csv("~/Desktop/statistical fallacy lecture/pacific_rim_npr_coastalmarine_kelp_fish_community_2008-2016_data.csv") 
  #reading in the kelp density (edit these file paths to be wherever you saved these csv files)
kelp_raw <- read_csv("~/Desktop/statistical fallacy lecture/pacific_rim_npr_coastalmarine_kelp_density_2013-2016_data.csv") 
  
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


# step 1: look at the relationship with 2 years at one site
# dataframe just two sites
two_years <- complete_data %>% 
  filter(year %in% c(2012:2014)) %>%
  filter(site %in% c("DK"))
# linear model with just two years
two_years_lm <- lm(richness ~ total_kelp_density, data = two_years)
# looking at the output of this linear model 
# what is the slope of the relationship (check out the estimate column and the number next to "total_kelp density")
summary(two_years_lm)
# assessing this linear model
# what does the fit look like (what is the r2?)
r2_two_years <- summary(two_years_lm)$r.squared 
# what is the effects size
f2_two_years <- r2_two_years / (1 - r2_two_years)
# how many samples would you need to see a significant result?
pwr.f2.test(u = 1, f2 = f2_two_years, sig.level = 0.05, power = 0.8)
#looking the outputs from the power analysis
#u: The numerator degrees of freedom, which typically corresponds to the number of predictor variables in the model (excluding the intercept). 
#v: The denominator degrees of freedom, representing the error degrees of freedom, calculated as n - u - 1 (where n is the total sample size). 
#f2: Cohen's effect size for the general linear model, calculated as \(R^{2}/(1-R^{2})\), where \(R^{2}\) is the coefficient of determination (the proportion of variance explained by the model).
#sig.level: The significance level (alpha), representing the Type I error probability. power: The desired statistical power, representing the probability of correctly rejecting a false null hypothesis (1 - Type II error probability).
#keep these outputs in mind as you do the other power analyses


# step 2: look at the relationship with 3 years at one site
# just four sites
three_years <- complete_data %>% 
  filter(year %in% c(2013:2015)) %>%
  filter(site %in% c("DK"))
# linear model with just four sites
three_years_lm <- lm(richness ~ total_kelp_density, data = three_years)
# looking at the output of this linear model 
# what is the slope of the relationship (check out the estimate column and the number next to "total_kelp density")
summary(three_years_lm)
# assessing this linear model
# what does the fit look like (what is the r2?)
r2_three_years <- summary(three_years_lm)$r.squared 
# what is the effects size
f2_three_years <- r2_three_years / (1 - r2_three_years)
# how many samples would you need to see a significant result?
pwr.f2.test(u = 1, f2 = f2_three_years, sig.level = 0.05, power = 0.8)

# step 3: look at the relationship with 4 years at one site
four_years <- complete_data %>%
  filter(site %in% c("DK"))
# linear model with just four sites
four_years_lm<- lm(richness ~ total_kelp_density, data = four_years)
# looking at the output of this linear model 
# what is the slope of the relationship (check out the estimate column and the number next to "total_kelp density")
summary(four_years_lm)
# assessing this linear model
# what does the fit look like (what is the r2?)
r2_four_years <- summary(four_years_lm)$r.squared 
# what is the effects size
f2_four_years <- r2_four_years / (1 - r2_four_years)
# how many samples would you need to see a significant result?
pwr.f2.test(u = 1, f2 = f2_four_years, sig.level = 0.05, power = 0.8)







