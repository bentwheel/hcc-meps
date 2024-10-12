# R helper script to generate cost predictions based on normalized risk scores outputted from
# the primary process runner (main.R).

# This script is intended to be called directly from README.md to prepare cost predictions for plotting.

library(tidyverse)
library(viridis)
library(ggrepel)

# No scientific notation
options(scipen = 999)

# Load in demographics file used for RA model demo inputs
demographics <- read_csv(here::here("etc/outputs/demographics.csv")) %>% 
  mutate(DUPERSID = as.character(DUPERSID),
  meps_year = as.character(meps_year))

# Load in risk scores
risk_scores <- read_csv(here::here("etc/outputs/cna_population_scores.csv"))

#### Develop Cost Predictions ####
# Get Actual Medicare costs for the subsequent year, for each year, for each member in the CNA population
cna_pop_pmpm_act <- read_csv(here::here("etc/outputs/expenditures_pmpm.csv")) %>% 
  filter(Type == "MCR") %>% 
  filter(meps_year != 2016) %>%  # We don't have 2015 prospective scores to scale with 2016 PMPM totals in this dataset
  mutate(meps_year = meps_year - 1) %>% # We want to apply 2017 PMPM averages to 2016 scores, 2018 PMPM avg to 2017 scores, etc.
  inner_join(risk_scores %>% distinct(DUPERSID, meps_year)) # Filtration join for correct PMPM calcs 
 
# Develop PMPM averages by year for risk score scaling
average_pmpm_cna_pop <- cna_pop_pmpm_act %>% 
  group_by(meps_year) %>% 
  summarize(total_exp_months = sum(Expos, na.rm=T),
            total_cost = sum(Cost, na.rm=T),
            total_cost_norx = sum(Cost_noRX, na.rm=T)) %>% 
  mutate(avg_cost_pmpm = total_cost / total_exp_months,
         avg_cost_pmpm_norx = total_cost_norx / total_exp_months)

# Scale risk scores
cna_pop_pmpm_pred <- risk_scores %>% 
  left_join(average_pmpm_cna_pop %>% select(meps_year, avg_cost_pmpm_actual = avg_cost_pmpm)) %>% 
  mutate(cost_pmpm_pred = avg_cost_pmpm_actual * score_norm) %>% 
  left_join(cna_pop_pmpm_act %>% select(DUPERSID, meps_year, cost_pmpm_act = PMPM_Cost)) %>% 
  mutate(meps_year = meps_year + 1) %>%  # Post-join, this should reflect accurate year for cost actuals vs. predictions
  write_csv(here::here("etc/outputs/cna_population_pred_costs.csv"))
