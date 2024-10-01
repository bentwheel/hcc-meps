# normalize-scores.R

library(tidyverse)
library(viridis)
library(ggrepel)

# No scientific notation
options(scipen = 999)


normalize_scores <- function() 
{
  # Load in demographics file used for RA model demo inputs
  demographics <- read_csv(here::here("etc/outputs/demographics.csv")) %>% 
    mutate(DUPERSID = as.character(DUPERSID),
           meps_year = as.character(meps_year))
  
  load(here::here("etc/outputs/mode_hcc_scores.rda"))
  load(here::here("etc/outputs/risk_scores.rda"))
  
  #### Normalize Scores ####
  # Process and normalize risk scores for each model population
  scores_eval_NE <- mode_hcc_scores %>%
    filter(elig_args == "NE") %>% 
    mutate(scoremodel = paste0("v",model,"_mode")) %>% 
    select(DUPERSID, meps_year, elig_args, scoremodel, risk_score) %>% 
    pivot_wider(names_from = scoremodel,
                values_from = risk_score)
  
  # Now do the same for CNA
  mode_scores_eval_CNA <- mode_hcc_scores %>%
    filter(elig_args == "CNA") %>% 
    mutate(scoremodel = paste0("v",model,"_mode")) %>% 
    select(DUPERSID, meps_year, elig_args, scoremodel, risk_score)
  
  # Get CNA scores for the same runs - but use the averages and not modes 
  avg_scores_eval_CNA <- risk_scores %>% 
    filter(elig_args == "CNA") %>% 
    mutate(scoremodel = paste0("v",model,"_avg")) %>% 
    select(DUPERSID, meps_year, elig_args, scoremodel, risk_score)
  
  # Combine all scores, pivot, and join to expos/costs info for validation & normalization
  scores_eval_CNA <- mode_scores_eval_CNA %>% 
    union_all(avg_scores_eval_CNA) %>% 
    pivot_wider(names_from = scoremodel,
                values_from = risk_score)
  
  # Get expos in the projection year for scaling risk scores
  proj_expos <- read_csv(here::here("etc/outputs/expenditures_pmpm.csv")) %>% 
    mutate(DUPERSID = as.character(DUPERSID),
           meps_year = as.character(meps_year)) %>% 
    filter(Type == "MCR") %>% 
    select(DUPERSID, meps_year, Expos) %>% 
    filter(meps_year != "2016") %>% 
    mutate(meps_year = as.character(as.numeric(meps_year) - 1))
  
  # Normalize for each year and for all score model outputs
  scores_eval_NE_norm <- scores_eval_NE %>% 
    group_by(meps_year, elig_args) %>% 
    left_join(proj_expos) %>% 
    relocate(Expos, .after=meps_year) %>% 
    rename(model = elig_args) %>% 
    mutate(across(
      .cols = matches("^v(24|28)_(mode|avg)$"),
      .fns = list(mean = function(x) { x / weighted.mean(x, w = Expos)}),
      .names = "{.col}_norm"
    ))
  
  # This check to see normalization was done appropriately for each score - should be all 1's in all rows and columns
  scores_eval_NE_norm_check <- scores_eval_NE_norm %>% 
    group_by(meps_year, model) %>% 
    summarize(across(
      .cols = matches("^v(24|28)_(mode|avg)_norm$"),
      .fns = list(mean = function(x) { weighted.mean(x, w= Expos)}),
      .names = "{.col}_check"
    )) 
  
  # Normalize for each year and for all 8 score model outputs
  scores_eval_CNA_norm <- scores_eval_CNA %>% 
    group_by(meps_year, elig_args) %>% 
    left_join(proj_expos) %>% 
    relocate(Expos, .after=meps_year) %>% 
    rename(model = elig_args) %>% 
    mutate(across(
      .cols = matches("^v(24|28)_(mode|avg)$"),
      .fns = list(mean = function(x) { x / weighted.mean(x, w = Expos)}),
      .names = "{.col}_norm"
    )) 
  
  # This check to see normalization was done appropriately for each score - should be all 1's in all rows and columns
  scores_eval_CNA_norm_check <- scores_eval_CNA_norm %>% 
    group_by(meps_year, model) %>% 
    summarize(across(
      .cols = matches("^v(24|28)_(mode|avg)_norm$"),
      .fns = list(mean = function(x) { weighted.mean(x, w= Expos)}),
      .names = "{.col}_check"
    )) 
  
  # Place both sets of score calcs into tidy form for exportation in shareable data asset
  scores_eval_norm_CNA_tidy <- scores_eval_CNA_norm %>% 
    rename_with(~paste0(., "_raw"), matches("^v(24|28)_(mode|avg)$")) %>% 
    pivot_longer(
      cols = -c("DUPERSID", "meps_year", "Expos", "model"),
      names_to = c("version", "calculation", "score_adj"),
      values_to = "score",
      names_pattern = "^v(24|28)_(mode|avg)_(raw|norm)$"
    ) %>% 
    pivot_wider(
      names_from = "score_adj",
      values_from = "score",
      names_prefix = "score_"
    )
  
  scores_eval_norm_NE_tidy <- scores_eval_NE_norm %>% 
    rename_with(~paste0(., "_raw"), matches("^v(24|28)_mode$")) %>% 
    pivot_longer(
      cols = -c("DUPERSID", "meps_year", "Expos", "model"),
      names_to = c("version", "calculation", "score_adj"),
      values_to = "score",
      names_pattern = "^v(24|28)_(mode|avg)_(raw|norm)$"
    ) %>% 
    pivot_wider(
      names_from = "score_adj",
      values_from = "score",
      names_prefix = "score_"
    )
  
  # Write normalized scores to CSV file
  scores_eval_norm <- scores_eval_norm_CNA_tidy %>% 
    union_all(scores_eval_norm_NE_tidy) %>% 
    rename(exposure_months = Expos) %>% 
    write_csv(here::here("etc/outputs/cna_population_scores.csv"))

}
