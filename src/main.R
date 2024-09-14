# main.R

## REQUIRED LIBRARIES
library(tidyverse)
library(cli)
library(furrr)
library(future)

# This must be installed "from scratch" via Github, consider using devtools::install_github()
library(MEPS)

# Prepare data for and run score models for MEPS population, 2016 - 2022, 
# using the CNA, CFA, and NE models for OREC code 0 (aged). 
# Arguments:
# - n_dx_sim - Number of DX scenarios based on full frequency of ICD-10 codes.
# - sim_only_hcc_dxcodes - If TRUE, any ICD-10-CM codes truncated to 3 digits from MEPS that 
#     couldn't possibly map to an HCC in v24 or v28 are removed entirely from the simulation process unless
#     they map to a full ICD-10-CM code with probability of 1 based on the ICD-10-CM frequency distribution file.
#     (e.g., all I10. dx codes). This won't impact score outputs, but DX profile simulations will be far more simple
#     and the process will run substantially faster.
# - sample_prop - percentage of MEPS respondents to sample (used primarily for testing)
main <- function(n_dx_sim = 250, sim_only_hcc_dxcodes = T, sample_prop = 1) {
  
  # Prepare ICD-10-CM to CCSR Crossmap Files for each year, 2016 - 2022
  source(here::here("src/ahrq-ccsr-prep.R"))
  
  # Prepare ICD-10-CM frequency distributions for simulating real ICD-10-CM codes, 2016-2022
  source(here::here("src/icd10-freq-prep.R"))
  
  # Prepare the inputs required from MEPS and simulation contingencies for risk scoring
  source(here::here("src/meps-prep.R"))
  meps_prep(n_dx_sim, 
            sim_only_hcc_dxcodes) 
  
  # Load the file created by simulating NDC contingencies
  load(here::here(paste0("etc/contingencies_n=",n_dx_sim,".rda")))
  
  # Load Reticulate + Python functions for running hccpy RA score models for each individual x ICD10CM profile
  source("src/cms-hcc-run.R")
  
  # One day we'll run HHS-HCC on the commercial population, too - but for now we're just doing Medicare benes
  contingencies_to_run_cmshcc <- contingencies_to_run
  
  # Initialize parallel processing with the number of cores available
  plan(multisession, workers = availableCores())
  
  process_scores <- contingencies_to_run_cmshcc %>% 
    group_by(DUPERSID, meps_year) %>% 
    nest() %>% 
    ungroup() %>% 
    slice_sample(prop=sample_prop) %>% 
    mutate(cms_hcc_run.output = future_map(data, cms_hcc_run, .progress = T,
                                           .options = furrr_options(seed=T)))
  
  # Back to single-core processing
  plan(sequential)
  
  risk_scores <- process_scores %>%
    mutate(risk_scores = map(cms_hcc_run.output, ~pluck(.x, "risk_scores"))) %>%
    select(DUPERSID, meps_year, risk_scores) %>%
    unnest(cols = c(risk_scores))
  
  save(risk_scores, file=here::here("etc/outputs/risk_scores.rda"))
  
  mode_hcc_scores <- process_scores %>%
    mutate(mode_hcc_scores = map(cms_hcc_run.output, ~pluck(.x, "mode_hcc_scores"))) %>%
    select(mode_hcc_scores) %>%
    unnest(cols = c(mode_hcc_scores)) 
  
  save(mode_hcc_scores, file=here::here("etc/outputs/mode_hcc_scores.rda"))
  
  mode_hcc_mapping <- process_scores %>%
    mutate(mode_hcc_mapping = map(cms_hcc_run.output, ~pluck(.x, "mode_hcc_mapping"))) %>%
    select(mode_hcc_mapping) %>%
    unnest(cols = c(mode_hcc_mapping)) 
  
  save(mode_hcc_mapping, file=here::here("etc/outputs/mode_hcc_mapping.rda"))
  
  mode_hcc_values <- process_scores %>%
    mutate(mode_hcc_values = map(cms_hcc_run.output, ~pluck(.x, "mode_hcc_values"))) %>%
    select(mode_hcc_values) %>%
    unnest(cols = c(mode_hcc_values))  
  
  save(mode_hcc_values, file=here::here("etc/outputs/mode_hcc_values.rda"))
  
  # Postprocessing content to one day go here!
  
}

main()
