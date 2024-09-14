# Series of helper functions that are designed to run CMS-HCC scores for a given
# diagnosis code array. 

# Default is to run scores for Community Nondual Aged and New Enrollees, for both
# Duals and Nonduals (4 total scenarios)

# This script uses a pre-established Python venv loaded with necessary packages to 
# run HCC models on datasets using the R Reticulate package (allowing R datasets to
# be passed to Python for processing and the resulting Python data to be passed back
# to R).

library(tidyverse)
library(reticulate)

# In the R project file, or through the RStudio Menu > Tools > Project/Global Settings,
# make sure that "Automatically activate project-local Python virtual environments"
# is selected!

cms_hcc_run <- function(data) {
  
  # For testing/debug
  # data <- test_record
  
  # Create venv if it does not exist
  if (!virtualenv_exists("hccpy-venv")) {
    virtualenv_create("hccpy-venv")
    
    # Install necessary Python packages in this virtual environment, if necessary
    py_install(packages = c("hccpy", "pandas"))
  }

  use_virtualenv("hccpy-venv")
  
  # This should point to a python bin within a local userspace venv
  # py_config()
  
  # Arguments for HCC model to build cartesian cross join with inputs
  elig_args <- c("CFA","CNA","NE") %>% 
    as_tibble() %>% 
    rename(elig_args = value) 
  
  # Original entitlement args - only run scores as if entitlement was by age-in or disability, ignore ESRD for now.
  orec_args <- c(0) %>% 
    as_tibble() %>% 
    rename(orec_args = value)
  
  # Medicaid bool values for HCC model
  mcaid_args <- c(T,F) %>% 
    as_tibble() %>% 
    rename(mcaid_args = value)
  
  # Cross product of everything - default settings are four runs
  cross_all_args <- elig_args %>% 
    cross_join(orec_args) %>% 
    cross_join(mcaid_args) %>% 
    mutate(run_index = row_number())
  
  # Make cms-hcc input dataset
  cmshcc_input_data <- data %>% 
    cross_join(cross_all_args) 
  
  # Convert R data frame to a pandas DataFrame in Python
  py_cmshcc_input_data <- r_to_py(cmshcc_input_data) 
  
  # Define a Python function to process the data
  cms_hcc_run <- py_run_string("
import pandas as pd
import numpy as np
import json
from hccpy.hcc import HCCEngine

def cms_hcc_run(input_data, ver):
    he = HCCEngine(version=ver)

    # Convert the merged DataFrame to a dictionary
    input_data['pid_runid'] = list(zip(input_data['id'], input_data['run_index']))

    profile_dict = dict(zip(input_data['pid_runid'], zip(input_data['profile'], input_data['AGE_EXACT'], input_data['SEX'], input_data['elig_args'], input_data['orec_args'], input_data['mcaid_args'])))

    # Prepare a list to store the results
    results = []
    
    # Iterate over the dictionary and call 'he.profile()' for each entry
    for (id, runid), (codes, age, sex, elig, orec, mcaid) in profile_dict.items():
      rp = he.profile(codes, age=age, sex=sex, elig=elig, orec=orec, medicaid=mcaid)
      # for debugging:
      # print('Running: ', id, codes, age, sex, elig, orec, mcaid)
    
      # Convert the result to a JSON string
      rp_json_str = json.dumps(rp)
  
      # Append the ID and the JSON string to the results list
      results.append({'ID': id, 'Run_ID': runid, 'CMS-HCC': rp_json_str})

    # Create a DataFrame from the results
    results_df = pd.DataFrame(results)
    
    return results_df
")
  
  processed_data_24 <- cms_hcc_run$cms_hcc_run(py_cmshcc_input_data, "24") 
  processed_data_28 <- cms_hcc_run$cms_hcc_run(py_cmshcc_input_data, "28") 
  
  probabilities <- data %>% 
    distinct(id, probability)

  processed_data <- processed_data_24 %>% 
    union_all(processed_data_28) %>% 
    left_join(probabilities, by=c("ID"="id"))
  
  intermediate <- processed_data %>% 
    arrange(ID, Run_ID) %>% 
    mutate(model_out = map(`CMS-HCC`, parse_json)) %>% 
    unnest_wider(model_out) %>% 
    left_join(cross_all_args, by=c("Run_ID"="run_index")) 
  
  risk_scores_out <- intermediate %>% 
    group_by(Run_ID, model, orec_args, elig_args, mcaid_args) %>% 
    summarize(risk_score = weighted.mean(risk_score, w=probability),
              risk_score_age = weighted.mean(risk_score_age, w=probability),
              risk_score_adj = weighted.mean(risk_score_adj, w=probability), 
              risk_score_age_adj = weighted.mean(risk_score_age_adj, w=probability),
              .groups="keep")
  
  trials_mode <- intermediate %>% 
    separate_wider_delim("ID", names=c("DUPERSID", "meps_year", "profid1"), delim="-",
                         cols_remove=F) %>%
    group_by(DUPERSID, meps_year, Run_ID, model, orec_args, elig_args, mcaid_args) %>% 
    slice_max(order_by = probability, n = 1, with_ties=F) %>% 
    ungroup()
  
  mode_hcc_scores <- trials_mode %>% 
    select(DUPERSID, meps_year, Run_ID, model, orec_args, elig_args, mcaid_args, risk_score, risk_score_age, risk_score_adj,
           risk_score_age_adj)
  
  mode_hcc_mapping <- trials_mode %>% 
    select(DUPERSID, meps_year, Run_ID, model, orec_args, elig_args, mcaid_args, hcc_map) %>% 
    mutate(hcc_map_dx = map(hcc_map, names)) %>% 
    unnest(hcc_map_dx) %>% 
    mutate(hcc_map_hcc = map2(hcc_map, hcc_map_dx, ~pluck(.x, .y))) %>% 
    select(-hcc_map)
  
  mode_hcc_values <- trials_mode %>% 
    select(DUPERSID, meps_year, Run_ID, model, orec_args, elig_args, mcaid_args, details) %>% 
    mutate(details_name = map(details, names)) %>% 
    unnest(details_name) %>% 
    mutate(details_value = map2(details, details_name, ~pluck(.x, .y))) %>% 
    select(-details)

  scoremodel.out <- list(risk_scores = risk_scores_out,
                         mode_hcc_scores = mode_hcc_scores,
                         mode_hcc_mapping = mode_hcc_mapping,
                         mode_hcc_values = mode_hcc_values)
  
  scoremodel.out
  
}

parse_json <- function(json_str) {
  # Check if the string is valid JSON
  if (startsWith(trimws(json_str), "{") && endsWith(trimws(json_str), "}")) {
    tryCatch(
      jsonlite::fromJSON(json_str),
      error = function(e) NA
    )
  } else {
    NA
  }
}

