# hcc-meps
Process to apply HHS-HCC model scores to MEPS respondent data from 2016 - 2021

# Overview & Purpose

# Preparation of MEPS Data Sources

# HCC Models Processed

# Comorbidity Models Processed

# Outputs

risk_scores - average risk score for each set of DX combination risk scores grouped by DUPERSID, model, Run_ID, and meps_year

mode_dx - readout of DX code combination modes based on trial sampling, grouped by DUPERSID, model, Run_ID, and meps_year

mode_scores_cmshcc - risk score for the most commonly simulated DX code combination (the one represented in mode_dx), grouped by DUPERSID, model, Run_ID, and meps_year

mode_mapping_cmshcc - mapping of DX codes in mode_dx to HCCs in the CMS-HCC model, grouped by DUPERSID, model, Run_ID, and meps_year

mode_values_cmshcc - mapping of HCCs in mode_mapping_cmshcc to score components in the CMS_HCC model, grouped by DUPERSID, model, Run_ID, and meps_year

# Process Flow Overview

meps-prep.R = load files from MEPS and preprocess for HCC and elixhauser score models
ahrq-ccsr-prep.R = load CCSR crosswalks and appropriately crossmap ICD10CM codes to CCSR
icd10-freq-prep.R = load ICD10CM frequency files from the state of California
cms-hcc-run.R = Load supporting functions to load the Python venv to run CMS-HCC models via hccpy package
hhs-hcc-run.R = Load supporting functions to load the Python venv to run HHS-HCC models via hccpy package
elix.R = Load supporting functions to load the Python 




