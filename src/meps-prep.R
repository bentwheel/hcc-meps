# meps-prep.R
# Author: C. Seth Lester, ASA, MAAA
# 20 Jan 2024

## INTRODUCTION 

# This R Script utilizes a custom MEPS package maintained by Emily Mitchell
# to retrieve MEPS data files that contain key demographic, ICD-10-CM, and 
# NDC inputs on thousands of individual respondent data contained in the MEPS
# survey data PUFs. Data is then prepared and gently "editorialized" for
# processing by the HHS-HCC Risk Score model in a separate R script.

# MEPS Data PUFs:
# https://meps.ahrq.gov/mepsweb/data_stats/download_data_files.jsp

# MEPS R Package on Github:
# https://github.com/e-mitchell/meps_r_pkg

## REQUIRED LIBRARIES
library(tidyverse)

# This must be installed "from scratch" via Github, consider using devtools::install_github()
library(MEPS)

## PREPARE DEMOGRAPHICS INFO

# The HHS-HCC model requires age (via DOB) and gender inputs for each individual.
# These inputs can be extracted for the individual respondent level in the MEPS
# Full-Year Consolidated (FYC) data public use files. We'll fetch those now.

meps_years <- list(meps_year = c("2016", "2017", "2018", "2019", "2020", "2021"))
fyc_datasets <- NULL

for (year in meps_years$meps_year)
{
  yr2d <- str_sub(year, 3)
  select_variables <- c("DUPERSID", "VARPSU", "VARSTR", paste0("PERWT",yr2d,"F"),
                        "DOBYY", "DOBMM", "SEX")
  
  fyc_data <- MEPS::read_MEPS(type = "FYC", year=year) %>% 
    select(all_of(select_variables)) %>% 
    rename_with(~paste0("PERWTYYF"), starts_with("PERWT")) %>% 
    mutate(meps_year = year) %>% 
    filter(DOBYY > 0, DOBMM > 0)  # can't risk score these folks!

  fyc_datasets <- fyc_datasets %>% 
    union_all(fyc_data)
}

# Tidy up variable inputs, randomly generate a actual day of birth (0 - 31 based on DOBMM and DOBYY)
fyc_data_proc <- fyc_datasets  %>% 
  mutate(
    sex = case_when(
      SEX == 1 ~ "M",
      SEX == 2 ~ "F",
      .default = as.character(SEX)),
    DOBYY = as.numeric(DOBYY),
    DOBMM = as.numeric(DOBMM),
    DOBDD_max = days_in_month(mdy(paste(month(DOBMM, label=T), 1, DOBYY))),
    DOBDD = map_int(DOBDD_max, ~floor(runif(1, 1, .x+1))),
    DOB = mdy(paste(month(DOBMM, label=T), DOBDD, DOBYY)),
    AGE_ASOF_DATE = ymd(paste0(meps_year, "1231")),
    AGE_EXACT = (AGE_ASOF_DATE - ymd(DOB)) / dyears(1)) %>% 
  select(-SEX, -DOBYY, -DOBMM, -DOBDD, -DOBDD_max) %>% 
  rename(SEX=sex) 

## PREPARE DIAGNOSIS INFO

# Now it's time to get a list of ICD-10-CM codes for each individual respondent.
# MEPS truncates all ICD-10-CM codes to three digits only, and further censors
# codes for rare or unique diseases. In an attempt to remove the influence of
# censorship on any analysis of the final dataset once risk scored, we will remove
# all individuals with censored condition IDs.

# First, let's fetch the necessary data from MEPS.

cond_datasets <- NULL

for (year in meps_years$meps_year)
{
  select_variables <- c("DUPERSID", "CONDN", "ICD10CDX", "CCSR1X", "CCSR2X", 
                        "CCSR3X", "ERCOND", "ERNUM", "IPCOND", "IPNUM", "OPCOND", "OPNUM")
  
  cond_data <- MEPS::read_MEPS(type = "COND", year=year) %>% 
    select(any_of(select_variables)) %>% 
    mutate(meps_year = year)
  
  if(as.numeric(year) < 2021) {
    cond_data <- cond_data %>% 
      mutate(
        ERCOND = case_when(
          ERNUM > 0 ~ 1,
          ERNUM == 0 ~ 2,
          .default = ERNUM),
        IPCOND = case_when(
          IPNUM > 0 ~ 1,
          IPNUM == 0 ~ 2,
          .default = IPNUM),
        OPCOND = case_when(
          OPNUM > 0 ~ 1,
          OPNUM == 0 ~ 2,
          .default = OPNUM)) %>% 
      select(-ERNUM, -IPNUM, -OPNUM)
  }
  
  cond_datasets <- cond_datasets %>% 
    union_all(cond_data)
}

# NEXT, let's take note of where the population has censored codes - we want to remove
# any censored individuals from our analysis, ideally. CCSR codes are also sometimes censored,
# so we'll remove those as well. Don't do any important analysis with this dataset, 
# it's just for illustrative purposes!

censored_code_individuals <- cond_datasets %>% 
  filter(ICD10CDX < 0 | CCSR1X < -1 | CCSR2X < -1 | CCSR3X < -1) %>% 
  distinct(DUPERSID, meps_year) 

# Remove these individuals from the conditions file
cond_datasets_proc <- cond_datasets %>% 
  anti_join(censored_code_individuals)

# Now also remove them from the demographics-bearing dataset
fyc_data_proc <- fyc_data_proc %>% 
  anti_join(censored_code_individuals)

# Replace all "-1" values in CCSR fields with NAs
cond_datasets_proc <- cond_datasets_proc %>% 
  mutate(
    CCSR1X = if_else(CCSR1X == "-1", NA_character_, CCSR1X),
    CCSR2X = if_else(CCSR2X == "-1", NA_character_, CCSR2X),
    CCSR3X = if_else(CCSR3X == "-1", NA_character_, CCSR3X))

# We will next build the AHRQ CCSR mapping from ICD10CM codes to CCSR Categories.
# This is done in the source file "src/ahrq-ccsr-prep.R".
source("./src/ahrq-ccsr-prep.R")
icd10_ccsr3_map <- read_csv("./etc/ahrq_ccsr_maps/icd10_ccsr3_map.csv") %>% 
  mutate(meps_year = as.character(meps_year))

# NOW we have a complete mapping for all years (2016 - 2021) of ICD10CM codes
# to CCSR codes and descriptions. Now we have to handle the matter of the missing
# ICD-10-CM digits, which are all truncated to three-digit codes in the
# MEPS condition dataset. 

# To solve for this, we'll turn to the state of California, which has
# open data reporting for ICD-10-CM frequency tables for ED, IP, and OP
# events. Using these frequency tables, we can form a distribution of complete
# ICD-10-CM codes based on the truncated codes in MEPS plus the ICD-10-CM code.

source("./src/icd10-freq-prep.R")
ca_icd10_freqs <- read_csv("./etc/icd10_freq_tables/icd10_freqs.csv") %>% 
  mutate(meps_year = as.character(meps_year))

# Now we'll combine the icd10-cm frequency data with our conditions file
conds_w_freqs <- cond_datasets_proc %>% 
  left_join(ca_icd10_freqs) %>% 
  inner_join(icd10_ccsr3_map) %>% 
  group_by(DUPERSID, CONDN, ICD10CDX, CCSR1X, CCSR2X, CCSR3X, meps_year) %>% 
  mutate(ed_pct = ed_freq / sum(ed_freq),
         ip_pct = ip_freq / sum(ip_freq),
         op_pct = op_freq / sum(op_freq),
         total_pct = total_freq / sum(total_freq)) %>% 
  ungroup() %>% 
  mutate(
    use_pct = case_when(
      ERCOND == 1 ~ ed_pct,
      IPCOND == 1 ~ ip_pct,
      OPCOND == 1 ~ op_pct,
      .default = total_pct
    )) %>% 
  select(DUPERSID, meps_year, ICD10CDX, ICD10CM, use_pct) 

# Next we will pick a number of simulations to run relative to the size
# of the sample weighting assigned to the MEPS respondent.
# This means n draws
# for how each 3-digit ICD code is completed based on our frequency distr.

# Get sample weights and rescale - this will inform the number of trials
meps_weights <- fyc_data_proc %>% 
  select(DUPERSID, meps_year, PERWTYYF) %>% 
  mutate(POOLWTYYF = PERWTYYF / 6) %>% 
  select(-PERWTYYF)

# Perform 1 million trials of different 
ipw_scaler <- 1e6/sum(meps_weights$POOLWTYYF)

prof_generator <- function(data, trials) {
  trials_out <- list(trial = 1:trials) %>% 
    as_tibble() %>% 
    cross_join(data) %>% 
    group_by(trial, ICD10CDX) %>% 
    mutate(
      rand = runif(1, min=0, max=1),
      cpd = cumsum(use_pct),
      lag_cpd = lag(cpd),
      cpd_lo = if_else(is.na(lag_cpd), 0, lag_cpd),
      cpd_hi = cpd,
      selected = between(rand, cpd_lo, cpd_hi)) %>% 
    ungroup() %>% 
    filter(selected) %>% 
    group_by(trial) %>% 
    summarize(profile = list(ICD10CM)) %>% 
    ungroup() %>% 
    group_by(profile) %>% 
    summarize(freq = n()) %>% 
    ungroup()
  
  trials_out
}

# Finally assemble all ICD-10-CM code lists, and give a frequency distribution
# for each individual to determine how many unique ICD-10-CM profiles to run

consolidated_conds_probs <- conds_w_freqs %>% 
  group_by(DUPERSID, meps_year) %>% 
  nest() %>% 
  ungroup() %>% 
  left_join(meps_weights) %>% 
  filter(POOLWTYYF > 0) %>% 
  mutate(trials = ceiling(POOLWTYYF * ipw_scaler)) %>% 
  mutate(results = map2(data, trials, prof_generator, .progress=T)) %>% 
  select(DUPERSID, meps_year, results) %>% 
  unnest(results)

#### DRUGS!

# Drugs should be pretty easy, we just need a list of NDCs for each individual.

# First, let's fetch the necessary data from MEPS.

rx_datasets <- NULL

for (year in meps_years$meps_year)
{
  select_variables <- c("DUPERSID", "RXNDC")
  
  rx_data <- MEPS::read_MEPS(type = "RX", year=year) %>% 
    select(all_of(select_variables)) %>% 
    mutate(meps_year = year)
  
  rx_datasets <- rx_datasets %>% 
    union_all(rx_data)
}

# Now, let's go through and identify any respondents who have been censored, so we can remove them
# from this exercise.

censored_rx_individuals <- rx_datasets %>% 
  filter(RXNDC < 0) %>% 
  distinct(DUPERSID, meps_year) 

# Remove these individuals from the RX file, also consolidate the NDC codes 
# into a list item for each individual and meps_year
rx_datasets_proc <- rx_datasets %>% 
  anti_join(censored_rx_individuals) %>% 
  group_by(DUPERSID, meps_year) %>% 
  summarize(rx_list = list(RXNDC)) %>% 
  ungroup()

# Now also remove them from the demographics-bearing dataset
fyc_data_proc <- fyc_data_proc %>% 
  anti_join(censored_rx_individuals)

### FINAL dataset

# To risk score this population, we will need a unique ID made out of MEPS ID and meps_year,
# plus demographics (age/sex), an RX list, and any number of potential profile trials for the 
# possible health conditions, so we can get a final distribution for the risk score outputs
# for each individual.

final_rx_data <- rx_datasets_proc %>% 
  unite(col=id, DUPERSID, meps_year, sep="-")

final_dx_data <- consolidated_conds_probs %>% 
  unite(col=id, DUPERSID, meps_year, sep="-")

final_input_data <- fyc_data_proc %>% 
  select(DUPERSID, meps_year, SEX, AGE_EXACT) %>%
  unite(col=id, DUPERSID, meps_year, sep="-") %>% 
  left_join(final_rx_data) %>% 
  left_join(final_dx_data) %>% 
  replace(.=="NULL", NA)  %>%  # replace any NULLs with NA
  replace_na(list(freq=1))

# Lastly, write this out as a R data file for input into our HHS-HCC risk score project. 
final_input_data %>% save(file="./etc/meps_hcc_model_inputs.RData")
