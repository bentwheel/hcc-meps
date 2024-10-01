# meps-prep.R
# Author: C. Seth Lester, ASA, MAAA
# 20 Jan 2024

## INTRODUCTION 

# This R Script utilizes a custom MEPS package maintained by Emily Mitchell (AHRQ)
# to retrieve MEPS data files that contain key demographic, truncated ICD-10-CM, and 
# NDC inputs on thousands of individual respondent data contained in the MEPS
# survey data PUFs. Data is then prepared and gently "editorialized" for
# processing by the HHS-HCC Risk Score model in a separate R script.

# MEPS Data PUFs:
# https://meps.ahrq.gov/mepsweb/data_stats/download_data_files.jsp

# MEPS R Package on Github:
# https://github.com/e-mitchell/meps_r_pkg

## REQUIRED LIBRARIES
library(tidyverse)
library(cli)
library(furrr)
library(future)
library(haven)

# This must be installed "from scratch" via Github, consider using devtools::install_github()
# More info here: https://github.com/e-mitchell/meps_r_pkg
library(MEPS)

# No scientific notation
options(scipen = 999)

# Function to prepare input files for CMS-HCC processing. 
# Arguments
# - n_dx_sim - Number of DX scenarios based on full frequency of ICD-10 codes.
# - sim_only_hcc_dxcodes - If TRUE, any ICD-10-CM codes truncated to 3 digits from MEPS that 
#     couldn't possibly map to an HCC in v24 or v28 are removed entirely from the simulation process unless
#     they map to a full ICD-10-CM code with probability of 1 based on the ICD-10-CM frequency distribution file.
#     (e.g., all I10. dx codes). This won't impact score outputs, but DX profile simulations will be far more simple.
meps_prep <- function(n_dx_sim = 500, sim_only_hcc_dxcodes = T)
{
  
  ## PREPARE DEMOGRAPHICS INFO
  
  # The HHS-HCC model requires age (via DOB) and gender inputs for each individual.
  # These inputs can be extracted for the individual respondent level in the MEPS
  # Full-Year Consolidated (FYC) data public use files. We'll fetch those now.
  
  meps_years <- list(meps_year = c("2016", "2017", "2018", "2019", "2020", "2021", "2022"))
  fyc_datasets <- NULL
  
  # Function to rename columns
  rename_fields <- function(names) {
    # Iterate over each name and apply the renaming logic
    names <- sapply(names, function(name) {
      # Check if the name matches the pattern
      if (str_detect(name, "^(PRI|MCR|MCD)[A-Z]{2}\\d{2}$")) {
        # Replace the last two digits with "YY"
        name <- str_sub(name, 1, -3) %>% str_c("YY")
      }
      else if (str_detect(name, "^INS[A-Z]{2}\\d{2}X$")) {
        # Replace the last two digits with "YY"
        name <- str_remove(name, "\\d{2}X") %>% str_c("YY")
      }
      return(name)  # Return the modified or original name
    })
    return(names)
  }
  
  for (year in meps_years$meps_year)
  {
    yr2d <- str_sub(year, 3)
    select_variables <- c("DUPERSID", "VARPSU", "VARSTR", paste0("PERWT",yr2d,"F"),
                          paste0("TOTEXP",yr2d), paste0("TOTPRV",yr2d), paste0("TOTMCR",yr2d), 
                          paste0("TOTMCD",yr2d), paste0("TOTSLF",yr2d), 
                          paste0("RXEXP",yr2d), paste0("RXPRV",yr2d), paste0("RXMCR",yr2d), 
                          paste0("RXMCD",yr2d), paste0("RXSLF",yr2d), 
                          "DOBYY", "DOBMM", "SEX")
    
    fyc_data <- MEPS::read_MEPS(type = "FYC", year=year) %>% 
      select(all_of(select_variables), matches("^(PRI|MCR|MCD)(JA|FE|MA|AP|MY|JU|JL|AU|SE|OC|NO|DE)\\d{2}$"),
             matches("^INS(JA|FE|MA|AP|MY|JU|JL|AU|SE|OC|NO|DE)\\d{2}X$")) %>% 
      rename_with(~paste0("PERWTYYF", recycle0=T), starts_with("PERWT")) %>% 
      rename_with(~paste0("TOTEXPYY", recycle0=T), starts_with("TOTEXP")) %>% 
      rename_with(~paste0("TOTPRVYY", recycle0=T), starts_with("TOTPRV")) %>% 
      rename_with(~paste0("TOTMCRYY", recycle0=T), starts_with("TOTMCR")) %>% 
      rename_with(~paste0("TOTMCDYY", recycle0=T), starts_with("TOTMCD")) %>% 
      rename_with(~paste0("TOTSLFYY", recycle0=T), starts_with("TOTSLF")) %>% 
      rename_with(~paste0("RXEXPYY", recycle0=T), starts_with("RXEXP")) %>% 
      rename_with(~paste0("RXPRVYY", recycle0=T), starts_with("RXPRV")) %>% 
      rename_with(~paste0("RXMCRYY", recycle0=T), starts_with("RXMCR")) %>% 
      rename_with(~paste0("RXMCDYY", recycle0=T), starts_with("RXMCD")) %>% 
      rename_with(~paste0("RXSLFYY", recycle0=T), starts_with("RXSLF")) %>% 
      rename_with(.fn = rename_fields) %>% 
      mutate(meps_year = year) %>% 
      relocate(meps_year, .after=DUPERSID)
    
    # If meps_year >= 2018, then we need to remove the first two characters of DUPERSID
    # because it contains the two-digit PANEL number. This will break longitudinal joins
    # later when comparing base year scores to projection year expenditures.
    fyc_data <- fyc_data %>% 
      mutate(DUPERSID = if_else(meps_year >= 2018, str_sub(DUPERSID, start=3L, end=-1L), DUPERSID))
  
    fyc_datasets <- fyc_datasets %>% 
      union_all(fyc_data)
  }
  
  #### Output sample weights and other important survey thingies
  survey_weights <- fyc_datasets %>% 
    select(DUPERSID, meps_year, PERWTYYF, VARPSU, VARSTR) %>% 
    write_csv(here::here("etc/outputs/survey_weights.csv"))
  
  #### Organize Exposure Calculations
  # Determine PMPM Costs by Payor (PRV + MCR + MCD + SLF + OTH = TOT)
  # Determine Exposures by Payor Coverage (PRV + MCR + MCD + UNI (uninsured) + OTH = TOT)
  # Assign Dual Enrollee flags / indicators for Duals
  
  # First prepare a summary of exposure months by cateogry above, plus DUPERSID and meps_year
  fyc_data_exposures <- fyc_datasets %>% 
    select(DUPERSID, meps_year, matches("^(PRI|MCR|MCD|INS)[A-Z]{2}YY$")) %>% 
    pivot_longer(
      cols = -c(DUPERSID,meps_year),
      names_to = c("Type", "Month"),
      names_pattern = "(\\w{3})(\\w{2})",
      values_to = "Enrolled"
    ) %>% 
    mutate(Enrolled = !as.logical(Enrolled - 1)) %>% 
    pivot_wider(names_from = "Type", values_from = "Enrolled") %>% 
    mutate(OTH = (!MCR & !PRI & !MCD & INS),
           UNI = !INS) %>% 
    relocate(OTH, .before=INS) %>% 
    rename(PRV=PRI,
           TOT=INS) %>% 
    pivot_longer(cols = -c(DUPERSID, meps_year, Month),
                 names_to = "Type",
                 values_to = "Enrolled") %>% 
    write_csv("./etc/outputs/exposures_monthly.csv")
  
  # Summarize month-by-month enrollment with a count of expos by member, meps_year, and coverage type
  fyc_data_expos_totals <- fyc_data_exposures %>% 
    group_by(DUPERSID, meps_year, Type) %>% 
    summarize(Expos = sum(Enrolled)) %>% 
    ungroup() %>% 
    write_csv("./etc/outputs/exposures_total.csv")
  
  # Identify all members who are impaneled across multiple years in MEPS AND,
  # for all values of meps_year, who has at least one month of Medicare coverage in meps_year+1?
  # This will be our target list for running the CMS-HCC model.
  mcr_benes_to_score <- fyc_data_expos_totals %>% 
    pivot_wider(names_from=Type,
                values_from=Expos) %>% 
    filter(meps_year != "2016") %>% # Since this is a filtration on payment/projection year, we don't need this
    filter(MCR > 0) %>% 
    distinct(DUPERSID, meps_year) %>% 
    # Reduce the meps_year by 1 so it can be used along with DUPERSID as a filtering inner join
    mutate(meps_year = as.character(as.numeric(meps_year) - 1))  %>% 
    inner_join(fyc_data_expos_totals %>% 
                 distinct(DUPERSID, meps_year)) %>% 
    write_csv("./etc/outputs/mcr_benes.csv")

  mcr_benes_to_score_no_duals <- fyc_data_expos_totals %>% 
    pivot_wider(names_from=Type,
                values_from=Expos) %>% 
    filter(meps_year != "2016") %>% # Since this is a filtration on payment/projection year, we don't need this
    filter(MCR > 0 & MCD == 0) %>% 
    distinct(DUPERSID, meps_year) %>% 
    # Reduce the meps_year by 1 so it can be used along with DUPERSID as a filtering inner join
    mutate(meps_year = as.character(as.numeric(meps_year) - 1))  %>% 
    inner_join(fyc_data_expos_totals %>% 
                 distinct(DUPERSID, meps_year)) %>% 
    write_csv("./etc/outputs/mcr_benes_no_duals.csv")
  
  # Now prepare a summary of expenditures by DUPERSID and meps_year
  fyc_expenditures_totals <- fyc_datasets %>% 
    group_by(DUPERSID, meps_year) %>% 
    summarize(across(
      .cols = matches("^(TOT|RX)(EXP|MCR|MCD|PRV|SLF)YY$"),
      .fns = sum,
      .names = "{.col}"
    )) %>% 
    ungroup() %>% 
    group_by(DUPERSID, meps_year) %>% 
    mutate(TOTOTHYY = max(TOTEXPYY - (TOTMCRYY + TOTPRVYY + TOTMCDYY + TOTSLFYY), 0),
           RXOTHYY = max(RXEXPYY - (RXMCRYY + RXPRVYY + RXMCDYY + RXSLFYY), 0)) %>% 
    ungroup() %>% 
    rename(TOTTOTYY = TOTEXPYY,
           RXTOTYY = RXEXPYY) %>% # Trick to get this category to be labelled "TOT" consistent with the exposures file
    pivot_longer(
      cols = -c(DUPERSID, meps_year),
      names_to = c("TOTorRX", "Type"),
      names_pattern = "(TOT|RX)(\\w{3})YY",
      values_to = "Cost") %>% 
    pivot_wider(
      names_from = TOTorRX,
      values_from = Cost
    ) %>% 
    rename(Cost = TOT,
           CostRX = RX) %>% 
    mutate(Cost_noRX = Cost - CostRX) %>% 
    select(-CostRX) %>%
    write_csv("etc/outputs/expenditures_total.csv")
  
  fyc_data_expos_costs <- fyc_expenditures_totals %>% 
    left_join(fyc_data_expos_totals) %>% 
    mutate(PMPM_Cost = Cost/Expos,
           PMPM_Cost_noRX = Cost_noRX / Expos) %>% 
    write_csv("etc/outputs/expenditures_pmpm.csv", na="")
  
  ### Prepare Demographic Inputs for Risk Adjusters
  # Tidy up variable inputs, randomly generate a actual day of birth (0 - 31 based on DOBMM and DOBYY), 
  # And remove exposure calc and expenditure variables - we don't need those anymore as we have 
  # summarized them in the previous section.
  
  ra_demos <- fyc_datasets %>% 
    inner_join(mcr_benes_to_score_no_duals) %>% # Join against the first filter on MCR exposures > 1 in the payment year
    filter(DOBYY > 0 & DOBMM > 0) %>%  # Can't risk adjust anyone without age info
    mutate(
      sex = case_when(
        SEX == 1 ~ "M",
        SEX == 2 ~ "F",
        .default = "U"),
      DOBYY = as.numeric(DOBYY),
      DOBMM = as.numeric(DOBMM),
      DOB = mdy(paste(month(DOBMM, label=T), 15, DOBYY)),
      AGE_ASOF_DATE = ymd(paste0(as.numeric(meps_year)+1, "0101")),
      AGE_EXACT = (AGE_ASOF_DATE - ymd(DOB)) / dyears(1),
      AGE_CURTATE = as.integer(AGE_EXACT)) %>% 
    select(-SEX, -DOBYY, -DOBMM) %>% 
    select(-matches("^(PRI|MCR|MCD)(JA|FE|MA|AP|MY|JU|JL|AU|SE|OC|NO|DE)\\w{2}$"),
           -matches("^INS(JA|FE|MA|AP|MY|JU|JL|AU|SE|OC|NO|DE)YY$"),
           -matches("TOT\\w{3}YY"),
           -matches("RX\\w{3}YY"),
           -VARSTR, -VARPSU, -PERWTYYF) %>% 
    rename(SEX=sex)  %>% 
    # Finally, apply age filter to try and remove any MCR benes not appropriate for the CNA RA model.
    # This means we want only people who are exact age 65 or greater as of 1/1/YY, where YY is the 
    # year equal to the payment year. To remove benes who are top-coded at age 85, we will also remove
    # any individual explicitly older than 85 as of 1/1/YY. They could be 85, 88, 95, 106 - and this
    # will reduce model performance.
    filter(between(AGE_CURTATE, 65, 84)) %>% 
    write_csv("etc/outputs/demographics.csv")
  
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
                          "CCSR3X", "CCSR4X", "ERCOND", "ERNUM", "IPCOND", "IPNUM", "OPCOND", "OPNUM")
    
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
    
    if(year < 2022) {
      cond_data <- cond_data %>% 
        mutate(CCSR4X = NA_character_) %>% 
        relocate(CCSR4X, .after=CCSR3X)
    }
    
    # For meps_year >= 2018, remove 2 digit panel number from first 2 characters of DUPERSID:
    cond_data <- cond_data %>% 
      mutate(DUPERSID = if_else(meps_year >= 2018, str_sub(DUPERSID, start=3L, end=-1L), DUPERSID))
    
    cond_datasets <- cond_datasets %>% 
      union_all(cond_data)
  }
  
  # NEXT, let's take note of where the population has censored codes - we will label them with a flag
  # indicating that there were censored codes associated with the record, which we'll key by DUPERSID and YEAR. 
  # CCSR codes are also sometimes censored,
  # so we'll flag when those are deleted as well. Don't do any important analysis with this dataset, 
  # it's just for illustrative purposes on how to do analyses!
  
  censored_code_individuals <- cond_datasets %>% 
    filter(ICD10CDX < 0 | (CCSR1X < -1 | CCSR2X < -1 | CCSR3X < -1)) %>% 
    distinct(DUPERSID, meps_year) %>%
    mutate(CensoredDXFlag = T)
  
  # Label these individuals in the conditions file
  cond_datasets_proc <- cond_datasets %>% 
    left_join(censored_code_individuals)
  
  # Now also label them in the demographics-bearing dataset
  ra_demos <- ra_demos %>% 
    left_join(censored_code_individuals) %>% 
    write_csv("etc/outputs/demographics.csv")
  
  # Find members with ESRD so we can remove them from the CNA and NE models later
  esrd_individuals <- cond_datasets_proc %>% 
    inner_join(ra_demos %>% distinct(DUPERSID, meps_year)) %>%  # Filter on eligible benes 
    filter(ICD10CDX == "N18") %>% 
    distinct(DUPERSID, meps_year) %>% 
    write_csv(here::here("etc/outputs/esrd_individuals.csv"))
  
  # Replace all "-1" values in CCSR fields with NAs
  cond_datasets_proc <- cond_datasets_proc %>% 
    mutate(
      CCSR1X = if_else(CCSR1X == "-1", NA_character_, CCSR1X),
      CCSR2X = if_else(CCSR2X == "-1", NA_character_, CCSR2X),
      CCSR3X = if_else(CCSR3X == "-1", NA_character_, CCSR3X),
      CCSR4X = if_else(CCSR4X == "-1", NA_character_, CCSR4X))
  
  
  # We will next build the AHRQ CCSR mapping from ICD10CM codes to CCSR Categories.
  # This is done in the source file "src/ahrq-ccsr-prep.R".
  
  icd10_ccsr4_map <- read_csv("./etc/ahrq_ccsr_maps/icd10_ccsr4_map.csv", 
                              guess_max = 60000) %>% 
    mutate(meps_year = as.character(meps_year))
  
  # NOW we have a complete mapping for all years (2016 - 2022) of ICD10CM codes
  # to CCSR codes and descriptions. Now we have to handle the matter of the missing
  # ICD-10-CM digits, which are all truncated to three-digit codes in the
  # MEPS condition dataset. 
  
  # To solve for this, we'll turn to the state of California, which has
  # open data reporting for ICD-10-CM frequency tables for ED, IP, and OP
  # events. Using these frequency tables, we can form a distribution of complete
  # ICD-10-CM codes based on the truncated codes in MEPS plus the ICD-10-CM code.
  
  # source("./src/icd10-freq-prep.R")
  ca_icd10_freqs <- read_csv("./etc/icd10_freq_tables/icd10_freqs.csv",
                             guess_max = 60000) %>% 
    mutate(meps_year = as.character(meps_year))
  
  # # Get ICD-10 codes that map to CMS-HCC model HCCs for V24 and V28
  hcc_icd_mappings <- read_csv("etc/hcc-icd-mappings.csv")  %>% 
    select(`Diagnosis\nCode`, `CMS-HCC\nModel\nCategory\nV24`, `CMS-HCC\nModel\nCategory\nV28`) %>% 
    rename(ICD10CM = `Diagnosis\nCode`) %>% 
    mutate(CMS_HCC = as.integer(if_any(2:3, ~ !is.na(.)))) %>% 
    mutate(ICD10CDX = str_extract(ICD10CM, "^\\w{1}\\d{2}")) %>% 
    group_by(ICD10CDX) %>% 
    summarize(total = sum(CMS_HCC)) %>% 
    ungroup() %>% 
    mutate(CMS_HCC = total > 0) %>% 
    distinct(ICD10CDX, CMS_HCC)
  
  # Now we'll combine the icd10-cm frequency data with our conditions file
  conds_w_freqs <- cond_datasets_proc %>% 
    left_join(ca_icd10_freqs) %>% 
    inner_join(icd10_ccsr4_map) %>% 
    group_by(DUPERSID, CONDN, ICD10CDX, CCSR1X, CCSR2X, CCSR3X, CCSR4X, meps_year) %>% 
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
    select(DUPERSID, meps_year, ICD10CDX, ICD10CM, use_pct) %>% 
    filter(use_pct > 0)  # functionally some of these probabilities are so small that they are stored as zero - let's get rid of these.
  
  # Are we only using HCC dx codes?
  if(sim_only_hcc_dxcodes) {
    use_pct_thresh <- 1
  } else {
    use_pct_thresh <- .05
  }
  
  #  The set of all combinations for some of these DX arrays is so huge that we need to edit down some of 
  # the non-HCC related diagnoses that are super infrequent. Goal is to remove low-probability DX codes that aren't
  # associated with any change in the CMS-HCC model output. That way we still have a realistic distribution
  # of simulated DX codes based on the 3 digit truncation from MEPS, but we're not altering the probability of
  # sampling any HCC-related probabilities at the 3 digit ICD10 grouping level.
  conds_w_freqs_edited <- conds_w_freqs %>% 
    left_join(hcc_icd_mappings) %>% 
    inner_join(ra_demos) %>% # Filtration join: Don't bother doing this for anyone who isn't at least close to our target pop
    replace_na(list(CMS_HCC = F)) %>% 
    filter(CMS_HCC | use_pct >= !!use_pct_thresh) %>% # Either it's a 3-digit ICD DX with associated HCC's or its got a sample prob over .05
    group_by(DUPERSID, meps_year, ICD10CDX, CMS_HCC) %>% 
    mutate(new_use_pct = use_pct / sum(use_pct)) %>% 
    ungroup() %>% 
    select(-use_pct) %>% 
    rename(use_pct = new_use_pct) 
  

  
  # Now do the primary processing, one individual at a time, getting
  # a risk score for every possible DX code configuration and associated probability
  # with that configuration
  
  # Function to simulate DX code profiles
  simulate_dx_profiles <- function(group, n_sim = n_dx_sim) {
    icd10cm_lists <- group %>% group_by(ICD10CDX) %>% summarise(ICD10CM_list = list(ICD10CM))
    use_pct_lists <- group %>% group_by(ICD10CDX) %>% summarise(use_pct_list = list(use_pct))
    
    simulations <- replicate(n_sim, {
      sample_dx_codes <- map2(icd10cm_lists$ICD10CM_list, use_pct_lists$use_pct_list, ~sample(.x, size = 1, prob = .y))
      paste(unlist(sample_dx_codes), collapse = ", ")
    })
    
    result <- tibble(
      DX_profile = simulations
    ) %>%
      count(DX_profile, name = "count") %>%
      mutate(probability = count / n_sim) 
  
    return(result)
  }
  
  # Initialize parallel processing with the number of cores available
  plan(multisession, workers = availableCores())
  
  # Build at least n_dx_sim simulated combinations of all possible DX codes, sampled based on edited ICD10 code
  # frequency distributions (unedited when associated with any HCCs in the CMS-HCC model)
  make_contingencies <- conds_w_freqs_edited %>% 
    group_by(DUPERSID, meps_year) %>%
    nest() %>% 
    ungroup() %>% 
    mutate(simulate_dx_profiles = future_map(data, simulate_dx_profiles, .progress = T,
                                             .options = furrr_options(seed=T)))
  
  # Back to single-core processing
  plan(sequential)
  
  contingencies_to_run <- make_contingencies %>% 
    select(-data) %>% 
    group_by(DUPERSID, meps_year) %>% 
    unnest(cols = c(simulate_dx_profiles)) %>% 
    ungroup() %>% 
    group_by(DUPERSID, meps_year) %>% 
    mutate(profile_id = row_number(),
           id = str_c(DUPERSID, meps_year, profile_id, sep="-")) %>% 
    ungroup() %>% 
    right_join(ra_demos) %>%  # Limit this exercise to only the fully filtered target population, must be a right join or else we lose anyone without reported conditions
    anti_join(esrd_individuals) %>%  # Remove this group too
    mutate(id = if_else(is.na(id), str_c(DUPERSID, meps_year, 1, sep="-"), id),
           probability = if_else(is.na(probability), 1, probability)) %>% 
    mutate(profile = map(DX_profile, function(x) { strsplit(x, ", ")[[1]] } )) %>% 
    select(DUPERSID, meps_year, id, SEX, AGE_EXACT, profile, probability)
  
  save(contingencies_to_run, file=here::here(paste0("etc/contingencies_n=",n_dx_sim,".rda")))
}

# Testing
# meps_prep(n_dx_sim = 500)

