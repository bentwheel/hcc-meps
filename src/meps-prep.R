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

# For this next step, we need to crosswalk CCSR IDs to each record, as this
# will help us stochastically "complete" the ICD-10-CM codes to full values.
# To do this, we need to populate the etc/AHRQ_CCSR_MAPS with AHRQ's
# CCSR mapping files, which you can download here:
# https://hcup-us.ahrq.gov/toolssoftware/ccsr/ccsr_archive.jsp#ccsr

# Note that the CCSR crossmaps come in different versions. The MEPS documentation
# for each CONDITIONS file specifies the version of CCSR to apply to which year.
# In some cases, the MEPS team applied later versions of CCSR maps retroactively
# to ensure consistency. Here are the CCSR versions you will need to download,
# unzip, and place in this folder (exactly as named):

# Data Year | CCSR Version (Expected Filename)
# --------------------------------------------
#  2016     | v2019.1 (DXCCSR2019_1.CSV)
#  2017     | v2019.1 (DXCCSR2019_1.CSV)
#  2018     | v2019.1 (DXCCSR2019_1.CSV)
#  2019     | v2020.3 (DXCCSR_v2020-3.CSV)
#  2020     | v2021.2 (DXCCSR_v2021-2.csv)
#  2021     | v2022.1 (DXCCSR_v2022-1.CSV)

# Replace all "-1" values in CCSR fields with NAs
cond_datasets_proc <- cond_datasets_proc %>% 
  mutate(
    CCSR1X = if_else(CCSR1X == "-1", NA_character_, CCSR1X),
    CCSR2X = if_else(CCSR2X == "-1", NA_character_, CCSR2X),
    CCSR3X = if_else(CCSR3X == "-1", NA_character_, CCSR3X))

# Now let's read in all the CCSR crossmaps and apply them to our 
# 2016 - 2021 conditions dataset.

icd10_ccsr3_map <- NULL

# Parse v2019.1 CCSR
input <- str_replace_all(read_file("./etc/ahrq_ccsr_maps/DXCCSR2019_1.CSV"), "\'", "")
icd10_ccsr_map_2019.1 <- read_csv(input, guess_max = 60000) 

icd10_ccsr3_map_2019.1 <- icd10_ccsr_map_2019.1 %>% 
  select(ICD10CM = `ICD-10-CM CODE`,
         ICD10_DSC = `ICD-10-CM CODE DESCRIPTION`,
         CCSR1X = `CCSR CATEGORY 1`,
         CCSR1X_DSC = `CCSR CATEGORY 1 DESCRIPTION`,
         CCSR2X = `CCSR CATEGORY 2`,
         CCSR2X_DSC = `CCSR CATEGORY 2 DESCRIPTION`,
         CCSR3X = `CCSR CATEGORY 3`,
         CCSR3X_DSC = `CCSR CATEGORY 3 DESCRIPTION`)

# Apply this crosswalk to meps years 2016 - 2018
icd10_ccsr3_map <- list(meps_year = c("2016", "2017", "2018")) %>% 
  as_tibble() %>% 
  cross_join(icd10_ccsr3_map_2019.1)

# Tidy up
rm(icd10_ccsr_map_2019.1)
rm(icd10_ccsr3_map_2019.1)

# Parse v2020.3 CCSR and apply to year 2019
input <- str_replace_all(read_file("./etc/ahrq_ccsr_maps/DXCCSR_v2020-3.CSV"), "\'", "")
icd10_ccsr_map_2020.3 <- read_csv(input, guess_max = 60000) 

icd10_ccsr3_map_2020.3 <- icd10_ccsr_map_2020.3 %>% 
  select(ICD10CM = `ICD-10-CM CODE`,
         ICD10_DSC = `ICD-10-CM CODE DESCRIPTION`,
         CCSR1X = `CCSR CATEGORY 1`,
         CCSR1X_DSC = `CCSR CATEGORY 1 DESCRIPTION`,
         CCSR2X = `CCSR CATEGORY 2`,
         CCSR2X_DSC = `CCSR CATEGORY 2 DESCRIPTION`,
         CCSR3X = `CCSR CATEGORY 3`,
         CCSR3X_DSC = `CCSR CATEGORY 3 DESCRIPTION`) %>% 
  mutate(meps_year = "2019")

icd10_ccsr3_map <- icd10_ccsr3_map %>% 
  union_all(icd10_ccsr3_map_2020.3)

# Tidy up
rm(icd10_ccsr_map_2020.3)
rm(icd10_ccsr3_map_2020.3)

# Parse v2021.2 CCSR, apply to year 2020
input <- str_replace_all(read_file("./etc/ahrq_ccsr_maps/DXCCSR_v2021-2.csv"), "\'", "")
icd10_ccsr_map_2021.2 <- read_csv(input, guess_max = 60000) 

icd10_ccsr3_map_2021.2 <- icd10_ccsr_map_2021.2 %>% 
  select(ICD10CM = `ICD-10-CM CODE`,
         ICD10_DSC = `ICD-10-CM CODE DESCRIPTION`,
         CCSR1X = `CCSR CATEGORY 1`,
         CCSR1X_DSC = `CCSR CATEGORY 1 DESCRIPTION`,
         CCSR2X = `CCSR CATEGORY 2`,
         CCSR2X_DSC = `CCSR CATEGORY 2 DESCRIPTION`,
         CCSR3X = `CCSR CATEGORY 3`,
         CCSR3X_DSC = `CCSR CATEGORY 3 DESCRIPTION`) %>% 
  mutate(meps_year = "2020")

icd10_ccsr3_map <- icd10_ccsr3_map %>% 
  union_all(icd10_ccsr3_map_2021.2)

# Tidy up
rm(icd10_ccsr_map_2021.2)
rm(icd10_ccsr3_map_2021.2)

# Parse v2022.1 CCSR 
input <- str_replace_all(read_file("./etc/ahrq_ccsr_maps/DXCCSR_v2022-1.CSV"), "\'", "")
icd10_ccsr_map_2022.1 <- read_csv(input, guess_max = 60000) 

icd10_ccsr3_map_2022.1 <- icd10_ccsr_map_2022.1 %>% 
  select(ICD10CM = `ICD-10-CM CODE`,
         ICD10_DSC = `ICD-10-CM CODE DESCRIPTION`,
         CCSR1X = `CCSR CATEGORY 1`,
         CCSR1X_DSC = `CCSR CATEGORY 1 DESCRIPTION`,
         CCSR2X = `CCSR CATEGORY 2`,
         CCSR2X_DSC = `CCSR CATEGORY 2 DESCRIPTION`,
         CCSR3X = `CCSR CATEGORY 3`,
         CCSR3X_DSC = `CCSR CATEGORY 3 DESCRIPTION`) %>% 
  mutate(meps_year = "2021")

icd10_ccsr3_map <- icd10_ccsr3_map %>% 
  union_all(icd10_ccsr3_map_2022.1)

# Tidy up
rm(icd10_ccsr_map_2022.1)
rm(icd10_ccsr3_map_2022.1)
gc()

# NOW we have a complete mapping for all years (2016 - 2021) of ICD10CM codes
# to CCSR codes and descriptions. Now we have to handle the matter of the missing
# ICD-10-CM digits, which are all truncated to three-digit codes in the
# MEPS condition dataset. 

# To solve for this, we'll turn to the state of California, which has
# open data reporting for ICD-10-CM frequency tables for ED, IP, and OP
# events. Using these frequency tables, we can form a distribution of complete
# ICD-10-CM codes based on the truncated codes in MEPS plus the ICD-10-CM code.


