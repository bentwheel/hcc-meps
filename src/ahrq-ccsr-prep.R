# ahrq-ccsr-prep.R

# To help narrow down the distribution of non-truncated ICD-10-CM codes that can
# potentially be associated with a 3-digit ICD-10-CM code, we will use the 
# AHRQ's CCSR diagnosis classification system. CCSR is essentially a grouper that
# buckets ICD-10-CM codes into less granular clinically meaningful categories.

# Once this is done, we need to crosswalk CCSR IDs to each record, as this
# will help us stochastically "complete" the ICD-10-CM codes to full values.
# To do this, we need to populate the etc/ahrq_ccsr_maps directory with AHRQ's
# CCSR mapping CSV files, which you can download here:
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

# Now let's read in all the CCSR crossmaps and apply them to our 
# 2016 - 2021 conditions dataset.

library(tidyverse)

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

# Write out CSV file of our custom crossmap for meps years 2016 - 2021
icd10_ccsr3_map %>% write_csv("./etc/ahrq_ccsr_maps/icd10_ccsr3_map.csv")
