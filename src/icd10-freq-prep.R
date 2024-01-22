# icd10-freq-prep.R

# This R script parses and extracts open data files from California
# in order to construct ED, IP, and OP icd-10-cm frequency tables for
# each year, 2016 - 2021. 

# These tables are used to build a distribution of complete
# ICD-10-CM codes based on the truncated codes in MEPS plus the ICD-10-CM code.

# You will need to populate the directory "etc/icd10_freq_tables" with the
# 6 crossmaps for each years 2016 - 2021 from each of the three sections below,
# making 18 tables in total.

# ED frequency tables:
# https://data.chhs.ca.gov/dataset/hospital-emergency-department-diagnosis-procedure-and-external-cause-codes

# IP frequency tables:
# https://data.chhs.ca.gov/dataset/hospital-inpatient-diagnosis-procedure-and-external-cause-codes

# OP frequency tables:
# https://data.chhs.ca.gov/dataset/ambulatory-surgery-diagnosis-procedure-and-external-cause-codes

# The 18 tables should be downloaded as XLSX files and placed in the project's 
# ./etc/icd10_freq_tables/ directory before continuing.

library(tidyverse)
library(readxl)

ca_icd10_freqs_ed <- NULL
ca_icd10_freqs_ip <- NULL
ca_icd10_freqs_op <- NULL

# Parse ED files
ca_icd10_freqs_ed_2021 <- 
  read_xlsx(
    path="./etc/icd10_freq_tables/2021-hospital-emergency-department-diagnosis-code-frequency.xlsx",
    sheet="Diagnosis Code Frequencies 2021") %>% 
  mutate(
    ICD10CM = str_remove(ICDCMCode, "[:punct:]"),
    ICD10CDX = str_sub(ICD10CM, 1, 3),
    meps_year = 2021)

ca_icd10_freqs_ed_2020 <- 
  read_xlsx(
    path="./etc/icd10_freq_tables/2020-hospital-emergency-department-diagnosis-code-frequency.xlsx",
    sheet="Diagnosis Code Frequencies 2020") %>% 
  mutate(
    ICD10CM = str_remove(ICDCMCode, "[:punct:]"),
    ICD10CDX = str_sub(ICD10CM, 1, 3),
    meps_year = 2020)

ca_icd10_freqs_ed_2019 <- 
  read_xlsx(
    path="./etc/icd10_freq_tables/2019-hospital-emergency-department-diagnosis-code-frequency.xlsx",
    sheet="Diagnosis Code Frequencies 2019") %>% 
  select(-PrimaryDiag, -SecondDiag) %>% 
  mutate(
    ICD10CM = str_remove(ICDCMCode, "[:punct:]"),
    ICD10CDX = str_sub(ICD10CM, 1, 3),
    meps_year = 2019) 

ca_icd10_freqs_ed_2018 <- 
  read_xlsx(
    path="./etc/icd10_freq_tables/2018-hospital-emergency-department-diagnosis-code-frequency.xlsx",
    sheet="Dx Code Frequencies 2018") %>% 
  select(-PrimaryDiag, -SecondDiag) %>% 
  rename(ICDCMCode=ICD10CMCode) %>% 
  mutate(
    ICD10CM = str_remove(ICDCMCode, "[:punct:]"),
    ICD10CDX = str_sub(ICD10CM, 1, 3),
    meps_year = 2018) 

ca_icd10_freqs_ed_2017 <- 
  read_xlsx(
    path="./etc/icd10_freq_tables/2017-hospital-emergency-department-diagnosis-code-frequency.xlsx",
    sheet="Dx Code Frequencies 2017") %>% 
  select(-PrimaryDiag, -SecondDiag) %>% 
  rename(ICDCMCode=ICD10CMCode) %>% 
  mutate(
    ICD10CM = str_remove(ICDCMCode, "[:punct:]"),
    ICD10CDX = str_sub(ICD10CM, 1, 3),
    meps_year = 2017) 

ca_icd10_freqs_ed_2016 <- 
  read_xlsx(
    path="./etc/icd10_freq_tables/2016-hospital-emergency-department-diagnosis-code-frequency.xlsx",
    sheet="Dx Code Frequencies 2016") %>% 
  select(-PrimaryDiag, -SecondDiag) %>% 
  rename(ICDCMCode=ICD10CMCode) %>% 
  mutate(
    ICD10CM = str_remove(ICDCMCode, "[:punct:]"),
    ICD10CDX = str_sub(ICD10CM, 1, 3),
    meps_year = 2016) 

# Union them all
ca_icd10_freqs_ed <- ca_icd10_freqs_ed_2016 %>% 
  union_all(ca_icd10_freqs_ed_2017) %>% 
  union_all(ca_icd10_freqs_ed_2018) %>% 
  union_all(ca_icd10_freqs_ed_2019) %>% 
  union_all(ca_icd10_freqs_ed_2020) %>% 
  union_all(ca_icd10_freqs_ed_2021) %>% 
  mutate(type = "ed_freq")

# Cleanup
rm(ca_icd10_freqs_ed_2016)
rm(ca_icd10_freqs_ed_2017)
rm(ca_icd10_freqs_ed_2018)
rm(ca_icd10_freqs_ed_2019)
rm(ca_icd10_freqs_ed_2020)
rm(ca_icd10_freqs_ed_2021)

# Parse IP files
ca_icd10_freqs_ip_2021 <- 
  read_xlsx(
    path="./etc/icd10_freq_tables/2021-hospital-inpatient-diagnosis-code-frequency.xlsx",
    sheet="ICD-10-CM") %>% 
  mutate(
    ICD10CM = str_remove(ICDCMCode, "[:punct:]"),
    ICD10CDX = str_sub(ICD10CM, 1, 3),
    meps_year = 2021)

ca_icd10_freqs_ip_2020 <- 
  read_xlsx(
    path="./etc/icd10_freq_tables/2020-hospital-inpatient-diagnosis-code-frequency.xlsx",
    sheet="ICD-10-CM") %>% 
  mutate(
    ICD10CM = str_remove(ICDCMCode, "[:punct:]"),
    ICD10CDX = str_sub(ICD10CM, 1, 3),
    meps_year = 2020)

ca_icd10_freqs_ip_2019 <- 
  read_xlsx(
    path="./etc/icd10_freq_tables/2019-hospital-inpatient-diagnosis-code-frequency.xlsx",
    sheet="ICD-10-CM") %>% 
  select(-PrimaryDiag, -SecondDiag) %>%
  mutate(
    ICD10CM = str_remove(ICDCMCode, "[:punct:]"),
    ICD10CDX = str_sub(ICD10CM, 1, 3),
    meps_year = 2019)

ca_icd10_freqs_ip_2018 <- 
  read_xlsx(
    path="./etc/icd10_freq_tables/2018-hospital-inpatient-diagnosis-code-frequency.xlsx",
    sheet="ICD-10-CM") %>% 
  select(-PrimaryDiag, -SecondDiag)%>%
  rename(ICDCMCode=ICD10CMCODE) %>% 
  mutate(
    ICD10CM = str_remove(ICDCMCode, "[:punct:]"),
    ICD10CDX = str_sub(ICD10CM, 1, 3),
    meps_year = 2018)

ca_icd10_freqs_ip_2017 <- 
  read_xlsx(
    path="./etc/icd10_freq_tables/2017-hospital-inpatient-diagnosis-code-frequency.xlsx",
    sheet="ICD-10-CM") %>% 
  select(-PrimaryDiag, -SecondDiag) %>%
  mutate(
    ICD10CM = str_remove(ICDCMCode, "[:punct:]"),
    ICD10CDX = str_sub(ICD10CM, 1, 3),
    meps_year = 2017)

ca_icd10_freqs_ip_2016 <- 
  read_xlsx(
    path="./etc/icd10_freq_tables/2016-hospital-inpatient-diagnosis-code-frequency.xlsx",
    sheet="ICD-10-CM") %>% 
  select(ICDCMCode=ICD10CMCode, TotalDiag) %>%
  mutate(
    ICD10CM = str_remove(ICDCMCode, "[:punct:]"),
    ICD10CDX = str_sub(ICD10CM, 1, 3),
    meps_year = 2016) # This one is missing DiagnosisDesc so we'll come back to this

# Union them all
ca_icd10_freqs_ip <- ca_icd10_freqs_ip_2017 %>% 
  union_all(ca_icd10_freqs_ip_2018) %>% 
  union_all(ca_icd10_freqs_ip_2019) %>% 
  union_all(ca_icd10_freqs_ip_2020) %>% 
  union_all(ca_icd10_freqs_ip_2021)

# Build a DX description crosswalk based on the 5 files that DO have it...
dx_desc <- ca_icd10_freqs_ip %>% 
  distinct(ICD10CM, DiagnosisDesc, meps_year) %>% 
  arrange(desc(meps_year)) %>% 
  group_by(ICD10CM) %>% 
  summarize(ICD10CM = first(ICD10CM),
            DiagnosisDesc = first(DiagnosisDesc))

# .. then apply that to the 2016 file...
ca_icd10_freqs_ip_2016 <- ca_icd10_freqs_ip_2016 %>% 
  left_join(dx_desc) %>% 
  relocate(DiagnosisDesc, .after = ICDCMCode)

# ...and finally union into the final IP freq table with the rest!
ca_icd10_freqs_ip <- ca_icd10_freqs_ip %>% 
  union_all(ca_icd10_freqs_ip_2016) %>% 
  mutate(type = "ip_freq") %>% 
  arrange(meps_year)

# Tidy up
rm(ca_icd10_freqs_ip_2016)
rm(ca_icd10_freqs_ip_2017)
rm(ca_icd10_freqs_ip_2018)
rm(ca_icd10_freqs_ip_2019)
rm(ca_icd10_freqs_ip_2020)
rm(ca_icd10_freqs_ip_2021)

# Parse OP files
ca_icd10_freqs_op_2021 <- 
  read_xlsx(
    path="./etc/icd10_freq_tables/2021-ambulatory-surgery-diagnosis-code-frequency.xlsx",
    sheet="Diagnosis Code 2021") %>% 
  mutate(
    ICD10CM = str_remove(ICDCMCode, "[:punct:]"),
    ICD10CDX = str_sub(ICD10CM, 1, 3),
    meps_year = 2021)

ca_icd10_freqs_op_2020 <- 
  read_xlsx(
    path="./etc/icd10_freq_tables/2020-ambulatory-surgery-diagnosis-code-frequency.xlsx",
    sheet="Diagnosis Code 2020") %>% 
  mutate(
    ICD10CM = str_remove(ICDCMCode, "[:punct:]"),
    ICD10CDX = str_sub(ICD10CM, 1, 3),
    meps_year = 2020)

ca_icd10_freqs_op_2019 <- 
  read_xlsx(
    path="./etc/icd10_freq_tables/2019-ambulatory-surgery-diagnosis-code-frequency.xlsx",
    sheet="Diagnosis Codes 2019") %>% 
  select(-PrimaryDiag, -SecondDiag) %>%
  mutate(
    ICD10CM = str_remove(ICDCMCode, "[:punct:]"),
    ICD10CDX = str_sub(ICD10CM, 1, 3),
    meps_year = 2019)

ca_icd10_freqs_op_2018 <- 
  read_xlsx(
    path="./etc/icd10_freq_tables/2018-ambulatory-surgery-diagnosis-code-frequency.xlsx",
    sheet="Dx Code Frequencies 2018") %>% 
  select(-PrimaryDiag, -SecondDiag) %>%
  rename(ICDCMCode=ICD10CMCode) %>%
  mutate(
    ICD10CM = str_remove(ICDCMCode, "[:punct:]"),
    ICD10CDX = str_sub(ICD10CM, 1, 3),
    meps_year = 2018)

ca_icd10_freqs_op_2017 <- 
  read_xlsx(
    path="./etc/icd10_freq_tables/2017-ambulatory-surgery-diagnosis-code-frequency.xlsx",
    sheet="Dx Code Frequencies 2017") %>% 
  select(-PrimaryDiag, -SecondDiag) %>%
  rename(ICDCMCode=ICD10CMCode) %>%
  mutate(
    ICD10CM = str_remove(ICDCMCode, "[:punct:]"),
    ICD10CDX = str_sub(ICD10CM, 1, 3),
    meps_year = 2017)

ca_icd10_freqs_op_2016 <- 
  read_xlsx(
    path="./etc/icd10_freq_tables/2016-ambulatory-surgery-diagnosis-code-frequency.xlsx",
    sheet="Dx Code Frequencies 2016") %>% 
  select(ICDCMCode=ICD10CMCode, TotalDiag) %>%
  mutate(
    ICD10CM = str_remove(ICDCMCode, "[:punct:]"),
    ICD10CDX = str_sub(ICD10CM, 1, 3),
    meps_year = 2016) # This one is missing DiagnosisDesc so we'll come back to this

# Union them all
ca_icd10_freqs_op <- ca_icd10_freqs_op_2017 %>% 
  union_all(ca_icd10_freqs_op_2018) %>% 
  union_all(ca_icd10_freqs_op_2019) %>% 
  union_all(ca_icd10_freqs_op_2020) %>% 
  union_all(ca_icd10_freqs_op_2021)

# Build a DX description crosswalk based on the 5 files that DO have it...
dx_desc <- ca_icd10_freqs_op %>% 
  distinct(ICD10CM, DiagnosisDesc, meps_year) %>% 
  arrange(desc(meps_year)) %>% 
  group_by(ICD10CM) %>% 
  summarize(ICD10CM = first(ICD10CM),
            DiagnosisDesc = first(DiagnosisDesc))

# .. then apply that to the 2016 file...
ca_icd10_freqs_op_2016 <- ca_icd10_freqs_op_2016 %>% 
  left_join(dx_desc) %>% 
  relocate(DiagnosisDesc, .after = ICDCMCode)

# ...and finally union into the final IP freq table with the rest!
ca_icd10_freqs_op <- ca_icd10_freqs_op %>% 
  union_all(ca_icd10_freqs_op_2016) %>% 
  mutate(type = "op_freq") %>% 
  arrange(meps_year)

# Tidy up
rm(ca_icd10_freqs_op_2016)
rm(ca_icd10_freqs_op_2017)
rm(ca_icd10_freqs_op_2018)
rm(ca_icd10_freqs_op_2019)
rm(ca_icd10_freqs_op_2020)
rm(ca_icd10_freqs_op_2021)

# Union, then pivot to three frequency columns, write out to CSV file
ca_icd10_freqs <- ca_icd10_freqs_ed %>% 
  union_all(ca_icd10_freqs_ip) %>% 
  union_all(ca_icd10_freqs_op) %>% 
  pivot_wider(names_from=type, values_from=TotalDiag) %>% 
  replace_na(list(ed_freq=0, ip_freq=0, op_freq=0)) %>% 
  mutate(total_freq = ed_freq + ip_freq + op_freq) %>% 
  mutate(meps_year = as.character(meps_year)) %>% 
  write_csv("./etc/icd10_freq_tables/icd10_freqs.csv")

rm(ca_icd10_freqs_ed)
rm(ca_icd10_freqs_ip)
rm(ca_icd10_freqs_op)

