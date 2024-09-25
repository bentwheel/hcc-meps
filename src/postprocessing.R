#### Intro Housekeeping ####

library(tidyverse)
library(viridis)
library(ggrepel)

# No scientific notation
options(scipen = 999)

# Pull in the demonstrative profiles that are the highest probability for each (ID, meps_year)
load(here::here("etc/outputs/risk_scores.rda"))
load(here::here("etc/outputs/mode_hcc_mapping.rda"))
load(here::here("etc/outputs/mode_hcc_values.rda"))
load(here::here("etc/outputs/mode_hcc_scores.rda"))

# Load in demographics file used for RA model demo inputs
demographics <- read_csv("etc/outputs/demographics.csv") %>% 
  mutate(DUPERSID = as.character(DUPERSID),
  meps_year = as.character(meps_year))

# Read ESRD diagnosed members to remove them from all CNA/NE models
esrd_filter <- read_csv(here::here("etc/outputs/esrd_individuals.csv")) %>% 
  mutate(DUPERSID = as.character(DUPERSID),
         meps_year = as.character(meps_year)) %>% 
  distinct(DUPERSID, meps_year)

# Filter on min age at EOY = 65 to hopefully remove most OREC == "Disabled" benes
age_filter <- demographics %>% 
  filter(AGE_EXACT >= 65) %>% 
  distinct(DUPERSID, meps_year) 

#### Model Mapping ####
# Determine which model run (and risk score model) to use for each individual

# For NE models, they are intended for / trained on: 
# "For purposes of calibrating the model, beneficiaries without 12 months of Part
#  A and Part B base year Medicare enrollment, but at least one month of payment year enrollment,
#  are defined for Medicare Advantage payment purposes as “new enrollees.”
# https://www.cms.gov/files/document/report-congress-risk-adjustment-medicare-advantage-december-2021.pdf
use_scores_ne_aged_base <- read_csv(here::here("etc/outputs/exposures_total.csv")) %>% 
  mutate(DUPERSID = as.character(DUPERSID),
         meps_year = as.character(meps_year)) %>% 
  pivot_wider(names_from=Type,
              values_from=Expos) %>% 
  filter(meps_year != "2022") %>% # Since this is a filtration on basis year, we don't need this
  filter(MCR < 12) %>% 
  distinct(DUPERSID, meps_year)

use_scores_ne_aged_proj <- read_csv(here::here("etc/outputs/exposures_total.csv")) %>% 
  mutate(DUPERSID = as.character(DUPERSID),
         meps_year = as.character(meps_year)) %>% 
  pivot_wider(names_from=Type,
              values_from=Expos) %>% 
  filter(meps_year != "2016") %>% # Since this is a filtration on payment/projection year, we don't need this
  mutate(orec_elig_mcaid = case_when(
    MCR > 0 & MCD > 0 ~ "0_NE_T",
    MCR > 0 ~ "0_NE_F",
    .default = NA_character_)) %>% 
  filter(!is.na(orec_elig_mcaid)) %>% 
  separate_wider_delim(orec_elig_mcaid, delim="_", names=c("orec_args", "elig_args", "mcaid_args")) %>% 
  select(DUPERSID, meps_year, ends_with("_args")) %>%
  mutate_all(~as.character(.)) %>% 
  mutate(orec_args = as.double(orec_args),
         mcaid_args = as.logical(mcaid_args)) %>% 
  mutate(meps_year = as.character(as.numeric(meps_year) - 1)) 

use_scores_ne_aged <- use_scores_ne_aged_base %>% 
  inner_join(use_scores_ne_aged_proj) %>% 
  inner_join(age_filter) %>% 
  anti_join(esrd_filter)

# Who qualifies for processing in the NE model? What are the total number of individuals with at least 
# one month of payment year enrollment for each year whose demographics are known in the MEPS data for the prior year?

# INSERT GGPLOT IMAGE CODE HERE #

# There is no difference between elig=CNA with mcaid =T or F, (same for elig=CFA, which is not addressed in this project) - 
# mcaid = T/F only matters for the NE model. So now we need to map each member
# in the MEPS datasets to what model they should be used for evaluation. This requires
# A full 12 months in the base year and at least one month in the projection year.
use_scores_CNA_base <- read_csv(here::here("etc/outputs/exposures_total.csv")) %>% 
  mutate(DUPERSID = as.character(DUPERSID),
         meps_year = as.character(meps_year)) %>% 
  pivot_wider(names_from=Type,
              values_from=Expos) %>% 
  filter(meps_year != "2022") %>% # Since this is a filtration on basis year, we don't need this
  filter(MCR == 12 & MCD == 0) %>%  # FULL YEAR OF Medicare Coverage in base year, with no duals (CNA model only)
  distinct(DUPERSID, meps_year)

use_scores_CNA_proj <- read_csv(here::here("etc/outputs/exposures_total.csv")) %>% 
  mutate(DUPERSID = as.character(DUPERSID),
         meps_year = as.character(meps_year)) %>% 
  pivot_wider(names_from=Type,
              values_from=Expos) %>% 
  filter(meps_year != "2016") %>% # Since this is a filtration on payment/projection year, we don't need this
  mutate(orec_elig_mcaid = case_when(
    MCR > 0 & MCD == 0 ~ "0_CNA_F",
    .default = NA_character_)) %>% 
  filter(!is.na(orec_elig_mcaid)) %>% 
  separate_wider_delim(orec_elig_mcaid, delim="_", names=c("orec_args", "elig_args", "mcaid_args")) %>% 
  select(DUPERSID, meps_year, ends_with("_args")) %>%
  mutate_all(~as.character(.)) %>% 
  mutate(orec_args = as.double(orec_args),
         mcaid_args = as.logical(mcaid_args)) %>% 
  mutate(meps_year = as.character(as.numeric(meps_year) - 1)) 

use_scores_CNA <- use_scores_CNA_base %>% 
  inner_join(use_scores_CNA_proj) %>% 
  inner_join(age_filter) %>% 
  anti_join(esrd_filter)

#### Normalize Scores ####
# Process and normalize risk scores for each model population
scores_eval_NE <- mode_hcc_scores %>%
  inner_join(use_scores_ne_aged) %>% 
  mutate(scoremodel = paste0("v",model,"_mode")) %>% 
  select(DUPERSID, meps_year, elig_args, scoremodel, risk_score) %>% 
  pivot_wider(names_from = scoremodel,
              values_from = risk_score)

# Now do the same for CNA
mode_scores_eval_CNA <- mode_hcc_scores %>%
  inner_join(use_scores_CNA) %>% 
  mutate(scoremodel = paste0("v",model,"_mode")) %>% 
  select(DUPERSID, meps_year, elig_args, scoremodel, risk_score)

# Get CNA scores for the same runs - but use the averages and not modes 
avg_scores_eval_CNA <- risk_scores %>% 
  inner_join(use_scores_CNA) %>% 
  mutate(scoremodel = paste0("v",model,"_avg")) %>% 
  select(DUPERSID, meps_year, elig_args, scoremodel, risk_score)

# Combine all scores, pivot, and join to expos/costs info for validation & normalization
scores_eval_CNA <- mode_scores_eval_CNA %>% 
  union_all(avg_scores_eval_CNA) %>% 
  pivot_wider(names_from = scoremodel,
              values_from = risk_score)

# Normalize for each year and for all 8 score model outputs
scores_eval_NE_norm <- scores_eval_NE %>% 
  group_by(meps_year, elig_args) %>% 
  mutate(across(
    .cols = matches("^v(24|28)_(mode|avg)$"),
    .fns = list(mean = function(x) { x / mean(x)}),
    .names = "{.col}_norm"
  )) 

# This check to see normalization was done appropriately for each score - should be all 1's in all rows and columns
scores_eval_NE_norm_check <- scores_eval_NE_norm %>% 
  group_by(meps_year, elig_args) %>% 
  summarize(across(
    .cols = matches("^v(24|28)_(mode|avg)_norm$"),
    .fns = list(mean = function(x) { mean(x)}),
    .names = "{.col}_check"
  )) 

# Normalize for each year and for all 8 score model outputs
scores_eval_CNA_norm <- scores_eval_CNA %>% 
  group_by(meps_year, elig_args) %>% 
  mutate(across(
    .cols = matches("^v(24|28)_(mode|avg)$"),
    .fns = list(mean = function(x) { x / mean(x)}),
    .names = "{.col}_norm"
  )) 

# This check to see normalization was done appropriately for each score - should be all 1's in all rows and columns
scores_eval_CNA_norm_check <- scores_eval_CNA_norm %>% 
  group_by(meps_year, elig_args) %>% 
  summarize(across(
    .cols = matches("^v(24|28)_(mode|avg)_norm$"),
    .fns = list(mean = function(x) { mean(x)}),
    .names = "{.col}_check"
  )) 

#### Develop Cost Predictions ####
# Get expenditures/expos for individuals
expenditures_pmpm <- read_csv("etc/outputs/expenditures_pmpm.csv") %>%
  filter(Type %in% c("MCR", "MCD", "SLF")) %>% 
  pivot_wider(names_from = Type,
              values_from = c(Cost, Cost_noRX, Expos, PMPM_Cost, PMPM_Cost_noRX)) %>% 
  filter(Expos_MCR > 0) %>% 
  mutate(Expos_SLF = Expos_MCR,
         PMPM_Cost_SLF = Cost_SLF / Expos_SLF,
         PMPM_Cost_noRX_SLF = Cost_noRX_SLF / Expos_SLF) %>% 
  # mutate(Cost_MCR = Cost_MCR + Cost_SLF,
  #        Cost_noRX_MCR = Cost_noRX_MCR + Cost_noRX_SLF,
  #        PMPM_Cost_MCR = PMPM_Cost_MCR + PMPM_Cost_SLF,
  #        PMPM_Cost_noRX_MCR = PMPM_Cost_noRX_MCR + PMPM_Cost_noRX_SLF) %>%
  select(-contains("SLF"))

# Helper function to align scores for [meps_year] with PMPM costs for [meps_year+1] and
# calculate A/E ratios at the individual level
test_pred_NE <- function(base_year = 2019, proj_year = base_year+1, base_min_expos=1, proj_min_expos=1)
{
  # Get base year pool list of IDs by minimum expos
  base_pool <- expenditures_pmpm %>% 
    mutate(DUPERSID = as.character(DUPERSID),
           meps_year = as.character(meps_year)) %>% 
    filter(meps_year == base_year,
           Expos_MCR >= base_min_expos) %>% 
    select(DUPERSID, meps_year) 
  
  # Get the target expenditures for this population and scale normalized scores for A/E
  proj_expenditures <- expenditures_pmpm %>% 
    mutate(DUPERSID = as.character(DUPERSID),
           meps_year = as.character(meps_year)) %>% 
    filter(meps_year == proj_year) %>% 
    filter(Expos_MCR >= proj_min_expos) %>% 
    select(DUPERSID, meps_year, Cost_MCR, Cost_noRX_MCR, Expos_MCR, PMPM_Cost_MCR, PMPM_Cost_noRX_MCR) %>% 
    select(-meps_year) %>% 
    inner_join(base_pool) %>% 
    inner_join(scores_eval_NE_norm) %>% 
    group_by(elig_args) %>% 
    mutate(AvgPMPM_MCR = sum(Cost_MCR) / sum(Expos_MCR),
           AvgPMPM_noRX_MCR = sum(Cost_noRX_MCR) / sum(Expos_MCR)) %>% 
    mutate(across(
      .cols = matches("^v(24|28)_(mode|avg)_norm$"),
      .fns = list(pred = function(x) { AvgPMPM_MCR * x}),
      .names = "{.col}_pred_wRX"
    )) %>% 
    mutate(across(
      .cols = matches("^v(24|28)_(mode|avg)_norm$"),
      .fns = list(pred = function(x) { AvgPMPM_noRX_MCR * x}),
      .names = "{.col}_pred_noRX"
    ))
  
  proj_expenditures %>% select(-meps_year)
}

# Helper function to align scores for [meps_year] with PMPM costs for [meps_year+1] and
# calculate A/E ratios at the individual level
test_pred_CNA <- function(base_year = 2016, proj_year = base_year+1, base_min_expos=1, proj_min_expos=1)
{
  # Get base year pool list of IDs by minimum expos
  base_pool <- expenditures_pmpm %>% 
    mutate(DUPERSID = as.character(DUPERSID),
           meps_year = as.character(meps_year)) %>% 
    filter(meps_year == base_year,
           Expos_MCR >= base_min_expos) %>% 
    select(DUPERSID, meps_year) 
  
  # Get the target expenditures for this population and scale normalized scores for A/E
  proj_expenditures <- expenditures_pmpm %>% 
    mutate(DUPERSID = as.character(DUPERSID),
           meps_year = as.character(meps_year)) %>% 
    filter(meps_year == proj_year) %>% 
    filter(Expos_MCR >= proj_min_expos) %>% 
    select(DUPERSID, meps_year, Cost_MCR, Cost_noRX_MCR, Expos_MCR, PMPM_Cost_MCR, PMPM_Cost_noRX_MCR) %>% 
    select(-meps_year) %>% 
    inner_join(base_pool) %>% 
    inner_join(scores_eval_CNA_norm) %>% 
    group_by(elig_args) %>% 
    mutate(AvgPMPM_MCR = sum(Cost_MCR) / sum(Expos_MCR),
           AvgPMPM_noRX_MCR = sum(Cost_noRX_MCR) / sum(Expos_MCR)) %>% 
    mutate(across(
      .cols = matches("^v(24|28)_(mode|avg)_norm$"),
      .fns = list(pred = function(x) { AvgPMPM_MCR * x}),
      .names = "{.col}_pred_wRX"
    )) %>% 
    mutate(across(
      .cols = matches("^v(24|28)_(mode|avg)_norm$"),
      .fns = list(pred = function(x) { AvgPMPM_noRX_MCR * x}),
      .names = "{.col}_pred_noRX"
    ))
  
  proj_expenditures %>% select(-meps_year)
}

# Demographic labels for more nuanced A2E plots later
demos_for_eval <- demographics %>% 
  mutate(DUPERSID = as.character(DUPERSID)) %>% 
  select(DUPERSID, meps_year, AGE_EXACT, SEX) %>% 
  mutate(AGE_GRP = case_when(
    AGE_EXACT < 65 ~ "Under 65",
    AGE_EXACT < 70 ~ "65 - 69",
    AGE_EXACT < 75 ~ "70 - 74",
    AGE_EXACT < 80 ~ "75 - 79",
    AGE_EXACT < 85 ~ "80 - 84",
    AGE_EXACT >= 85 ~ "Over 85")) %>% 
  arrange(AGE_EXACT) %>% 
  mutate(AGE_GRP = forcats::fct_inorder(AGE_GRP)) %>% 
  select(-AGE_EXACT)

# Build 2016-2021 base year projections
# Set min_expos variables to dial into more stringent exposure requirements for comparisons
projections_NE <- as_tibble(seq(2016,2021,by=1)) %>% 
  rename(meps_year = value) %>% 
  mutate(projections = purrr::map(meps_year, function(x) {test_pred_NE(base_year=x, base_min_expos = 1, proj_min_expos = 1)})) %>% 
  unnest(cols = "projections") %>% 
  mutate(meps_year = as.character(meps_year)) %>% 
  left_join(demos_for_eval) %>% 
  write_csv("etc/outputs/predictions_NE.csv") 

projections_CNA <- as_tibble(seq(2016,2021,by=1)) %>% 
  rename(meps_year = value) %>% 
  mutate(projections = purrr::map(meps_year, function(x) {test_pred_CNA(base_year=x, base_min_expos = 1, proj_min_expos = 1)})) %>% 
  unnest(cols = "projections") %>% 
  mutate(meps_year = as.character(meps_year)) %>% 
  left_join(demos_for_eval) %>% 
  write_csv("etc/outputs/predictions_CNA.csv")

#### R-Squared Plot for NE Models by MEPS Year ####
# Evaluate r-sq by meps_year for NE. Should focus only on the pop. for which the NE is intended.
rsq_check_NE <- projections_NE %>% 
  select(DUPERSID, meps_year, PMPM_Cost_MCR, PMPM_Cost_noRX_MCR, elig_args, 
         matches("^v(24|28)_(mode|avg)_norm_pred_(wRX|noRX)")) %>% 
  pivot_longer(
    cols = -c(DUPERSID,meps_year,elig_args,PMPM_Cost_MCR,PMPM_Cost_noRX_MCR),
    names_to = c("model", "calc", "RX_noRX"),
    names_pattern = "^v(24|28)_(mode|avg)_norm_pred_(wRX|noRX)$",
    values_to = "PMPM_Cost_MCR_Pred"
  ) %>% 
  pivot_wider(
    names_from = RX_noRX,
    values_from = PMPM_Cost_MCR_Pred,
  ) %>% 
  group_by(meps_year, elig_args, model, calc) %>% 
  summarize(rsq_wRX = cor(PMPM_Cost_MCR, wRX)^2,
            rsq_noRX = cor(PMPM_Cost_noRX_MCR, noRX)^2,
            count = n()) %>% 
  pivot_longer(
    cols = -c(meps_year, elig_args, model, calc, count),
    names_to = "Target",
    values_to = "R-Squared") %>% 
  unite(calc, Target, col="calc")
    
rsq_eval_NE.plot <- rsq_check_NE %>% 
  mutate(meps_year = fct_inorder(paste0(meps_year, "\n(n = ", count, ")"))) %>% 
  mutate(model = paste0("CMS-HCC v", model, " NE")) %>% 
  filter(elig_args == "NE",
         calc == "mode_rsq_wRX") %>% 
  ggplot(aes(x = meps_year,
             y = `R-Squared`,
             fill = model)) +
  geom_bar(stat = "identity", position="dodge2") + 
  geom_text(aes(label=scales::label_percent(accuracy=.1)(`R-Squared`)),
            position=position_dodge2(width=.9), vjust=-.8, size=2.5) +
  theme_classic() +
  scale_fill_viridis_d(end = 0.5) +
  scale_y_continuous(labels = scales::label_percent(accuracy=.1)) +
  labs(title = "Predictive Performance of CMS-HCC New Enrollee Models",
       subtitle = "Using Medical Expenditure Panel Data (MEPS) PUFs Datasets as Inputs",
       x = "MEPS Data Input Year",
       fill = "Model")

rsq_eval_NE.plot %>% ggsave(file=here::here("etc/images/rsq_ne.png"), height=8.5, width=11)

#### R-Squared Plot for CNA Models by MEPS Year ####
# Evaluate r-sq by meps_year for CNA. Should focus only on the pop. for which the CNA model is intended.
rsq_check_CNA <- projections_CNA %>% 
  select(DUPERSID, meps_year, PMPM_Cost_MCR, PMPM_Cost_noRX_MCR, elig_args, 
         matches("^v(24|28)_(mode|avg)_norm_pred_(wRX|noRX)")) %>% 
  pivot_longer(
    cols = -c(DUPERSID,meps_year,elig_args,PMPM_Cost_MCR,PMPM_Cost_noRX_MCR),
    names_to = c("model", "calc", "RX_noRX"),
    names_pattern = "^v(24|28)_(mode|avg)_norm_pred_(wRX|noRX)$",
    values_to = "PMPM_Cost_MCR_Pred"
  ) %>% 
  pivot_wider(
    names_from = RX_noRX,
    values_from = PMPM_Cost_MCR_Pred,
  ) %>% 
  group_by(meps_year, elig_args, model, calc) %>% 
  summarize(rsq_wRX = cor(PMPM_Cost_MCR, wRX)^2,
            rsq_noRX = cor(PMPM_Cost_noRX_MCR, noRX)^2,
            count = n()) %>% 
  pivot_longer(
    cols = -c(meps_year, elig_args, model, calc, count),
    names_to = "Target",
    values_to = "R-Squared") 

rsq_eval_CNA.plot <- rsq_check_CNA %>% 
  mutate(meps_year = fct_inorder(paste0(meps_year, "\n(n = ", scales::label_comma(accuracy=1)(count), ")"))) %>% 
  filter(str_detect(Target, "_wRX")) %>% 
  mutate(model = paste0("CMS-HCC v", model, " CNA (", str_to_title(calc), ")")) %>% 
  ggplot(aes(x = meps_year,
             y = `R-Squared`,
             fill = model)) +
  geom_bar(stat = "identity", position="dodge2") + 
  geom_text(aes(label=scales::label_percent(accuracy=.1)(`R-Squared`)),
            position=position_dodge2(width=.9), vjust=-.8, size=2.5) +
  theme_classic() +
  scale_fill_viridis_d(end = 0.5) +
  scale_y_continuous(labels = scales::label_percent(accuracy=.1)) +
  labs(title = "Predictive Performance of CMS-HCC Community Non-Dual Aged (CNA) Models",
       subtitle = "Using Medical Expenditure Panel Data (MEPS) PUFs Datasets as Inputs",
       x = "MEPS Data Input Year",
       fill = "Model") + 
  theme(legend.position = "bottom")

rsq_eval_CNA.plot %>% ggsave(file=here::here("etc/images/rsq_cna.png"), height=8.5, width=11)

#### A-to-E Plots for NE models by MEPS Year ####
a2e_check_NE <- projections_NE %>% 
  select(DUPERSID, meps_year, elig_args, Cost_MCR, Cost_noRX_MCR, Expos_MCR, 
         matches("^v(24|28)_(mode|avg)_norm_pred_(wRX|noRX)")) %>% 
  mutate(across(
    .cols = matches("^v(24|28)_(mode|avg)_norm_pred_wRX"),
    .fns = list(pmpy = function(x) { Expos_MCR * x}),
    .names = "{.col}_pmpy"
  )) %>% 
  mutate(across(
    .cols = matches("^v(24|28)_(mode|avg)_norm_pred_noRX"),
    .fns = list(pmpy = function(x) { Expos_MCR * x}),
    .names = "{.col}_pmpy"
  )) %>% 
  select(DUPERSID, meps_year, elig_args, Cost_MCR, Cost_noRX_MCR, Expos_MCR, 
         matches("^v(24|28)_(mode|avg)_norm_pred_(wRX|noRX)_pmpy$")) %>% 
  pivot_longer(
    cols = -c(DUPERSID,meps_year,elig_args,Cost_MCR,Cost_noRX_MCR, Expos_MCR),
    names_to = c("model", "calc", "RX_noRX"),
    names_pattern = "^v(24|28)_(mode|avg)_norm_pred_(wRX|noRX)_pmpy$",
    values_to = "Cost_MCR_Pred"
  ) %>% 
  pivot_wider(
    names_from = RX_noRX,
    values_from = Cost_MCR_Pred
  )

a2e_by_model_NE.data <- a2e_check_NE %>% 
  group_by(meps_year, elig_args, model, calc) %>% 
  summarize(Cost_MCR = sum(Cost_MCR),
            Cost_noRX_MCR = sum(Cost_noRX_MCR),
            Expos_MCR = sum(Expos_MCR),
            Cost_MCR_Pred_wRX = sum(wRX),
            Cost_MCR_Pred_noRX = sum(noRX),
            count = n()) %>%
  ungroup() %>% 
  mutate(a2e = Cost_MCR / Cost_MCR_Pred_wRX,
         a2e_noRX = Cost_noRX_MCR / Cost_MCR_Pred_noRX) %>% 
  mutate(Cost_MCR_Actual = Cost_MCR,
         Cost_MCR_Predicted = Cost_MCR_Pred_wRX) %>% 
  select(meps_year, elig_args, model, count, Cost_MCR_Actual, Cost_MCR_Predicted, Expos_MCR) %>% 
  mutate(PMPM_Cost_MCR_Actual = Cost_MCR_Actual / Expos_MCR,
         PMPM_Cost_MCR_Predicted = Cost_MCR_Predicted / Expos_MCR) %>% 
  select(-c(Cost_MCR_Actual, Cost_MCR_Predicted, Expos_MCR)) %>% 
  pivot_longer(
    cols = -c(meps_year, elig_args, model, count),
    names_to = "Type",
    values_to = "Value",
    names_pattern = "^PMPM_Cost_MCR_(Actual|Predicted)$"
  ) 

a2e_by_model_NE.plot <- a2e_by_model_NE.data %>% 
  mutate(model = paste0("CMS-HCC v", model, " NE")) %>% 
  unite(model, Type, col=Type, sep=" ") %>% 
  mutate(meps_year = fct_inorder(paste0(meps_year, "\n(n = ", count, ")"))) %>% 
  filter(Type != "CMS-HCC v24 NE Actual") %>% # arbitrarily remove one of the actual values - v24 / v28 - they're the same
  mutate(Type = if_else(str_detect(Type, "Actual"), "Actual", Type)) %>% 
  arrange(meps_year, Type) %>% 
  mutate(Type = fct_inorder(Type)) %>% 
  ggplot(aes(x = meps_year,
             y = Value,
             fill = Type)) + 
  geom_bar(stat="identity", position=position_dodge2(width=.9)) + 
  geom_text(aes(label = scales::label_dollar(accuracy=1)(Value)),
                   size = 3, position=position_dodge2(width=.9), vjust=-.8) +
  scale_fill_viridis_d(end = 0.5) +
  scale_y_continuous(labels = scales::label_dollar(accuracy=1)) +
  theme_classic() + 
  labs(
    title = "Actual Costs vs. Costs Predicted by CMS-HCC New Enrollee Models",
    subtitle = "Using Medical Expenditure Panel Data (MEPS) PUFs Datasets as Inputs",
    x = "MEPS Data Input Year",
    y = "Per Member, Per Month (PMPM) Costs (Medicare Only)",
    fill = "Value") +
  theme(legend.position = "bottom") 

a2e_by_model_NE.plot %>% ggsave(file=here::here("etc/images/a2e_ne.png"), height=8.5, width=11)

#### A-to-E plots for NE models - BY AGE ####
a2e_by_age_check_NE <- projections_NE %>% 
  select(DUPERSID, meps_year, elig_args, AGE_GRP, Cost_MCR, Cost_noRX_MCR, Expos_MCR, 
         matches("^v(24|28)_(mode|avg)_norm_pred_(wRX|noRX)")) %>% 
  mutate(across(
    .cols = matches("^v(24|28)_(mode|avg)_norm_pred_wRX"),
    .fns = list(pmpy = function(x) { Expos_MCR * x}),
    .names = "{.col}_pmpy"
  )) %>% 
  mutate(across(
    .cols = matches("^v(24|28)_(mode|avg)_norm_pred_noRX"),
    .fns = list(pmpy = function(x) { Expos_MCR * x}),
    .names = "{.col}_pmpy"
  )) %>% 
  select(DUPERSID, meps_year, elig_args, AGE_GRP, Cost_MCR, Cost_noRX_MCR, Expos_MCR, 
         matches("^v(24|28)_(mode|avg)_norm_pred_(wRX|noRX)_pmpy$")) %>% 
  pivot_longer(
    cols = -c(DUPERSID,meps_year,elig_args,AGE_GRP, Cost_MCR,Cost_noRX_MCR, Expos_MCR),
    names_to = c("model", "calc", "RX_noRX"),
    names_pattern = "^v(24|28)_(mode|avg)_norm_pred_(wRX|noRX)_pmpy$",
    values_to = "Cost_MCR_Pred"
  ) %>% 
  pivot_wider(
    names_from = RX_noRX,
    values_from = Cost_MCR_Pred
  )

a2e_by_age_NE.data <- a2e_by_age_check_NE %>% 
  group_by(elig_args, model, calc, AGE_GRP) %>% 
  summarize(Cost_MCR = sum(Cost_MCR),
            Cost_noRX_MCR = sum(Cost_noRX_MCR),
            Expos_MCR = sum(Expos_MCR),
            Cost_MCR_Pred_wRX = sum(wRX),
            Cost_MCR_Pred_noRX = sum(noRX),
            count = n()) %>%
  ungroup() %>% 
  mutate(a2e = Cost_MCR / Cost_MCR_Pred_wRX,
         a2e_noRX = Cost_noRX_MCR / Cost_MCR_Pred_noRX) %>% 
  mutate(Cost_MCR_Actual = Cost_MCR,
         Cost_MCR_Predicted = Cost_MCR_Pred_wRX) %>% 
  select(AGE_GRP, elig_args, model, count, Cost_MCR_Actual, Cost_MCR_Predicted, Expos_MCR) %>% 
  mutate(PMPM_Cost_MCR_Actual = Cost_MCR_Actual / Expos_MCR,
         PMPM_Cost_MCR_Predicted = Cost_MCR_Predicted / Expos_MCR) %>% 
  select(-c(Cost_MCR_Actual, Cost_MCR_Predicted, Expos_MCR)) %>% 
  pivot_longer(
    cols = -c(AGE_GRP, elig_args, model, count),
    names_to = "Type",
    values_to = "Value",
    names_pattern = "^PMPM_Cost_MCR_(Actual|Predicted)$"
  ) 

a2e_by_age_NE.plot <- a2e_by_age_NE.data %>% 
  mutate(model = paste0("CMS-HCC v", model, " NE")) %>% 
  unite(model, Type, col=Type, sep=" ") %>% 
  mutate(AGE_GRP = fct_inorder(paste0(AGE_GRP, "\n(n = ", count, ")"))) %>% 
  filter(Type != "CMS-HCC v24 NE Actual") %>% # arbitrarily remove one of the actual values - v24 / v28 - they're the same
  mutate(Type = if_else(str_detect(Type, "Actual"), "Actual", Type)) %>% 
  arrange(AGE_GRP, Type) %>% 
  mutate(Type = fct_inorder(Type)) %>% 
  ggplot(aes(x = AGE_GRP,
             y = Value,
             fill = Type)) + 
  geom_bar(stat="identity", position=position_dodge2(width=.9)) + 
  geom_text(aes(label = scales::label_dollar(accuracy=1)(Value)),
            size = 3, position=position_dodge2(width=.9), vjust=-.8) +
  scale_fill_viridis_d(end = 0.5) +
  scale_y_continuous(labels = scales::label_dollar(accuracy=1)) +
  theme_classic() + 
  labs(
    title = "Actual Costs vs. Costs Predicted by CMS-HCC New Enrollee Models",
    subtitle = "Using Medical Expenditure Panel Data (MEPS) PUFs Datasets as Inputs",
    x = "Age Group",
    y = "Per Member, Per Month (PMPM) Costs (Medicare Only)",
    fill = "Value") +
  theme(legend.position = "bottom") 

a2e_by_age_NE.plot %>% ggsave(file=here::here("etc/images/a2e_ne_by_age.png"), height=8.5, width=11)

#### A-to-E plots for CNA models by MEPS Year ####
a2e_check_CNA <- projections_CNA %>% 
  select(DUPERSID, meps_year, elig_args, Cost_MCR, Cost_noRX_MCR, Expos_MCR, 
         matches("^v(24|28)_(mode|avg)_norm_pred_(wRX|noRX)")) %>% 
  mutate(across(
    .cols = matches("^v(24|28)_(mode|avg)_norm_pred_wRX"),
    .fns = list(pmpy = function(x) { Expos_MCR * x}),
    .names = "{.col}_pmpy"
  )) %>% 
  mutate(across(
    .cols = matches("^v(24|28)_(mode|avg)_norm_pred_noRX"),
    .fns = list(pmpy = function(x) { Expos_MCR * x}),
    .names = "{.col}_pmpy"
  )) %>% 
  select(DUPERSID, meps_year, elig_args, Cost_MCR, Cost_noRX_MCR, Expos_MCR, 
         matches("^v(24|28)_(mode|avg)_norm_pred_(wRX|noRX)_pmpy$")) %>% 
  pivot_longer(
    cols = -c(DUPERSID,meps_year,elig_args,Cost_MCR,Cost_noRX_MCR, Expos_MCR),
    names_to = c("model", "calc", "RX_noRX"),
    names_pattern = "^v(24|28)_(mode|avg)_norm_pred_(wRX|noRX)_pmpy$",
    values_to = "Cost_MCR_Pred"
  ) %>% 
  pivot_wider(
    names_from = RX_noRX,
    values_from = Cost_MCR_Pred
  ) 

a2e_by_model_CNA.data <- a2e_check_CNA %>% 
  group_by(meps_year, elig_args, model, calc) %>% 
  summarize(Cost_MCR = sum(Cost_MCR),
            Cost_noRX_MCR = sum(Cost_noRX_MCR),
            Expos_MCR = sum(Expos_MCR),
            Cost_MCR_Pred_wRX = sum(wRX),
            Cost_MCR_Pred_noRX = sum(noRX),
            count = n()) %>%
  ungroup() %>% 
  mutate(a2e = Cost_MCR / Cost_MCR_Pred_wRX,
         a2e_noRX = Cost_noRX_MCR / Cost_MCR_Pred_noRX) %>% 
  mutate(Cost_MCR_Actual = Cost_MCR,
         Cost_MCR_Predicted = Cost_MCR_Pred_wRX) %>% 
  select(meps_year, elig_args, model, calc, count, Cost_MCR_Actual, Cost_MCR_Predicted, Expos_MCR) %>% 
  mutate(PMPM_Cost_MCR_Actual = Cost_MCR_Actual / Expos_MCR,
         PMPM_Cost_MCR_Predicted = Cost_MCR_Predicted / Expos_MCR) %>% 
  select(-c(Cost_MCR_Actual, Cost_MCR_Predicted, Expos_MCR)) %>% 
  pivot_longer(
    cols = -c(meps_year, elig_args, model, count, calc),
    names_to = "Type",
    values_to = "Value",
    names_pattern = "^PMPM_Cost_MCR_(Actual|Predicted)$"
  )

a2e_by_model_CNA.plot <- a2e_by_model_CNA.data %>% 
  mutate(model = paste0("CMS-HCC v", model, " CNA"),
         calc = paste0("(", str_to_title(calc), ")")) %>% 
  unite(model, Type, calc, col=Type, sep=" ") %>% 
  mutate(meps_year = fct_inorder(paste0(meps_year, "\n(n = ", scales::label_comma(accuracy=1)(count), ")"))) %>% 
  filter(Type == "CMS-HCC v24 CNA Actual (Avg)" | str_detect(Type, "Predicted")) %>% # arbitrarily remove all but one of the actual values - v24 / v28 - they're the same
  arrange(meps_year, Type) %>% 
  mutate(Type = fct_inorder(Type)) %>% 
  ggplot(aes(x = meps_year,
             y = Value,
             fill = Type)) + 
  geom_bar(stat="identity", position=position_dodge2(width=.9)) + 
  geom_text(aes(label = scales::label_dollar(accuracy=1)(Value)),
            size = 3, position=position_dodge2(width=.9), vjust=-.8) +
  scale_fill_viridis_d(end = 0.5) +
  scale_y_continuous(labels = scales::label_dollar(accuracy=1)) +
  theme_classic() + 
  labs(
    title = "Actual Costs vs. Costs Predicted by CMS-HCC Community Non-Dual Aged (CNA) Models",
    subtitle = "Using Medical Expenditure Panel Data (MEPS) PUFs Datasets as Inputs",
    x = "MEPS Data Input Year",
    y = "Per Member, Per Month (PMPM) Costs (Medicare Only)",
    fill = "Value") +
  theme(legend.position = "bottom") 

a2e_by_model_CNA.plot %>% ggsave(file=here::here("etc/images/a2e_cna.png"), height=8.5, width=11)

#### A-to-E plots for CNA models by Age Group ####
a2e_by_age_check_CNA <- projections_CNA %>% 
  select(DUPERSID, AGE_GRP, elig_args, Cost_MCR, Cost_noRX_MCR, Expos_MCR, 
         matches("^v(24|28)_(mode|avg)_norm_pred_(wRX|noRX)")) %>% 
  mutate(across(
    .cols = matches("^v(24|28)_(mode|avg)_norm_pred_wRX"),
    .fns = list(pmpy = function(x) { Expos_MCR * x}),
    .names = "{.col}_pmpy"
  )) %>% 
  mutate(across(
    .cols = matches("^v(24|28)_(mode|avg)_norm_pred_noRX"),
    .fns = list(pmpy = function(x) { Expos_MCR * x}),
    .names = "{.col}_pmpy"
  )) %>% 
  select(DUPERSID, AGE_GRP, elig_args, Cost_MCR, Cost_noRX_MCR, Expos_MCR, 
         matches("^v(24|28)_(mode|avg)_norm_pred_(wRX|noRX)_pmpy$")) %>% 
  pivot_longer(
    cols = -c(DUPERSID,AGE_GRP,elig_args,Cost_MCR,Cost_noRX_MCR, Expos_MCR),
    names_to = c("model", "calc", "RX_noRX"),
    names_pattern = "^v(24|28)_(mode|avg)_norm_pred_(wRX|noRX)_pmpy$",
    values_to = "Cost_MCR_Pred"
  ) %>% 
  filter(RX_noRX == "wRX") %>% 
  rename(wRX = Cost_MCR_Pred)

a2e_by_age_CNA.data <- a2e_by_age_check_CNA %>% 
  group_by(AGE_GRP, elig_args, model, calc) %>% 
  summarize(Cost_MCR = sum(Cost_MCR),
            Expos_MCR = sum(Expos_MCR),
            Cost_MCR_Pred_wRX = sum(wRX),
            count = n()) %>%
  ungroup() %>% 
  mutate(a2e = Cost_MCR / Cost_MCR_Pred_wRX) %>% 
  mutate(Cost_MCR_Actual = Cost_MCR,
         Cost_MCR_Predicted = Cost_MCR_Pred_wRX) %>% 
  select(AGE_GRP, elig_args, model, calc, count, Cost_MCR_Actual, Cost_MCR_Predicted, Expos_MCR) %>% 
  mutate(PMPM_Cost_MCR_Actual = Cost_MCR_Actual / Expos_MCR,
         PMPM_Cost_MCR_Predicted = Cost_MCR_Predicted / Expos_MCR) %>% 
  select(-c(Cost_MCR_Actual, Cost_MCR_Predicted, Expos_MCR)) %>% 
  pivot_longer(
    cols = -c(AGE_GRP, elig_args, model, count, calc),
    names_to = "Type",
    values_to = "Value",
    names_pattern = "^PMPM_Cost_MCR_(Actual|Predicted)$"
  )

a2e_by_age_CNA.plot <- a2e_by_age_CNA.data %>% 
  mutate(model = paste0("CMS-HCC v", model, " CNA"),
         calc = paste0("(", str_to_title(calc), ")")) %>% 
  unite(model, Type, calc, col=Type, sep=" ") %>% 
  mutate(AGE_GRP = fct_inorder(paste0(AGE_GRP, "\n(n = ", scales::label_comma(accuracy=1)(count), ")"))) %>% 
  filter(!str_detect(Type, "Mode")) %>% 
  filter(Type == "CMS-HCC v24 CNA Actual (Avg)" | str_detect(Type, "Predicted")) %>% # arbitrarily remove all but one of the actual values - v24 / v28 - they're the same
  arrange(AGE_GRP, Type) %>% 
  mutate(Type = fct_inorder(Type)) %>% 
  ggplot(aes(x = AGE_GRP,
             y = Value,
             fill = Type)) + 
  geom_bar(stat="identity", position=position_dodge2(width=.9)) + 
  geom_text(aes(label = scales::label_dollar(accuracy=1)(Value)),
            size = 2.5, position=position_dodge2(width=.9), vjust=-.8) +
  scale_fill_viridis_d(end = 0.5) +
  scale_y_continuous(labels = scales::label_dollar(accuracy=1),
                     limits = c(0, 1250)) +
  theme_classic() + 
  labs(
    title = "Actual Costs vs. Costs Predicted by CMS-HCC Community Non-Dual Aged (CNA) Models",
    subtitle = "Using Medical Expenditure Panel Data (MEPS) PUFs Datasets as Inputs",
    x = "Age Group",
    y = "Per Member, Per Month (PMPM) Costs (Medicare Only)",
    fill = "Value") +
  theme(legend.position = "bottom")

a2e_by_age_CNA.plot %>% ggsave(file=here::here("etc/images/a2e_cna_by_age.png"), height=8.5, width=11)

#### A-to-E plots for CNA models by Model AND Age Group ####
a2e_by_age_model_check_CNA <- projections_CNA %>% 
  select(DUPERSID, meps_year, AGE_GRP, elig_args, Cost_MCR, Cost_noRX_MCR, Expos_MCR, 
         matches("^v(24|28)_(mode|avg)_norm_pred_(wRX|noRX)")) %>% 
  mutate(across(
    .cols = matches("^v(24|28)_(mode|avg)_norm_pred_wRX"),
    .fns = list(pmpy = function(x) { Expos_MCR * x}),
    .names = "{.col}_pmpy"
  )) %>% 
  mutate(across(
    .cols = matches("^v(24|28)_(mode|avg)_norm_pred_noRX"),
    .fns = list(pmpy = function(x) { Expos_MCR * x}),
    .names = "{.col}_pmpy"
  )) %>% 
  select(DUPERSID, meps_year, AGE_GRP, elig_args, Cost_MCR, Cost_noRX_MCR, Expos_MCR, 
         matches("^v(24|28)_(mode|avg)_norm_pred_(wRX|noRX)_pmpy$")) %>% 
  pivot_longer(
    cols = -c(DUPERSID,meps_year,AGE_GRP,elig_args,Cost_MCR,Cost_noRX_MCR, Expos_MCR),
    names_to = c("model", "calc", "RX_noRX"),
    names_pattern = "^v(24|28)_(mode|avg)_norm_pred_(wRX|noRX)_pmpy$",
    values_to = "Cost_MCR_Pred"
  ) %>% 
  pivot_wider(
    names_from = RX_noRX,
    values_from = Cost_MCR_Pred
  ) 

a2e_by_age_model_CNA.data <- a2e_by_age_model_check_CNA %>% 
  group_by(meps_year, AGE_GRP, elig_args, model, calc) %>% 
  summarize(Cost_MCR = sum(Cost_MCR),
            Cost_noRX_MCR = sum(Cost_noRX_MCR),
            Expos_MCR = sum(Expos_MCR),
            Cost_MCR_Pred_wRX = sum(wRX),
            Cost_MCR_Pred_noRX = sum(noRX),
            count = n()) %>%
  ungroup() %>% 
  mutate(a2e = Cost_MCR / Cost_MCR_Pred_wRX,
         a2e_noRX = Cost_noRX_MCR / Cost_MCR_Pred_noRX) %>% 
  mutate(Cost_MCR_Actual = Cost_MCR,
         Cost_MCR_Predicted = Cost_MCR_Pred_wRX) %>% 
  select(meps_year, AGE_GRP, elig_args, model, calc, count, Cost_MCR_Actual, Cost_MCR_Predicted, Expos_MCR) %>% 
  mutate(PMPM_Cost_MCR_Actual = Cost_MCR_Actual / Expos_MCR,
         PMPM_Cost_MCR_Predicted = Cost_MCR_Predicted / Expos_MCR) %>% 
  select(-c(Cost_MCR_Actual, Cost_MCR_Predicted, Expos_MCR)) %>% 
  pivot_longer(
    cols = -c(meps_year, AGE_GRP, elig_args, model, count, calc),
    names_to = "Type",
    values_to = "Value",
    names_pattern = "^PMPM_Cost_MCR_(Actual|Predicted)$"
  )

a2e_by_age_model_CNA.plot <- a2e_by_age_model_CNA.data %>% 
  mutate(model = paste0("CMS-HCC v", model, " CNA"),
         calc = paste0("(", str_to_title(calc), ")")) %>% 
  unite(model, Type, calc, col=Type, sep=" ") %>% 
  mutate(meps_year = fct_inorder(as.character(meps_year))) %>% 
  filter(!str_detect(Type, "Mode")) %>% 
  filter(Type == "CMS-HCC v24 CNA Actual (Avg)" | str_detect(Type, "Predicted")) %>% # arbitrarily remove all but one of the actual values - v24 / v28 - they're the same
  arrange(meps_year, Type) %>% 
  mutate(Type = fct_inorder(Type)) %>% 
  ggplot(aes(x = AGE_GRP,
             y = Value,
             fill = Type)) + 
  geom_bar(stat="identity", position=position_dodge2(width=.9)) + 
  geom_text(aes(label = scales::label_dollar(accuracy=1)(Value)),
            size = 2.5, position=position_dodge2(width=.9), vjust=-.8) +
  scale_fill_viridis_d(end = 0.5) +
  scale_y_continuous(labels = scales::label_dollar(accuracy=1),
                     limits = c(0, 1450)) +
  theme_classic() + 
  labs(
    title = "Actual Costs vs. Costs Predicted by CMS-HCC Community Non-Dual Aged (CNA) Models",
    subtitle = "Using Medical Expenditure Panel Data (MEPS) PUFs Datasets as Inputs",
    x = "Age Group",
    y = "Per Member, Per Month (PMPM) Costs (Medicare Only)",
    fill = "Value") +
  theme(legend.position = "bottom") +
  facet_wrap(meps_year ~ ., nrow=6)

a2e_by_age_model_CNA.plot %>% ggsave(file=here::here("etc/images/a2e_cna_by_age_model.png"), height=8.5, width=11)
