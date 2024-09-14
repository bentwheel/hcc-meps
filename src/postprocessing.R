library(tidyverse)
library(viridis)
library(ggrepel)

# No scientific notation
options(scipen = 999)

# Load in the original contingencies file
load("etc/contingencies_n=250.rda")

# Pull in the demonstrative profiles that are the highest probability for each (ID, meps_year)
mode_risk_scores <- read_csv("etc/outputs/mode_risk_scores.csv")
mode_hcc_mappings <- read_csv("etc/outputs/mode_hcc_mappings.csv")
mode_hcc_values <- read_csv("etc/outputs/mode_hcc_values.csv")
avg_risk_scores <- read_csv("etc/outputs/avg_risk_scores.csv")
demographics <- read_csv("etc/outputs/demographics.csv")

# Pull mode profiles out of the contingencies file to map them with risk scores accordingly
mode_contingencies <- contingencies_to_run %>%
  select(-DUPERSID, -meps_year) %>%
  separate_wider_delim("id", names=c("DUPERSID", "meps_year", "profid1"), delim="-",
                       cols_remove=F) %>%
  group_by(DUPERSID, meps_year) %>% 
  arrange(desc(probability)) %>% 
  slice_max(order_by = probability, n = 1, with_ties=F) %>% 
  ungroup()

# Get CNA / NE scores for non-Medicaid folks with OREC of 0 (aged) (Run ID = 10, 18)
mode_scores_eval <- mode_risk_scores %>%
  filter(Run_ID %in% c(10, 18) & model %in% c(24, 28)) %>%
  mutate(scoremodel = paste0(elig_args,"_v",model,"_mode")) %>% 
  select(DUPERSID, meps_year, scoremodel, risk_score)

# Get CNA scores for the same runs - but use the averages and not modes (Run ID = 10, v24)
avg_scores_eval <- avg_risk_scores %>% 
  filter(Run_ID %in% c(10, 18) & model %in% c(24, 28)) %>%
  mutate(scoremodel = paste0(elig_args,"_v",model,"_avg")) %>% 
  select(DUPERSID, meps_year, scoremodel, risk_score)

# Get expenditures/expos for individuals
expenditures_pmpm <- read_csv("etc/outputs/expenditures_pmpm.csv") %>%
  filter(Type %in% c("MCR", "MCD")) %>% 
  pivot_wider(names_from = Type,
              values_from = c(Cost, Expos, PMPM_Cost)) %>% 
  filter(Expos_MCR > 0 & Expos_MCD == 0) 

# Combine all scores, pivot, and join to expos/costs info for validation & normalization
scores_eval <- mode_scores_eval %>% 
  union_all(avg_scores_eval) %>% 
  pivot_wider(names_from = scoremodel,
              values_from = risk_score)

# Normalize for each year and for all 8 score model outputs
scores_eval_norm <- scores_eval %>% 
  group_by(meps_year) %>% 
  mutate(across(
    .cols = matches("^(CNA|NE)_v(24|28)_(mode|avg)$"),
    .fns = list(mean = function(x) { x / mean(x)}),
    .names = "{.col}_norm"
  )) 

# This check to see normalization was done appropriately for each score - should be all 1's in all rows and columns
scores_eval_norm_check <- scores_eval_norm %>% 
  group_by(meps_year) %>% 
  summarize(across(
    .cols = matches("^(CNA|NE)_v(24|28)_(mode|avg)_norm$"),
    .fns = list(mean = function(x) { mean(x)}),
    .names = "{.col}_check"
  )) 

# Helper function to align scores for [meps_year] with PMPM costs for [meps_year+1] and
# calculate A/E ratios at the individual level
test_pred <- function(base_year = 2018, proj_year = base_year+1, base_min_expos=1, proj_min_expos=1)
{
  # Get base year pool list of IDs by minimum expos
  base_pool <- expenditures_pmpm %>% 
    filter(meps_year == base_year,
           Expos_MCR >= base_min_expos) %>% 
    select(DUPERSID, meps_year)
  
  # If base year is 2017, we need to make sure to remove the panel number (first 2 chars) from meps_year so the
  # join works.
  
  # Get the target expenditures for this population and scale normalized scores for A/E
  proj_expenditures <- expenditures_pmpm %>% 
    filter(meps_year == proj_year) %>% 
    filter(Expos_MCR >= proj_min_expos) %>% 
    select(DUPERSID, meps_year, Cost_MCR, Expos_MCR, PMPM_Cost_MCR) %>% 
    mutate(DUPERSID = if_else(meps_year == 2018, as.double(str_sub(DUPERSID, start=3L, end=-1L)), DUPERSID)) %>% 
    select(-meps_year) %>% 
    inner_join(base_pool) %>% 
    inner_join(scores_eval_norm) %>% 
    mutate(AvgPMPM_MCR = sum(Cost_MCR) / sum(Expos_MCR)) %>% 
    mutate(across(
      .cols = matches("^(CNA|NE)_v(24|28)_(mode|avg)_norm$"),
      .fns = list(pred = function(x) { AvgPMPM_MCR * x}),
      .names = "{.col}_pred"
    ))
  
  proj_expenditures %>% select(-meps_year)
}

# Build 2016-2020 base year projections
# Set min_expos variables to dial into more stringent exposure requirements for comparisons
projections_CNA <- as_tibble(seq(2016,2020,by=1)) %>% 
  rename(meps_year = value) %>% 
  mutate(projections = purrr::map(meps_year, function(x) {test_pred(base_year=x, base_min_expos = 1, proj_min_expos = 1)})) %>% 
  unnest(cols = "projections") %>% 
  write_csv("etc/outputs/predictions_CNA.csv")

# rough r-sq by meps_year
rsq_check <- projections_CNA %>% 
  select(DUPERSID, meps_year, PMPM_Cost_MCR, matches("^(CNA|NE)_v(24|28)_(mode|avg)_norm_pred$")) %>% 
  pivot_longer(
    cols = -c(DUPERSID,meps_year,PMPM_Cost_MCR),
    names_to = c("elig", "model", "calc"),
    names_pattern = "^(CNA|NE)_v(24|28)_(mode|avg)_norm_pred$",
    values_to = "PMPM_Cost_MCR_Pred"
  ) %>% 
  group_by(meps_year, elig, model, calc) %>% 
  summarize(rsq = cor(PMPM_Cost_MCR, PMPM_Cost_MCR_Pred)^2)

rsq_eval <- rsq_check %>% 
  ggplot(aes(x = elig,
             y = rsq,
             fill = calc)) + 
  geom_bar(stat = "identity", position="dodge2") + 
  scale_fill_viridis_d(end = 0.5) +
  scale_y_continuous(labels = scales::label_comma(accuracy=.01)) + 
  facet_grid(meps_year ~ model)

rsq_eval

a2e_check <- projections_CNA %>% 
  select(DUPERSID, meps_year, Cost_MCR, Expos_MCR, matches("^(CNA|NE)_v(24|28)_(mode|avg)_norm_pred$")) %>% 
  mutate(across(
    .cols = matches("^(CNA|NE)_v(24|28)_(mode|avg)_norm_pred$"),
    .fns = list(pmpy = function(x) { Expos_MCR * x}),
    .names = "{.col}_pmpy"
  )) %>% 
  select(DUPERSID, meps_year, Cost_MCR, Expos_MCR, matches("^(CNA|NE)_v(24|28)_(mode|avg)_norm_pred_pmpy$")) %>% 
  pivot_longer(
    cols = -c(DUPERSID,meps_year,Cost_MCR, Expos_MCR),
    names_to = c("elig", "model", "calc"),
    names_pattern = "^(CNA|NE)_v(24|28)_(mode|avg)_norm_pred_pmpy$",
    values_to = "PMPY_Cost_MCR_Pred"
  ) 


a2e_by_model <- a2e_check %>% 
  group_by(meps_year, elig, model, calc) %>% 
  summarize(Tot_Act_PMPM = sum(Cost_MCR) / sum(Expos_MCR),
            Tot_Pred_PMPM = sum(PMPY_Cost_MCR_Pred) / sum(Expos_MCR)) %>% 
  ungroup() %>% 
  mutate(a2e = Tot_Act_PMPM/Tot_Pred_PMPM)

a2e_by_model.plot <- a2e_by_model %>% 
  ggplot(aes(x = elig,
             y = a2e,
             color = calc)) + 
  geom_point(position=position_dodge2(width=.9)) + 
  geom_hline(yintercept = 1, color="black", linetype="dashed") +
  geom_label_repel(aes(label = scales::label_comma(accuracy=0.0001)(a2e)),
                   size = 4, position=position_dodge2(width=.9),
                   min.segment.length = 0) +
  scale_color_viridis_d(end = 0.5) +
  scale_y_continuous(labels = scales::label_comma(accuracy=0.0001)) + 
  facet_grid(meps_year ~ model)

# Prepare demographics file
demos_for_eval <- demographics %>% 
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

a2e_by_age_grp.data <- a2e_check %>% 
  inner_join(demos_for_eval) %>% 
  group_by(meps_year, elig, model, calc, AGE_GRP, SEX) %>% 
  summarize(Tot_Act_PMPM = sum(Cost_MCR) / sum(Expos_MCR),
            Tot_Pred_PMPM = sum(PMPY_Cost_MCR_Pred) / sum(Expos_MCR)) %>% 
  ungroup() %>% 
  mutate(a2e = Tot_Act_PMPM/Tot_Pred_PMPM)

a2e_by_age_grp.plot <- a2e_by_age_grp.data %>% 
  filter(AGE_GRP != "Under 65") %>% 
  filter(calc == "mode") %>% 
  ggplot(aes(x = AGE_GRP,
             y = a2e,
             color = model)) + 
  geom_point(position=position_dodge2(width=.9)) + 
  geom_hline(yintercept = 1, color="black", linetype="dashed") +
  geom_label_repel(aes(label = scales::label_comma(accuracy=0.0001)(a2e)),
                   size = 3, position=position_dodge2(width=.9),
                   min.segment.length = 0) +
  scale_color_viridis_d(end = 0.5) +
  scale_y_continuous(labels = scales::label_comma(accuracy=0.0001)) + 
  facet_grid(meps_year+SEX ~ elig)

a2e_by_age_grp_cna.data <- a2e_by_age_grp.data %>% 
  filter(AGE_GRP != "Under 65") %>% 
  filter(calc == "mode", elig == "CNA") %>% 
  select(-c(elig, calc, a2e)) %>% 
  pivot_wider(names_from = model,
              names_prefix = "Tot_Pred_PMPM_v",
              values_from = Tot_Pred_PMPM) %>% 
  pivot_longer(cols = -c(meps_year, AGE_GRP, SEX),
               names_to = "model_type",
               values_to = "pmpm") %>% 
  mutate(modelName = str_wrap(case_when(
    model_type == "Tot_Pred_PMPM_v24" ~ "CMS-HCC v.24 (Predicted)",
    model_type == "Tot_Pred_PMPM_v28" ~ "CMS-HCC v.28 (Predicted)",
    .default = "Actual"), 12)) %>% 
  mutate(meps_year = str_wrap(paste0(meps_year, " Scores vs. ", meps_year+1, " Actuals"), 15)) %>% 
  mutate(SEX = if_else(SEX == "F", "Female", "Male"))

a2e_by_age_grp_cna.plot <- a2e_by_age_grp_cna.data %>% 
  ggplot(aes(x = AGE_GRP, y = pmpm, fill = modelName)) +
  geom_bar(stat = "identity", position=position_dodge2(width=.9)) +
  geom_label(aes(label = scales::label_dollar(accuracy=1)(pmpm)),
             position=position_dodge2(width=.9), color="white", size=3, vjust=-.5, show.legend=F) +
  scale_fill_viridis_d(end = .75) +
  scale_y_continuous(labels = scales::label_dollar(accuracy=1),
                     limits = c(0, 1800)) +
  facet_grid(meps_year ~ SEX) +
  labs(title = str_wrap("Predicting Healthcare Expenditures of Medicare Respondents in the Medical Expenditure 
                        Panel Survey (MEPS) Public Use Files Using CMS-HCC Risk Score Models",80),
       subtitle = "Actual vs. Predicted Per Member, Per Month (PMPM) Expenditures, 2016 - 2020",
       x = "Age Group",
       y = "Per Member, Per Month (PMPM) Expenditures",
       fill = "Comparison") +
  theme_bw() + 
  theme(legend.position = "bottom")
       
a2e_by_age_grp_cna.plot
