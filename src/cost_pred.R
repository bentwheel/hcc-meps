
library(tidyverse)
library(viridis)
library(ggrepel)

# No scientific notation
options(scipen = 999)

# Load in demographics file used for RA model demo inputs
demographics <- read_csv(here::here("etc/outputs/demographics.csv")) %>% 
  mutate(DUPERSID = as.character(DUPERSID),
  meps_year = as.character(meps_year))

# Load in risk scores
risk_scores <- read_csv(here::here("etc/outputs/cna_population_scores.csv"))

#### Develop Cost Predictions ####
# Get Actual Medicare costs for the subsequent year, for each year, for each member in the CNA population
cna_pop_pmpm_act <- read_csv(here::here("etc/outputs/expenditures_pmpm.csv")) %>% 
  filter(Type == "MCR") %>% 
  filter(meps_year != 2016) %>%  # We don't have 2015 prospective scores to scale with 2016 PMPM totals in this dataset
  mutate(meps_year = meps_year - 1) %>% # We want to apply 2017 PMPM averages to 2016 scores, 2018 PMPM avg to 2017 scores, etc.
  inner_join(risk_scores %>% distinct(DUPERSID, meps_year)) # Filtration join for correct PMPM calcs 
 
# Develop PMPM averages by year for risk score scaling
average_pmpm_cna_pop <- cna_pop_pmpm_act %>% 
  group_by(meps_year) %>% 
  summarize(total_exp_months = sum(Expos, na.rm=T),
            total_cost = sum(Cost, na.rm=T),
            total_cost_norx = sum(Cost_noRX, na.rm=T)) %>% 
  mutate(avg_cost_pmpm = total_cost / total_exp_months,
         avg_cost_pmpm_norx = total_cost_norx / total_exp_months)

# Scale risk scores
cna_pop_pmpm_pred <- risk_scores %>% 
  left_join(average_pmpm_cna_pop %>% select(meps_year, avg_cost_pmpm_actual = avg_cost_pmpm)) %>% 
  mutate(cost_pmpm_pred = avg_cost_pmpm_actual * score_norm) %>% 
  left_join(cna_pop_pmpm_act %>% select(DUPERSID, meps_year, cost_pmpm_act = PMPM_Cost)) %>% 
  mutate(meps_year = meps_year + 1) %>%  # Post-join, this should reflect accurate year for cost actuals vs. predictions
  write_csv(here::here("etc/outputs/cna_population_pred_costs.csv"))

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

esrd_individuals <- read_csv(here::here("etc/outputs/esrd_individuals.csv")) %>% 
  mutate(esrd_dx = T)

#### R-Squared Plot for NE Models by MEPS Year ####
# Evaluate r-sq by meps_year for NE. Should focus only on the pop. for which the NE is intended.
rsq_eval_NE.data <- cna_pop_pmpm_pred %>% 
  filter(model == "NE") %>% 
  group_by(meps_year, version) %>% 
  summarize(rsq = cor(cost_pmpm_act, cost_pmpm_pred)^2,
            count = n()) %>% 
  ungroup() %>% 
  mutate(meps_year = meps_year - 1) #Since this is an evaluation of scores, more intuitive to frame in terms of base year
  
rsq_eval_NE.plot <- rsq_eval_NE.data %>% 
  mutate(meps_year = fct_inorder(paste0(meps_year, "\n(n = ", scales::label_comma(accuracy=1)(count), ")"))) %>% 
  mutate(model = paste0("CMS-HCC v", version, " NE")) %>% 
  ggplot(aes(x = meps_year,
             y = rsq,
             fill = model)) +
  geom_bar(stat = "identity", position="dodge2",
           color="gray20") + 
  geom_text(aes(label=scales::label_percent(accuracy=.01)(rsq)),
            position=position_dodge2(width=.9), vjust=-.8, size=2.2) +
  theme_classic() +
  scale_fill_viridis_d(end = 0.5) +
  scale_y_continuous(labels = scales::label_percent(accuracy=.1)) +
  labs(title = "Predictive Performance of CMS-HCC New Enrollee Models",
       subtitle = "Using Medical Expenditure Panel Data (MEPS) PUFs Datasets as Inputs",
       y = "R-Squared",
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
