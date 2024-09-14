
# Check differences between average vs. mode 

# Assuming `risk_score` and `mode_hcc_scores` are your two data frames
# Here are the non-numeric fields
non_numeric_fields <- c("DUPERSID", "meps_year", "Run_ID", "model", "orec_args", "elig_args", "mcaid_args")

# Join the tables on the non-numeric fields
joined_data <- risk_scores %>%
  inner_join(mode_hcc_scores, by = non_numeric_fields, suffix = c("_risk", "_mode"))

# Create new variables for the differences between the numeric fields
joined_data <- joined_data %>%
  mutate(
    risk_score_diff = risk_score_risk - risk_score_mode,
    risk_score_age_diff = risk_score_age_risk - risk_score_age_mode,
    risk_score_adj_diff = risk_score_adj_risk - risk_score_adj_mode,
    risk_score_age_adj_diff = risk_score_age_adj_risk - risk_score_age_adj_mode
  )

# Optionally, you can study the distributions of these differences using summary statistics or visualizations
summary(joined_data %>% select(ends_with("_diff")))

long_data <- joined_data %>%
  select(all_of(non_numeric_fields), ends_with("_diff")) %>%
  pivot_longer(
    cols = c(risk_score_diff, risk_score_age_diff, risk_score_adj_diff, risk_score_age_adj_diff),
    names_to = "score_type",
    values_to = "diff"
  ) 


joined_data.plot <- long_data %>% 
  filter(abs(diff) > .01) %>% 
  filter(score_type == "risk_score_diff") %>% 
  ggplot(aes(x = diff,
             fill = score_type)) +
  geom_histogram(binwidth = .125) + 
  scale_y_continuous(labels = scales::label_comma()) +
  #scale_x_log10() +
  theme_bw() + 
  facet_grid(model ~ elig_args) + 
  scale_fill_viridis_d() 

joined_data.plot

outliers <- long_data %>% 
  filter(abs(diff) > .01) %>% 
  filter(score_type == "risk_score_diff") %>% 
  select(DUPERSID, meps_year, model, elig_args, Run_ID, score_type, diff)


# insert map code here to process en bloc

test_record <- process_scores$data[[100]]

test_record_out <- cms_hcc_run(test_record)
test_record_out.rs <- test_record_out$risk_scores
test_record_out.hccmap <- test_record_out$mode_hcc_mapping
test_record_out.hccvalues <- test_record_out$mode_hcc_values
test_record_out.modescores <- test_record_out$mode_hcc_scores

# Now organize the data output for easy filtering later
v24_intermediate <- scored_data.json %>% 
  arrange(ID, Run_ID) %>% 
  mutate(model_out = map(`CMS-HCC`, parse_json, .progress=T)) %>% 
  unnest_wider(model_out) %>% 
  left_join(cross_all_args, by=c("Run_ID"="run_index"))

cmshcc_scores <- v24_intermediate %>% 
  select(ID, Run_ID, starts_with("risk_score"), model) %>% 
  
  
  scores_join <- cmshcc_scores %>% 
  filter(Run_ID == 1) %>%
  left_join(make_contingencies, by=c("ID"="id")) %>% 
  left_join(contingencies_to_run, by=c("ID"="id"))

foo <- scores_join %>% 
  distinct(DUPERSID, meps_year, AGE_EXACT, SEX, risk_score, risk_score_age) %>% 
  arrange(desc(risk_score))
# Cartesian products get super large, so we'll need to run risk scores for each tibble one at a time, probably.

# Apply the function to each group
results <- data %>%
  group_by(DUPERSID, meps_year) %>%
  nest() %>%
  mutate(cartesian_results = map(data, calculate_cartesian_products)) %>%
  select(-data) %>%
  unnest(cartesian_results)

# Check if the sum of probabilities for any one individual equals 1
prob_sum_check <- results %>%
  group_by(DUPERSID, meps_year) %>%
  summarise(total_probability = sum(probability)) %>%
  mutate(sum_equals_1 = abs(total_probability - 1) < 1e-6)

# Output the results
print(results)
print(prob_sum_check)

# Next we will pick a number of simulations to run relative to the size
# of the sample weighting assigned to the MEPS respondent.
# This means n draws
# for how each 3-digit ICD code is completed based on our frequency distr.

# Get sample weights and rescale - this will inform the number of trials
meps_weights <- fyc_data_proc %>% 
  select(DUPERSID, meps_year, PERWTYYF) %>% 
  mutate(POOLWTYYF = PERWTYYF / 6) %>% 
  select(-PERWTYYF)

# Perform 1 million trials of different combos of ICDs. More heavily weighted
# individuals in the MEPS data will go through more trials, ensuring that
# we 
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
  replace(.=="NULL", "")  %>%  # replace any NULLs with empty strings
  replace_na(list(freq=1)) %>% # any NULLs here are assumed to be single-frequency profile runs
  group_by(id) %>%
  mutate(profile_no = row_number()) %>%
  ungroup() %>%
  unite(col=id, id, profile_no, sep="-")


# Lastly, write this out as a R data file for input into our HHS-HCC risk score project. 
save(final_input_data, file="./etc/meps_hcc_model_inputs.RData")

# Now let's tidy up
rm(icd10_ccsr3_map)
rm(fyc_data_proc)
rm(fyc_datasets)
rm(fyc_data)
rm(meps_weights)
rm(ca_icd10_freqs)
rm(cond_data)
rm(censored_code_individuals)
rm(censored_rx_individuals)
rm(conds_w_freqs)
rm(cond_datasets)
rm(cond_datasets_proc)
rm(consolidated_conds_probs)
rm(rx_data)
rm(rx_datasets)
rm(rx_datasets_proc)
rm(dx_desc)
rm(input)
rm(final_dx_data)
rm(final_rx_data)
rm(final_input_data)
gc()
