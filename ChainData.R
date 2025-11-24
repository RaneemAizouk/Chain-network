### Codes in the raw data:
# "adm"     → Admission
# "disch"   → Discharge
# "day45"   → Day-45 follow-up
# "day90"   → Day-90 follow-up
# "day180"  → Day-180 follow-up
# "readm"   → Readmission
# "comm"    → Community control
# Discharge is considered hospital setting.
# We have two groups: community controls and followed-up children.
#Carriage status (ESBL negative/positive) → state_esbl (0/1)
#Setting (hospital vs community) → setting_state (0/1)
#Cohort (follow-up vs controls) → group (follow_up / control, or 0/1 if you want)

library(tidyverse)
library(lubridate)
library(readxl)

#------------------------------------------------------
# 1. Load ESBL dataset
#------------------------------------------------------
esbl <- read_excel("/Users/raizouk/Desktop/Oxford/Caroline/data/kenya_esbl copy.xlsx")
names(esbl)

#------------------------------------------------------
# 2. Clean IDs & dates, add readable date column
#    - Keep original date_collect
#    - Create numeric Excel date + actual Date
#------------------------------------------------------
esbl_clean <- esbl %>%
  mutate(
    # Ensure record_id is not numeric (avoid 1e+07 display)
    record_id = as.character(record_id),
    
    # Convert "." or text to NA, then to numeric Excel format
    date_collect_num = suppressWarnings(as.numeric(na_if(date_collect, "."))),
    
    # Convert Excel serial numbers to actual calendar dates
    actual_date = as.Date(date_collect_num, origin = "1899-12-30")
  ) %>%
  # Place actual_date next to date_collect for clarity
  relocate(actual_date, date_collect, .after = time_point)

#------------------------------------------------------
# 3. Number of observations per child
#    - n_obs = how many rows each child has
#    - obs_summary = how many children have 1,2,3,... obs
#------------------------------------------------------
obs_per_child <- esbl_clean %>%
  group_by(record_id) %>%
  summarize(n_obs = n(), .groups = "drop") %>%
  arrange(n_obs)

obs_summary <- obs_per_child %>%
  count(n_obs, name = "num_children")

obs_summary   # frequency of children by number of observations

# Total number of unique individuals
total_children <- esbl_clean %>%
  distinct(record_id) %>%
  nrow()
total_children

#------------------------------------------------------
# 4. Keep only children with ≥ 2 observations
#    (remove single-observation children, who
#     cannot contribute time transitions)
#------------------------------------------------------
esbl_clean_filtered <- esbl_clean %>%
  inner_join(obs_per_child %>% filter(n_obs >= 2), by = "record_id")

# Check filtering worked: all n_obs >= 2
esbl_clean_filtered %>%
  group_by(record_id) %>%
  summarize(n_obs = n(), .groups = "drop") %>%
  arrange(n_obs)

# How many individuals remain
n_distinct(esbl_clean_filtered$record_id)   # e.g. 698
################## 4.1) How many are missing in each group
esbl_clean_filtered <- esbl_clean_filtered %>%
  mutate(
    time_point = case_when(
      time_point == "adm"    ~ "Admission",
      time_point == "disch"  ~ "Discharge",
      time_point == "day45"  ~ "D45",
      time_point == "day90"  ~ "D90",
      time_point == "day180" ~ "D180",
      time_point == "readm"  ~ "Readmission",
      time_point == "comm"   ~ "CommunityControl",
      TRUE ~ time_point
    ),
    group = ifelse(time_point == "CommunityControl", "control", "follow_up")
  )
table(esbl_clean_filtered$group, useNA = "ifany")
n_followup_children <- esbl_clean_filtered %>%
  filter(group == "follow_up") %>%
  distinct(record_id) %>%
  nrow()
visit_counts <- esbl_clean_filtered %>%
  filter(
    group == "follow_up",
    time_point %in% c("Admission", "Discharge", "D45", "D90", "D180")
  ) %>%
  distinct(record_id, time_point) %>%
  count(time_point, name = "n_with_visit") %>%
  mutate(
    n_children_total = n_followup_children,
    n_missing = n_children_total - n_with_visit,
    prop_missing = n_missing / n_children_total
  )

visit_counts
########How Many Remaining Children Have BOTH Admission and Discharge?
adm_disch_pairs <- esbl_clean_filtered %>%
  filter(
    group == "follow_up",
    time_point %in% c("Admission", "Discharge")
  ) %>%
  distinct(record_id, time_point) %>%            # keep one per child per type
  tidyr::pivot_wider(
    names_from = time_point,
    values_from = time_point,
    values_fill = NA
  ) %>%
  mutate(
    has_adm = !is.na(Admission),
    has_disch = !is.na(Discharge)
  ) %>%
  filter(has_adm & has_disch)

# Number of children with BOTH Admission and Discharge
n_adm_disch <- nrow(adm_disch_pairs)
n_adm_disch


#------------------------------------------------------
# 5. Recode timepoints, define groups and setting (hospital vs community)
#    (this keeps your original "idea" block and just
#     applies it to the filtered data)
#------------------------------------------------------
esbl_clean_filtered <- esbl_clean_filtered %>%
  mutate(
    # Recode time_point to readable labels
    time_point = case_when(
      time_point == "adm"    ~ "Admission",
      time_point == "disch"  ~ "Discharge",
      time_point == "day45"  ~ "D45",
      time_point == "day90"  ~ "D90",
      time_point == "day180" ~ "D180",
      time_point == "readm"  ~ "Readmission",
      time_point == "comm"   ~ "CommunityControl",
      TRUE                   ~ time_point  # keep as-is if already recoded
    ),
    
    # Cohort group: community controls vs followed-up children
    group = ifelse(time_point == "CommunityControl", "control", "follow_up"),
    
    
    # Define state for setting: hospital vs community
    # (your original definition preserved)
    setting_state = case_when(
      time_point %in% c("Admission", "Discharge", "Readmission") ~ 0,   # hospital
      time_point %in% c("D45", "D90", "D180") ~ 1,  # community
      TRUE ~ NA_real_
    )
  )
###how to consider readmission is it additional sample /follow up for those ewho have already sampled 
unique(esbl_clean$time_point)
#------------------------------------------------------
# 6. ESBL biological state: 0 = negative, 1 = positive
#------------------------------------------------------
esbl_clean_filtered <- esbl_clean_filtered %>%
  mutate(
    state_esbl = case_when(
      esbl_result == "Negative" ~ 0,
      esbl_result == "Positive" ~ 1,
      TRUE ~ NA_real_
    )
  )

table(esbl_clean_filtered$state_esbl, useNA = "ifany")   # Good check

#------------------------------------------------------
# 7. Sort data by child and chronological time
#    Use actual_date for ordering and dt
#------------------------------------------------------
esbl_clean_filtered <- esbl_clean_filtered %>%
  arrange(record_id, actual_date, time_point)



#------------------------------------------------------
# 9. Markov long dataset 
#    - one row per child per transition
#    - includes dt and previous state and setting
#    - filters out missing states and dt<=0
#------------------------------------------------------
markov_long <- esbl_clean_filtered %>%
  filter(group == "follow_up") %>%          # only followed-up children
  arrange(record_id, actual_date) %>%
  group_by(record_id) %>%
  mutate(
    prev_date    = lag(actual_date),
    state_prev   = lag(state_esbl),
    dt           = as.numeric(actual_date - prev_date),
    setting_prev = lag(setting_state)
  ) %>%
  ungroup() %>%
  filter(
    !is.na(state_prev),
    !is.na(state_esbl),
    !is.na(setting_prev),    #  use the start-of-interval setting
    dt > 0
  ) %>%
  mutate(
    person_id = as.integer(factor(record_id)),
    site_id   = as.integer(factor(site))
  )

table(markov_long$state_prev, markov_long$state_esbl)
summary(markov_long$dt)
length(unique(markov_long$person_id))
##check sites
markov_long %>%
  distinct(site_id, site) %>%
  arrange(site_id)

#------------------------------------------------------
# 10. Final dataset for Stan (markov_stan idea preserved,
#     but now markov_long is already filtered)
#------------------------------------------------------
markov_stan <- markov_long   # already filtered as desired

# Sanity check
length(unique(markov_stan$person_id))

#------------------------------------------------------
# 11. Build Stan data list
#     N = number of transitions (rows in markov_stan)
#     N_person = number of unique children
#     state = 0/1 -> 1/2, as Stan uses 1-based indexing
#------------------------------------------------------
stan_data <- list(
  N        = nrow(markov_stan),##all data observations 
  N_person = length(unique(markov_stan$person_id)),##unique number of individuals 
  N_site   = length(unique(markov_stan$site_id)), #number of sites 
  person_id = markov_stan$person_id,# child id 
  site_id   = markov_stan$site_id,#site id 
  state     = markov_stan$state_esbl + 1,       # carriage state
  dt        = markov_stan$dt,
  setting   = markov_stan$setting_prev          # hospital/community covariate
)

str(stan_data)


table(markov_long$site_id)
markov_long %>% 
  group_by(site) %>%
  summarize(mean_state = mean(state_esbl))
###children per site 
markov_long %>%
  distinct(record_id, site) %>%
  count(site)
#######Observations per site
esbl_clean_filtered %>% count(site)

####Transitions per site:
table(markov_long$site_id)

stan_code <- "functions {
  // Continuous-time 2-state transition matrix
  matrix two_state_Q(real t, real lambda01, real lambda10) {
    real s = lambda01 + lambda10;
    real e = exp(-s * t);

    matrix[2,2] P;

    P[1,1] = lambda10/s + lambda01/s * e;
    P[1,2] = lambda01/s * (1 - e);

    P[2,2] = lambda01/s + lambda10/s * e;
    P[2,1] = lambda10/s * (1 - e);

    return P;
  }
}

data {
  int<lower=1> N;                            // number of transitions (rows)
  array[N] int<lower=1,upper=2> state;       // observed state (1 or 2)
  array[N] real<lower=0> dt;                 // time between visits
  int<lower=1> N_person;                     // number of unique children
  array[N] int<lower=1, upper=N_person> person_id;
  array[N] int<lower=0,upper=1> setting;     // 0 = hospital, 1 = community

  int<lower=1> N_site;                       // NEW: number of sites/villages
  array[N] int<lower=1, upper=N_site> site_id;  // NEW: site index per row
}


parameters {
  // Baseline hazards (community) for acquisition and loss
  real<lower=0> lambda01_base;       // acquisition (0→1) in community
  real<lower=0> lambda10_base;       // loss (1→0) in community

  // Hazard ratios for hospital vs community
  real<lower=0> HR01_hosp;           // HR for acquisition in hospital
  real<lower=0> HR10_hosp;           // HR for loss in hospital

  //  Site-level random effects on log hazard
  vector[N_site] u_site;             // site-specific random effects
  real<lower=0> sigma_site;          // SD of site effects
}


model {
  // Priors (you can refine these)
  lambda01_base ~ normal(0, 1);
  lambda10_base ~ normal(0, 1);
  HR01_hosp ~ lognormal(0, 0.5);
  HR10_hosp ~ lognormal(0, 0.5);
  // priors for village random effects
  u_site ~ normal(0, sigma_site);        // site-level deviations
  sigma_site ~ normal(0, 0.5);           // half-normal prior on SD


  for (n in 2:N) {
    if (person_id[n] == person_id[n-1]) {

      int prev_state = state[n-1];   // previous observed state (1 or 2)
      int curr_state = state[n];     // current state
      int s0 = setting[n];           // 0 = hospital, 1 = community
      int j  = site_id[n];             // site index for this row


      // Log-hazards according to setting
     // Village effect only applies in community
      real log_lambda01;
      real log_lambda10;

      if (s0 == 0) {
      log_lambda01 = log(lambda01_base) + log(HR01_hosp)+u_site[j];    //  u_site added to both setting
      log_lambda10 = log(lambda10_base) + log(HR10_hosp)+u_site[j];
      } else {
      log_lambda01 = log(lambda01_base) + u_site[j];
      log_lambda10 = log(lambda10_base) + u_site[j];
      }
     // then later on we can add village 

      // Convert to positive intensities
      real lambda01 = exp(log_lambda01);
      real lambda10 = exp(log_lambda10);

      // Transition probability matrix over dt[n]
      matrix[2,2] P = two_state_Q(dt[n], lambda01, lambda10);

      // Log-likelihood contribution
      target += log(P[prev_state, curr_state] + 1e-12);

    }
  }
}

generated quantities {
  // Define community & hospital hazards from the parameters
  real lambda01_comm = lambda01_base;
  real lambda10_comm = lambda10_base;
  real lambda01_hosp = lambda01_base * HR01_hosp;
  real lambda10_hosp = lambda10_base * HR10_hosp;

  // Daily rates (for convenience)
  real daily_acquisition_comm = lambda01_comm;
  real daily_clearance_comm   = lambda10_comm;

  real daily_acquisition_hosp = lambda01_hosp;
  real daily_clearance_hosp   = lambda10_hosp;

  // Hazard ratios (hospital vs community)
  real HR_acquisition = HR01_hosp;
  real HR_clearance   = HR10_hosp;

  // Mean durations in community baseline
  real mean_colonised_duration    = 1 / lambda10_comm;
  real mean_uncolonised_duration  = 1 / lambda01_comm;

  // fully model-based overall acquisition & clearance rates
  real total_time      = 0;
  real num_acq_hazard  = 0;
  real num_clr_hazard  = 0;

  for (n in 1:N) {
    int j = site_id[n];

    // site-specific log-hazards in community
    real log_lambda01_comm_site = log(lambda01_comm) + u_site[j];
    real log_lambda10_comm_site = log(lambda10_comm) + u_site[j];

    // site-specific log-hazards in hospital
    real log_lambda01_hosp_site = log_lambda01_comm_site + log(HR01_hosp);
    real log_lambda10_hosp_site = log_lambda10_comm_site + log(HR10_hosp);

    real lambda01_comm_site = exp(log_lambda01_comm_site);
    real lambda10_comm_site = exp(log_lambda10_comm_site);
    real lambda01_hosp_site = exp(log_lambda01_hosp_site);
    real lambda10_hosp_site = exp(log_lambda10_hosp_site);

    // Pick the right hazard for this interval based on setting[n]
    real lambda01_site = (setting[n] == 0)
      ? lambda01_hosp_site
      : lambda01_comm_site;

    real lambda10_site = (setting[n] == 0)
      ? lambda10_hosp_site
      : lambda10_comm_site;

    // Hazard * time contributions
    num_acq_hazard += lambda01_site * dt[n];
    num_clr_hazard += lambda10_site * dt[n];
    total_time     += dt[n];
  }

  real daily_acquisition_overall = num_acq_hazard / total_time;
  real daily_clearance_overall   = num_clr_hazard / total_time;
}



"
stan_model_simple <- rstan::stan_model(model_code = stan_code)

fit <- rstan::sampling(
  object = stan_model_simple,
  data = stan_data,
  iter = 2000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  seed = 2025,
  verbose = TRUE,
  control = list(adapt_delta = 0.99, max_treedepth = 12))

print(fit)




names(esbl_clean_filtered)
rstan::check_hmc_diagnostics(fit)

# lambda01 hospital, lambda01 base comunity 
# lambda = lambda01 base for community 
# lambda = lambda01 base *indicator(1,0) hospital + hospital HR *indicator(1,0) hospital or not
# for (n in 2:N) {
#   if (menage_id_member[n] == menage_id_member[n-1]) {
#     real log12_base = q_1_2_base + u[HouseID[n]] + beta_1_2_age * age[n] + beta_1_2_sexe * sexe[n];
#     real log21_base = q_2_1_base + u[HouseID[n]] + beta_2_1_age * age[n] + beta_2_1_sexe * sexe[n];
#     matrix[2,2] P_total = diag_matrix(rep_vector(1.0, 2));
#     real t_star1 = intervention_date[n];
#     real t_star2 = intervention_date2[n];
#  Calssify the time for the hospital to be discharge -admission only 
# if we dont have discharge we cant do hospital but we can do community 
# we can do comunity if we have 2 observations considering discharge .
# plot seasonality against esble positive 
# check how many are missing for the hospital vs comunity 
################to plot esble per month 
library(dplyr)
library(lubridate)

data_clean <- esbl %>%
  mutate(
    date_collect_num = suppressWarnings(as.numeric(na_if(date_collect, "."))),
    actual_date = as.Date(date_collect_num, origin = "1899-12-30"),
    ESBL_num = ifelse(esbl_result_code == 1, 1, 0),
    month = month(actual_date, label = TRUE, abbr = TRUE)
  ) %>%
  filter(!is.na(actual_date))
monthly_prev <- data_clean %>%
  group_by(month) %>%
  summarise(
    total = n(),
    pos = sum(ESBL_num, na.rm = TRUE),
    prevalence = round((pos / total) * 100, 1)
  )
print(monthly_prev)
library(ggplot2)

ggplot(monthly_prev, aes(x = month, y = prevalence, group = 1)) +
  geom_line(size = 1.2, colour = "steelblue") +
  geom_point(size = 3, colour = "steelblue") +
  geom_text(aes(label = paste0(prevalence, "%")), vjust = -0.5) +
  labs(
    title = "Monthly Variation in ESBL Colonisation",
    x = "Month",
    y = "Prevalence (%)"
  ) +
  theme_minimal()
############Compare Hospital vs Community Seasons
data_clean <- data_clean %>%
  mutate(setting = case_when(
    time_point %in% c("adm", "disch", "readm") ~ "Hospital",
    TRUE ~ "Community"
  ))
####
monthly_setting_prev <- data_clean %>%
  group_by(month, setting) %>%
  summarise(
    total = n(),
    pos = sum(ESBL_num),
    prevalence = round((pos / total) * 100, 1),
    .groups = "drop"
  )
ggplot(monthly_setting_prev, aes(x = month, y = prevalence, colour = setting, group = setting)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Monthly ESBL Prevalence by Setting",
    x = "Month",
    y = "Prevalence (%)",
    colour = "Setting"
  ) +
  theme_minimal()
#######
data_clean <- data_clean %>%
  mutate(season = case_when(
    month(actual_date) %in% 3:5 ~ "Long Rains",
    month(actual_date) %in% 10:12 ~ "Short Rains",
    month(actual_date) %in% c(6:9) ~ "Dry",
    month(actual_date) %in% c(1,2) ~ "Dry"
  ))
###############new, Create acquisition indicator
markov_long <- markov_long %>%
  mutate(acquisition = as.integer(state_prev == 0 & state_esbl == 1))

############# risk of acquisition per setting
acq_risk <- markov_long %>%
  group_by(setting_prev) %>%
  summarise(
    n_transitions = n(),
    n_acquisitions = sum(acquisition),
    risk_acquisition = n_acquisitions / n_transitions
  ) %>%
  mutate(setting = ifelse(setting_prev == 0, "Hospital", "Community"))

acq_risk
######Compute continuous-time rate (events per person-day)
acq_rate <- markov_long %>%
  group_by(setting_prev) %>%
  summarise(
    total_time = sum(dt),
    n_acquisitions = sum(acquisition),
    rate = n_acquisitions / total_time   # per day
  ) %>%
  mutate(setting = ifelse(setting_prev == 0, "Hospital", "Community"))

acq_rate
###Compute rate ratio (H / C)
rate_H <- acq_rate$rate[acq_rate$setting_prev == 0]
rate_C <- acq_rate$rate[acq_rate$setting_prev == 1]

rate_ratio <- rate_H / rate_C
rate_ratio


