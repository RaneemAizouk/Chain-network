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
# 2. Clean IDs & dates
#------------------------------------------------------
esbl_clean <- esbl %>%
  mutate(
    record_id = as.character(record_id),
    date_collect_num = suppressWarnings(as.numeric(na_if(date_collect, "."))),
    actual_date = as.Date(date_collect_num, origin = "1899-12-30")
  ) %>%
  relocate(actual_date, date_collect, .after = time_point) %>%
  filter(!is.na(actual_date),
         actual_date >= as.Date("2016-01-01"))

#------------------------------------------------------
# 3. Observation counts per child
#------------------------------------------------------
obs_per_child <- esbl_clean %>%
  group_by(record_id) %>%
  summarize(n_obs = n(), .groups = "drop") %>%
  arrange(n_obs)

obs_summary <- obs_per_child %>% count(n_obs)
total_children <- n_distinct(esbl_clean$record_id)

#------------------------------------------------------
# 4. Keep children with ≥ 2 samples
#------------------------------------------------------
esbl_clean_filtered <- esbl_clean %>%
  inner_join(obs_per_child %>% filter(n_obs >= 2), by = "record_id")

#------------------------------------------------------
# 5. Recode timepoints
#------------------------------------------------------
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
      TRUE                   ~ time_point
    ),
    group = ifelse(time_point == "CommunityControl", "control", "follow_up")
  )

#------------------------------------------------------
# 6. ESBL state (0/1)
#------------------------------------------------------
esbl_clean_filtered <- esbl_clean_filtered %>%
  mutate(
    state_esbl = case_when(
      esbl_result == "Negative" ~ 0,
      esbl_result == "Positive" ~ 1,
      TRUE ~ NA_real_
    )
  )

#------------------------------------------------------
# 7. Sort chronologically
#------------------------------------------------------
esbl_clean_filtered <- esbl_clean_filtered %>%
  arrange(record_id, actual_date, time_point)

#------------------------------------------------------
# 8. Extract discharge dates
#------------------------------------------------------
discharge_dates <- esbl_clean_filtered %>%
  group_by(record_id) %>%
  summarise(
    discharge_date = case_when(
      any(time_point == "Discharge") ~ min(actual_date[time_point == "Discharge"], na.rm = TRUE),
      any(time_point %in% c("D45", "D90", "D180")) ~ min(actual_date[time_point %in% c("D45", "D90", "D180")], na.rm = TRUE),
      TRUE ~ NA_Date_
    ),
    .groups = "drop"
  )

esbl_clean_filtered <- esbl_clean_filtered %>% left_join(discharge_dates, "record_id")

#------------------------------------------------------
# 9. Pre-split intervals
#------------------------------------------------------
intervals_raw <- esbl_clean_filtered %>%
  filter(group == "follow_up",
         time_point != "Readmission") %>%
  arrange(record_id, actual_date) %>%
  group_by(record_id) %>%
  mutate(
    t_start     = actual_date,
    t_end       = lead(actual_date),
    state_start = state_esbl,
    state_end   = lead(state_esbl),
    tp_start    = time_point
  ) %>%
  ungroup() %>%
  filter(!is.na(t_end))

#------------------------------------------------------
# 10. Interval-splitting function
#------------------------------------------------------
split_interval <- function(row) {
  t1 <- row$t_start
  t2 <- row$t_end
  d  <- row$discharge_date
  
  if (is.na(d) || !(t1 < d & t2 > d)) {
    setting = ifelse(!is.na(d) && t1 < d, 0, 1)
    return(tibble(
      record_id   = row$record_id,
      site        = row$site,
      t_start     = t1,
      t_end       = t2,
      state_start = row$state_start,
      state_end   = row$state_end,
      setting_prev = setting,
      dt = as.numeric(t2 - t1)
    ))
  }
  
  tibble(
    record_id   = row$record_id,
    site        = row$site,
    t_start     = c(t1, d),
    t_end       = c(d,  t2),
    state_start = c(row$state_start, NA),
    state_end   = c(NA, row$state_end),
    setting_prev = c(0, 1),
    dt = c(as.numeric(d - t1), as.numeric(t2 - d))
  )
}

#------------------------------------------------------
# 11. Apply splitting
#------------------------------------------------------
markov_split <- intervals_raw %>%
  rowwise() %>% do(split_interval(.)) %>%
  ungroup() %>%
  filter(dt > 0)

#------------------------------------------------------
# 12. Stan-ready dataset
#------------------------------------------------------
markov_stan <- markov_split %>%
  mutate(
    person_id  = as.integer(factor(record_id)),
    site_id    = as.integer(factor(site)),
    state_prev = state_start + 1,
    state      = state_end + 1
  ) %>%
  filter(!is.na(state_prev), !is.na(state), dt > 0)

#====================================================================
# 13. NEW SECTION — Fraction of children acquiring ESBL-E in hospital
#====================================================================

# (A) Mark acquisition events
markov_split <- markov_split %>%
  mutate(acquisition = as.integer(state_start == 0 & state_end == 1))

# (B) Children at risk in hospital (start uncolonised)
children_at_risk_hosp <- markov_split %>%
  filter(setting_prev == 0, state_start == 0) %>%
  distinct(record_id) %>%
  pull(record_id)

# (C) Children who acquired ESBL-E in hospital
children_acquired_hosp <- markov_split %>%
  filter(setting_prev == 0, acquisition == 1) %>%
  distinct(record_id) %>%
  pull(record_id)

# (D) Fraction
frac_acquired_hosp <- length(children_acquired_hosp) / length(children_at_risk_hosp)

# (E) Summary table
acq_summary <- tibble(
  setting = "Hospital",
  children_at_risk = length(children_at_risk_hosp),
  children_acquired = length(children_acquired_hosp),
  fraction = frac_acquired_hosp,
  percent = round(100 * frac_acquired_hosp, 1)
)

print(acq_summary)

#------------------------------------------------------
# 14. Inspect individuals (optional)
#------------------------------------------------------
esbl_clean_filtered %>%
  filter(record_id == "10003203") %>%
  arrange(actual_date) %>%
  select(record_id, time_point, actual_date, esbl_result, discharge_date)

esbl_clean_filtered %>% 
  filter(record_id == "10003224") %>%
  select(record_id, time_point, actual_date, discharge_date)

stan_code <- "
functions {
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
  int<lower=1> N;
  array[N] int<lower=1,upper=2> state_prev;
  array[N] int<lower=1,upper=2> state;
  array[N] real<lower=0> dt;
  int<lower=1> N_person;
  array[N] int<lower=1,upper=N_person> person_id;
  array[N] int<lower=0,upper=1> setting;
  int<lower=1> N_site;
  array[N] int<lower=1,upper=N_site> site_id;
}

parameters {
  real log_lambda01_base;
  real log_lambda10_base;
  real log_HR01_hosp;
  real log_HR10_hosp;
  vector[N_site] u_site;
  real<lower=0> sigma_site;
}

model {
  log_lambda01_base ~ normal(-3, 1.5);
  log_lambda10_base ~ normal(-3, 1.5);
  log_HR01_hosp ~ normal(0, 0.7);
  log_HR10_hosp ~ normal(0, 0.7);
  u_site ~ normal(0, sigma_site);
  sigma_site ~ exponential(1);

  for (n in 1:N) {
    int prev_state = state_prev[n];
    int curr_state = state[n];
    int s0 = setting[n];
    int j  = site_id[n];

    real log_l01 = log_lambda01_base + u_site[j];
    real log_l10 = log_lambda10_base + u_site[j];

    if (s0 == 0) {
      log_l01 += log_HR01_hosp;
      log_l10 += log_HR10_hosp;
    }

    real lambda01 = exp(log_l01);
    real lambda10 = exp(log_l10);
    matrix[2,2] P = two_state_Q(dt[n], lambda01, lambda10);
    target += log(P[prev_state, curr_state] + 1e-12);
  }
}

generated quantities {
  real lambda01_comm = exp(log_lambda01_base);
  real lambda10_comm = exp(log_lambda10_base);
  real HR_acquisition = exp(log_HR01_hosp);
  real HR_clearance   = exp(log_HR10_hosp);

  real lambda01_hosp = lambda01_comm * HR_acquisition;
  real lambda10_hosp = lambda10_comm * HR_clearance;

  real daily_acquisition_comm = lambda01_comm;
  real daily_clearance_comm   = lambda10_comm;
  real daily_acquisition_hosp = lambda01_hosp;
  real daily_clearance_hosp   = lambda10_hosp;

  real mean_colonised_duration   = 1 / lambda10_comm;
  real mean_uncolonised_duration = 1 / lambda01_comm;

  real total_time = 0;
  real num_acq_hazard = 0;
  real num_clr_hazard = 0;

  for (n in 1:N) {
    int j = site_id[n];
    int s0 = setting[n];

    real log_l01_comm_site = log_lambda01_base + u_site[j];
    real log_l10_comm_site = log_lambda10_base + u_site[j];
    real log_l01_hosp_site = log_l01_comm_site + log_HR01_hosp;
    real log_l10_hosp_site = log_l10_comm_site + log_HR10_hosp;

    real lambda01_comm_site = exp(log_l01_comm_site);
    real lambda10_comm_site = exp(log_l10_comm_site);
    real lambda01_hosp_site = exp(log_l01_hosp_site);
    real lambda10_hosp_site = exp(log_l10_hosp_site);

    real lambda01_site = (s0 == 0) ? lambda01_hosp_site : lambda01_comm_site;
    real lambda10_site = (s0 == 0) ? lambda10_hosp_site : lambda10_comm_site;

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
  iter = 6000,
  warmup = 3000,
  chains = 4,
  cores = 4,
  seed = 2025,
  verbose = TRUE,
  control = list(adapt_delta = 0.9999, max_treedepth = 15)
)

print(fit, digits_summary = 4)
rstan::check_hmc_diagnostics(fit)
print(frac_acquired_hosp)
##########################################


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


