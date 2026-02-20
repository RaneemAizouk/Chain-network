## ============================================================
## ESBL + ABX Full Pipeline — COMPLETE FINAL VERSION
## Author: raneem aizouk
##
## COHORT:     Admission + Discharge + ≥1 of (D45/D90/D180) → N = 329
## TIMEPOINTS: Admission, Discharge, D45, D90, D180 ONLY
## SETTINGS:   Admission = hospital (0); Discharge/D45/D90/D180 = community (1)
##
## ABX COVARIATES (based on correlation analysis):
##   r(aminoglycoside, penicillin) = 0.71 → COMBINED into log_days_1stline
##   r(3rdceph, watch)             = 0.96 → KEEP 3rdceph, DROP watch
##   carbapenem                           → DROPPED (n=1 child only)
##
##   FINAL TWO ABX COVARIATES:
##     log_days_1stline = log(1 + days_aminoglycoside + days_penicillin)
##     log_days_3rdceph = log(1 + days_3rdceph)
##
## HOSPITAL STAY: median 6 days, mean 7.2, range 1–43
## ============================================================

library(tidyverse)
library(lubridate)
library(readxl)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

## ============================================================
## PART 1: LOAD & STANDARDISE
## ============================================================

esbl      <- read_excel("/Users/raizouk/Documents/chain_network/data/kenya_esbl copy.xlsx")
abx_wide  <- read_excel("/Users/raizouk/Documents/chain_network/data/daily_record_sum_wide.xls")
abx_daily <- read_excel("/Users/raizouk/Documents/chain_network/data/daily_review_antibiotics-with dates.xlsx")

esbl      <- esbl      %>% mutate(record_id = as.character(record_id) %>% str_trim())
abx_wide  <- abx_wide  %>% mutate(record_id = as.character(record_id) %>% str_trim())
abx_daily <- abx_daily %>% mutate(record_id = as.character(record_id) %>% str_trim())

message("Raw IDs — ESBL: ", n_distinct(esbl$record_id),
        " | ABX wide: ",    n_distinct(abx_wide$record_id),
        " | ABX daily: ",   n_distinct(abx_daily$record_id))

## ============================================================
## PART 2: CLEAN DATES & MAP TIME POINTS
##         ONLY: Admission, Discharge, D45, D90, D180
## ============================================================

esbl_visits <- esbl %>%
  mutate(
    date_collect     = na_if(as.character(date_collect), "."),
    date_collect_num = suppressWarnings(as.numeric(date_collect)),
    parsed_char_date = suppressWarnings(
      parse_date_time(date_collect,
                      orders = c("ymd","dmy","mdy","Ymd","dmY","mdY"),
                      exact  = FALSE)
    ),
    actual_date = case_when(
      !is.na(date_collect_num) ~ as.Date(date_collect_num, origin = "1899-12-30"),
      !is.na(parsed_char_date) ~ as.Date(parsed_char_date),
      TRUE                     ~ as.Date(NA)
    ),
    tp_l = tolower(as.character(time_point)),
    time_point = case_when(
      grepl("^adm",   tp_l) ~ "Admission",
      grepl("^disch", tp_l) ~ "Discharge",
      grepl("day45",  tp_l) ~ "D45",
      grepl("day90",  tp_l) ~ "D90",
      grepl("day180", tp_l) ~ "D180",
      TRUE                  ~ NA_character_  # Readmission/CommunityControl → dropped
    )
  ) %>%
  filter(!is.na(time_point))

message("IDs after time_point mapping: ", n_distinct(esbl_visits$record_id))
message("Visit type counts:")
print(table(esbl_visits$time_point))

## ============================================================
## PART 3: COHORT DEFINITION
##   Require: Admission (valid date) + Discharge (valid date)
##            + at least one of D45/D90/D180 (valid date)
## ============================================================

cohort_check <- esbl_visits %>%
  group_by(record_id) %>%
  summarise(
    has_adm      = any(time_point == "Admission" & !is.na(actual_date)),
    has_disch    = any(time_point == "Discharge" & !is.na(actual_date)),
    has_followup = any(time_point %in% c("D45","D90","D180") & !is.na(actual_date)),
    n_visits     = n_distinct(time_point),
    which_visits = paste(sort(unique(time_point)), collapse = " -> "),
    .groups = "drop"
  )

# Consort flow
consort <- cohort_check %>%
  summarise(
    n_total               = n(),
    n_has_adm             = sum(has_adm),
    n_adm_and_disch       = sum(has_adm & has_disch),
    n_adm_disch_followup  = sum(has_adm & has_disch & has_followup),
    n_dropped_no_adm      = sum(!has_adm),
    n_dropped_no_disch    = sum(has_adm & !has_disch),
    n_dropped_no_followup = sum(has_adm & has_disch & !has_followup)
  ) %>%
  pivot_longer(everything(), names_to = "criterion", values_to = "n_children")

message("\n=== CONSORT FLOW ===")
print(consort)
write_csv(consort, "consort_flow.csv")

# Visit patterns in final cohort
visit_patterns <- cohort_check %>%
  filter(has_adm & has_disch & has_followup) %>%
  count(which_visits, name = "n_children") %>%
  arrange(desc(n_children))

message("\n=== Visit patterns ===")
print(visit_patterns)

# Observations per child
obs_dist <- cohort_check %>%
  filter(has_adm & has_disch & has_followup) %>%
  count(n_visits, name = "n_children") %>%
  mutate(pct = round(100 * n_children / sum(n_children), 1))

message("\n=== Observations per child ===")
print(obs_dist)

# Final cohort IDs
ids_final <- cohort_check %>%
  filter(has_adm & has_disch & has_followup) %>%
  pull(record_id)

message("\nFINAL COHORT N = ", length(ids_final))

## ============================================================
## PART 4: BUILD esbl_clean_filtered
## ============================================================

esbl_clean_filtered <- esbl_visits %>%
  filter(record_id %in% ids_final) %>%
  mutate(
    state_esbl = case_when(
      tolower(esbl_result) == "negative" ~ 0L,
      tolower(esbl_result) == "positive" ~ 1L,
      TRUE ~ NA_integer_
    ),
    setting_state = case_when(
      time_point == "Admission"                          ~ 0L,
      time_point %in% c("Discharge","D45","D90","D180") ~ 1L,
      TRUE ~ NA_integer_
    )
  ) %>%
  arrange(record_id, actual_date, time_point)

message("\nesbl_clean_filtered: ", nrow(esbl_clean_filtered), " rows, ",
        n_distinct(esbl_clean_filtered$record_id), " children")

# ESBL prevalence by visit (Table A)
esbl_by_visit <- esbl_clean_filtered %>%
  filter(!is.na(state_esbl)) %>%
  group_by(time_point) %>%
  summarise(
    n_children = n_distinct(record_id),
    n_pos      = sum(state_esbl == 1),
    n_neg      = sum(state_esbl == 0),
    pct_pos    = round(100 * mean(state_esbl == 1), 1),
    .groups = "drop"
  ) %>%
  arrange(match(time_point, c("Admission","Discharge","D45","D90","D180")))

message("\n=== TABLE A: ESBL prevalence by visit ===")
print(esbl_by_visit)
write_csv(esbl_by_visit, "table_esbl_by_visit.csv")

## ============================================================
## PART 5: BUILD markov_long
## ============================================================

markov_long <- esbl_clean_filtered %>%
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
    !is.na(setting_prev),
    dt > 0
  ) %>%
  mutate(
    person_id = as.integer(factor(record_id)),
    site_id   = as.integer(factor(site))
  )

message("\n=== markov_long ===")
message("Transition rows: ", nrow(markov_long))
message("Children:        ", n_distinct(markov_long$record_id))
message("dt range (days): ", paste(round(range(markov_long$dt), 1), collapse = " to "))

# Transitions by setting
markov_long %>%
  mutate(label = if_else(setting_prev == 0, "Hospital","Community")) %>%
  count(label) %>%
  print()

# Transition counts by state pair and setting
markov_long %>%
  mutate(
    from = if_else(state_prev == 0, "Neg","Pos"),
    to   = if_else(state_esbl == 0, "Neg","Pos"),
    transition = paste0(from, " -> ", to),
    setting    = if_else(setting_prev == 0, "Hospital","Community")
  ) %>%
  count(setting, transition) %>%
  print()

# Hospital stay duration
hosp_duration <- markov_long %>%
  filter(setting_prev == 0) %>%
  summarise(
    n           = n(),
    median_days = median(dt),
    mean_days   = round(mean(dt), 1),
    sd_days     = round(sd(dt), 1),
    min_days    = min(dt),
    max_days    = max(dt),
    q25         = quantile(dt, 0.25),
    q75         = quantile(dt, 0.75),
    pct_lt3days = round(100 * mean(dt < 3), 1),
    pct_lt5days = round(100 * mean(dt < 5), 1)
  )

message("\n=== Hospital stay duration ===")
print(t(hosp_duration))
write_csv(hosp_duration, "hospital_stay_duration.csv")

# Stay length distribution
markov_long %>%
  filter(setting_prev == 0) %>%
  mutate(group = case_when(
    dt <= 3  ~ "1-3 days",
    dt <= 7  ~ "4-7 days",
    dt <= 14 ~ "8-14 days",
    TRUE     ~ "15+ days"
  )) %>%
  count(group) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

## ============================================================
## PART 6: EMPIRICAL TABLES FOR PAPER
## ============================================================

# TABLE B: Hospital acquisition & clearance
adm_disch_wide <- esbl_clean_filtered %>%
  filter(time_point %in% c("Admission","Discharge"), !is.na(state_esbl)) %>%
  select(record_id, time_point, state_esbl) %>%
  pivot_wider(names_from = time_point, values_from = state_esbl,
              values_fn = first) %>%
  rename(esbl_adm = Admission, esbl_disch = Discharge) %>%
  filter(!is.na(esbl_adm), !is.na(esbl_disch))

hosp_transitions <- adm_disch_wide %>%
  summarise(
    n_pairs        = n(),
    n_adm_neg      = sum(esbl_adm == 0),
    n_acquired     = sum(esbl_adm == 0 & esbl_disch == 1),
    pct_acquired   = round(100 * n_acquired / max(n_adm_neg, 1), 1),
    n_adm_pos      = sum(esbl_adm == 1),
    n_cleared      = sum(esbl_adm == 1 & esbl_disch == 0),
    pct_cleared    = round(100 * n_cleared / max(n_adm_pos, 1), 1),
    n_persistent   = sum(esbl_adm == 1 & esbl_disch == 1),
    pct_persistent = round(100 * n_persistent / max(n_adm_pos, 1), 1)
  )

message("\n=== TABLE B: Hospital acquisition & clearance ===")
print(t(hosp_transitions))
write_csv(hosp_transitions, "table_hospital_transitions.csv")

## ============================================================
## PART 7: ABX EXPOSURE — FINAL COVARIATES
##
## DECISIONS FROM CORRELATION ANALYSIS:
##   aminoglycoside + penicillin  r=0.71 → COMBINE → log_days_1stline
##   3rdceph + watch              r=0.96 → KEEP 3rdceph, DROP watch
##   carbapenem                          → DROP (n=1)
## ============================================================

abx_wide_cohort <- abx_wide %>%
  filter(record_id %in% ids_final) %>%
  rename_with(~str_trim(.))

message("\nChildren in ABX wide matched to cohort: ",
        n_distinct(abx_wide_cohort$record_id))
message("Children with NO ABX record: ",
        length(setdiff(ids_final, abx_wide_cohort$record_id)))

abx_exposure <- abx_wide_cohort %>%
  mutate(
    days_aminoglycoside = coalesce(as.numeric(Aminoglycosides), 0),
    days_penicillin     = coalesce(as.numeric(Penicillins), 0),
    days_3rdceph        = coalesce(as.numeric(`Third-generation cephalosporins`), 0),
    days_access         = coalesce(as.numeric(Access), 0),
    days_watch          = coalesce(as.numeric(Watch), 0),

    # ── TWO FINAL MODEL COVARIATES ────────────────────────────
    log_days_1stline = log1p(days_aminoglycoside + days_penicillin),
    log_days_3rdceph = log1p(days_3rdceph),
    # ──────────────────────────────────────────────────────────

    # Binary flags for descriptive tables only
    any_1stline    = as.integer((days_aminoglycoside + days_penicillin) > 0),
    any_3rdceph    = as.integer(days_3rdceph > 0),
    any_carbapenem = as.integer(coalesce(as.numeric(Carbapenems), 0) > 0),

    abx_line_max = case_when(
      any_carbapenem == 1 ~ 3L,
      any_3rdceph    == 1 ~ 2L,
      TRUE                ~ 1L
    )
  ) %>%
  select(record_id,
         days_aminoglycoside, days_penicillin, days_3rdceph,
         days_access, days_watch,
         log_days_1stline, log_days_3rdceph,
         any_1stline, any_3rdceph, any_carbapenem,
         abx_line_max)

# Confirm low correlation after combining
message("\n=== Correlation after combining (should be low) ===")
print(
  abx_exposure %>%
    select(log_days_1stline, log_days_3rdceph) %>%
    cor(use = "complete.obs") %>%
    round(3)
)

# ABX line distribution
abx_line_dist <- abx_exposure %>%
  count(abx_line_max) %>%
  mutate(
    label = case_when(
      abx_line_max == 1 ~ "1st line only",
      abx_line_max == 2 ~ "2nd line (3rd-gen ceph)",
      abx_line_max == 3 ~ "3rd line (carbapenem)"
    ),
    pct = round(100 * n / sum(n), 1)
  )

message("\n=== ABX line distribution ===")
print(abx_line_dist)
write_csv(abx_line_dist, "abx_line_distribution.csv")

# ABX class patient-days
abx_class_totals <- abx_wide_cohort %>%
  select(-record_id) %>%
  summarise(across(everything(), ~sum(as.numeric(.x), na.rm = TRUE))) %>%
  pivot_longer(everything(),
               names_to  = "antibiotic_class",
               values_to = "total_patient_days") %>%
  filter(!is.na(total_patient_days), total_patient_days > 0) %>%
  arrange(desc(total_patient_days))

message("\n=== ABX class totals (patient-days) ===")
print(abx_class_totals)
write_csv(abx_class_totals, "abx_class_totals.csv")

# TABLE C: acquisition by Watch ABX (descriptive)
acq_by_abx <- adm_disch_wide %>%
  filter(esbl_adm == 0) %>%
  left_join(abx_exposure %>%
              select(record_id, any_3rdceph, days_3rdceph, abx_line_max),
            by = "record_id") %>%
  mutate(any_3rdceph = coalesce(any_3rdceph, 0L)) %>%
  group_by(received_3rdceph = any_3rdceph) %>%
  summarise(
    n_children   = n(),
    n_acquired   = sum(esbl_disch == 1, na.rm = TRUE),
    pct_acquired = round(100 * mean(esbl_disch == 1, na.rm = TRUE), 1),
    median_days  = median(days_3rdceph, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(received_3rdceph = if_else(
    received_3rdceph == 1, "Received 3rd-gen ceph", "No 3rd-gen ceph"
  ))

message("\n=== TABLE C: ESBL acquisition by 3rd-gen ceph use ===")
print(acq_by_abx)
write_csv(acq_by_abx, "table_acq_by_3rdceph.csv")

# TABLE C2: acquisition by ABX line
acq_by_line <- adm_disch_wide %>%
  filter(esbl_adm == 0) %>%
  left_join(abx_exposure %>% select(record_id, abx_line_max), by = "record_id") %>%
  mutate(abx_line_max = coalesce(abx_line_max, 1L)) %>%
  group_by(abx_line_max) %>%
  summarise(
    n            = n(),
    n_acquired   = sum(esbl_disch == 1, na.rm = TRUE),
    pct_acquired = round(100 * mean(esbl_disch == 1, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  mutate(label = case_when(
    abx_line_max == 1 ~ "1st line only (Ampicillin + Gentamicin)",
    abx_line_max == 2 ~ "2nd line (3rd-gen cephalosporin added)",
    abx_line_max == 3 ~ "3rd line (carbapenem)"
  ))

message("\n=== TABLE C2: ESBL acquisition by ABX line ===")
print(acq_by_line)
write_csv(acq_by_line, "table_acq_by_abx_line.csv")

# Dose-response: acquisition by days of 3rd-gen ceph
acq_by_ceph_days <- adm_disch_wide %>%
  filter(esbl_adm == 0) %>%
  left_join(abx_exposure %>% select(record_id, days_3rdceph), by = "record_id") %>%
  mutate(
    days_3rdceph = coalesce(days_3rdceph, 0),
    group = case_when(
      days_3rdceph == 0  ~ "0 days (none)",
      days_3rdceph <= 3  ~ "1-3 days",
      days_3rdceph <= 7  ~ "4-7 days",
      TRUE               ~ "8+ days"
    ),
    group = factor(group,
                   levels = c("0 days (none)","1-3 days","4-7 days","8+ days"))
  ) %>%
  group_by(group) %>%
  summarise(
    n            = n(),
    n_acquired   = sum(esbl_disch == 1, na.rm = TRUE),
    pct_acquired = round(100 * mean(esbl_disch == 1, na.rm = TRUE), 1),
    .groups = "drop"
  )

message("\n=== Dose-response: acquisition by ceph days ===")
print(acq_by_ceph_days)
write_csv(acq_by_ceph_days, "acq_by_ceph_days.csv")

## ============================================================
## PART 8: MERGE ABX INTO markov_long
## ============================================================

# Remove any old ABX columns from previous runs
old_cols <- c(
  "days_aminoglycoside","days_penicillin","days_3rdceph","days_access","days_watch",
  "log_days_1stline","log_days_3rdceph","log_days_1stline_hosp","log_days_3rdceph_hosp",
  "any_1stline","any_3rdceph","any_carbapenem","abx_line_max",
  "any_watch","any_aminogly","any_penicillin","log_days_watch","log_days_watch_hosp",
  "abx_watch_hosp","abx_3rdceph_hosp","abx_carbapenem_hosp","abx_line_hosp"
)
markov_long <- markov_long %>% select(-any_of(old_cols))

markov_long <- markov_long %>%
  left_join(abx_exposure, by = "record_id") %>%
  mutate(
    # Active ONLY in hospital intervals (setting_prev == 0)
    # Children with no ABX record → 0 via coalesce (treated as unexposed)
    log_days_1stline_hosp = if_else(
      setting_prev == 0L, coalesce(log_days_1stline, 0), 0
    ),
    log_days_3rdceph_hosp = if_else(
      setting_prev == 0L, coalesce(log_days_3rdceph, 0), 0
    )
  )

message("\n=== NAs in ABX columns (must all be 0) ===")
print(colSums(is.na(markov_long %>%
  select(log_days_1stline_hosp, log_days_3rdceph_hosp))))

message("\n=== ABX covariate summary in hospital intervals ===")
markov_long %>%
  filter(setting_prev == 0) %>%
  select(log_days_1stline_hosp, log_days_3rdceph_hosp) %>%
  summary() %>%
  print()

## ============================================================
## PART 9: STAN DATA LIST
## ============================================================

stan_data_abx <- list(
  N          = nrow(markov_long),
  N_person   = length(unique(markov_long$person_id)),
  N_site     = length(unique(markov_long$site_id)),
  person_id  = as.integer(markov_long$person_id),
  site_id    = as.integer(markov_long$site_id),
  state_prev = as.integer(markov_long$state_prev + 1),   # 0/1 -> 1/2
  state      = as.integer(markov_long$state_esbl + 1),   # 0/1 -> 1/2
  dt         = as.numeric(markov_long$dt),
  setting    = as.integer(markov_long$setting_prev),      # 0=hospital, 1=community
  log_days_1stline = as.numeric(markov_long$log_days_1stline_hosp),
  log_days_3rdceph = as.numeric(markov_long$log_days_3rdceph_hosp)
)

message("\n=== Stan data checks ===")
message("N (total transitions):          ", stan_data_abx$N)
message("N_person:                       ", stan_data_abx$N_person)
message("N_site:                         ", stan_data_abx$N_site)
message("Hospital transitions:           ", sum(stan_data_abx$setting == 0))
message("Community transitions:          ", sum(stan_data_abx$setting == 1))
message("Hosp intervals with 1stline>0:  ", sum(stan_data_abx$log_days_1stline > 0))
message("Hosp intervals with 3rdceph>0:  ", sum(stan_data_abx$log_days_3rdceph > 0))
message("Any NA?                         ",
        any(sapply(stan_data_abx, function(x) any(is.na(x)))))

## ============================================================
## PART 10: STAN MODEL
## ============================================================

stan_code_abx <- "
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
  array[N] int<lower=0,upper=1> setting;
  int<lower=1> N_site;
  array[N] int<lower=1,upper=N_site> site_id;
  int<lower=1> N_person;
  array[N] int<lower=1,upper=N_person> person_id;
  array[N] real<lower=0> log_days_1stline;
  array[N] real<lower=0> log_days_3rdceph;
}
parameters {
  real log_lambda01_base;
  real log_lambda10_base;
  real log_HR01_hosp;
  real log_HR10_hosp;
  real beta_1stline_01;
  real beta_3rdceph_01;
  real beta_1stline_10;
  real beta_3rdceph_10;
  vector[N_site] u_site;
  real<lower=0> sigma_site;
}
model {
  log_lambda01_base ~ normal(-3, 1.5);
  log_lambda10_base ~ normal(-3, 1.5);
  log_HR01_hosp     ~ normal(0, 0.7);
  log_HR10_hosp     ~ normal(0, 0.7);
  beta_1stline_01   ~ normal(0, 0.7);
  beta_3rdceph_01   ~ normal(0, 0.7);
  beta_1stline_10   ~ normal(0, 0.7);
  beta_3rdceph_10   ~ normal(0, 0.7);
  u_site     ~ normal(0, sigma_site);
  sigma_site ~ exponential(1);

  for (n in 1:N) {
    int j = site_id[n];
    real log_l01 = log_lambda01_base + u_site[j];
    real log_l10 = log_lambda10_base + u_site[j];

    if (setting[n] == 0) {
      log_l01 += log_HR01_hosp;
      log_l10 += log_HR10_hosp;
    }

    // ABX effects: non-zero only in hospital intervals (enforced in R)
    log_l01 += beta_1stline_01 * log_days_1stline[n]
             + beta_3rdceph_01 * log_days_3rdceph[n];

    log_l10 += beta_1stline_10 * log_days_1stline[n]
             + beta_3rdceph_10 * log_days_3rdceph[n];

    matrix[2,2] P = two_state_Q(dt[n], exp(log_l01), exp(log_l10));
    target += log(P[state_prev[n], state[n]] + 1e-12);
  }
}
generated quantities {
  // Community baseline hazards
  real lambda01_comm = exp(log_lambda01_base);
  real lambda10_comm = exp(log_lambda10_base);

  // Hospital hazard ratios (vs community baseline, no ABX)
  real HR_hosp_acquisition = exp(log_HR01_hosp);
  real HR_hosp_clearance   = exp(log_HR10_hosp);

  // ABX HRs: per unit increase in log(1+days)
  real HR_1stline_acquisition = exp(beta_1stline_01);
  real HR_3rdceph_acquisition = exp(beta_3rdceph_01);
  real HR_1stline_clearance   = exp(beta_1stline_10);
  real HR_3rdceph_clearance   = exp(beta_3rdceph_10);

  // Predicted HRs at clinically meaningful durations
  real HR_3rdceph_5days  = exp(beta_3rdceph_01 * log1p(5.0));
  real HR_3rdceph_10days = exp(beta_3rdceph_01 * log1p(10.0));
  real HR_1stline_7days  = exp(beta_1stline_01 * log1p(7.0));

  // Mean durations in community (days)
  real mean_colonised_duration   = 1.0 / lambda10_comm;
  real mean_uncolonised_duration = 1.0 / lambda01_comm;

  // Hospital hazards
  real lambda01_hosp_baseline = lambda01_comm * HR_hosp_acquisition;
  real lambda10_hosp_baseline = lambda10_comm * HR_hosp_clearance;

  // Overall hazard-weighted rates
  real total_time     = 0;
  real num_acq_hazard = 0;
  real num_clr_hazard = 0;

  for (n in 1:N) {
    int j  = site_id[n];
    int s0 = setting[n];
    real log_l01 = log_lambda01_base + u_site[j];
    real log_l10 = log_lambda10_base + u_site[j];
    if (s0 == 0) {
      log_l01 += log_HR01_hosp;
      log_l10 += log_HR10_hosp;
    }
    log_l01 += beta_1stline_01 * log_days_1stline[n]
             + beta_3rdceph_01 * log_days_3rdceph[n];
    log_l10 += beta_1stline_10 * log_days_1stline[n]
             + beta_3rdceph_10 * log_days_3rdceph[n];
    num_acq_hazard += exp(log_l01) * dt[n];
    num_clr_hazard += exp(log_l10) * dt[n];
    total_time     += dt[n];
  }

  real daily_acquisition_overall = num_acq_hazard / total_time;
  real daily_clearance_overall   = num_clr_hazard / total_time;
}
"

## ============================================================
## PART 11: FIT
## ============================================================

stan_model_abx <- rstan::stan_model(model_code = stan_code_abx)

fit_abx <- rstan::sampling(
  object  = stan_model_abx,
  data    = stan_data_abx,
  iter    = 6000,
  warmup  = 3000,
  chains  = 4,
  cores   = 4,
  seed    = 2025,
  control = list(adapt_delta = 0.9999, max_treedepth = 15)
)

## ============================================================
## PART 12: RESULTS
## ============================================================

params_of_interest <- c(
  "lambda01_comm", "lambda10_comm",
  "HR_hosp_acquisition", "HR_hosp_clearance",
  "HR_1stline_acquisition", "HR_3rdceph_acquisition",
  "HR_1stline_clearance",   "HR_3rdceph_clearance",
  "HR_3rdceph_5days", "HR_3rdceph_10days", "HR_1stline_7days",
  "mean_colonised_duration", "mean_uncolonised_duration",
  "daily_acquisition_overall", "daily_clearance_overall",
  "lambda01_hosp_baseline",
  "sigma_site"
)

print(fit_abx, pars = params_of_interest, digits_summary = 4)

results_table <- as.data.frame(
  rstan::summary(fit_abx, pars = params_of_interest)$summary
) %>%
  rownames_to_column("parameter") %>%
  select(parameter, mean, `2.5%`, `50%`, `97.5%`, n_eff, Rhat) %>%
  mutate(across(where(is.numeric), ~round(.x, 4)))

message("\n=== Final model results ===")
print(results_table)
write_csv(results_table, "esbl_abx_final_results.csv")

# Convergence check
bad_conv <- results_table %>% filter(Rhat > 1.01 | n_eff < 400)
if (nrow(bad_conv) > 0) {
  message("\nWARNING — convergence issues detected:")
  print(bad_conv)
} else {
  message("\nAll parameters converged (Rhat < 1.01, n_eff > 400)")
}

# Trace plots
rstan::traceplot(fit_abx,
                 pars = c("log_lambda01_base","log_lambda10_base",
                           "log_HR01_hosp","log_HR10_hosp",
                           "beta_1stline_01","beta_3rdceph_01",
                           "sigma_site"))

# Forest plot
rstan::plot(fit_abx,
            pars = c("HR_hosp_acquisition","HR_hosp_clearance",
                     "HR_1stline_acquisition","HR_3rdceph_acquisition",
                     "HR_1stline_clearance","HR_3rdceph_clearance",
                     "HR_3rdceph_5days","HR_3rdceph_10days"))

## ============================================================
## PART 13: FORMATTED SUMMARY FOR PAPER
## ============================================================

get_post <- function(param) {
  r <- results_table %>% filter(parameter == param)
  if (nrow(r) == 0) return("NOT FOUND")
  sprintf("%.3f (95%% CrI: %.3f-%.3f)", r$mean, r$`2.5%`, r$`97.5%`)
}

message("\n=== KEY NUMBERS FOR PAPER ===")
message("---COMMUNITY BASELINE---")
message("Acquisition rate (lambda01_comm):      ", get_post("lambda01_comm"), " per day")
message("Clearance rate   (lambda10_comm):      ", get_post("lambda10_comm"), " per day")
message("Mean duration of ESBL carriage:        ", get_post("mean_colonised_duration"), " days")
message("Mean duration ESBL-free:               ", get_post("mean_uncolonised_duration"), " days")
message("")
message("---HOSPITAL vs COMMUNITY---")
message("HR hospital acquisition:               ", get_post("HR_hosp_acquisition"))
message("HR hospital clearance:                 ", get_post("HR_hosp_clearance"))
message("")
message("---ABX EFFECTS ON ACQUISITION---")
message("HR 1st-line (per log-unit days):       ", get_post("HR_1stline_acquisition"))
message("HR 3rd-gen ceph (per log-unit days):   ", get_post("HR_3rdceph_acquisition"))
message("HR 3rd-gen ceph at 5 days:             ", get_post("HR_3rdceph_5days"))
message("HR 3rd-gen ceph at 10 days:            ", get_post("HR_3rdceph_10days"))
message("HR 1st-line at 7 days:                 ", get_post("HR_1stline_7days"))
message("")
message("---OVERALL RATES---")
message("Overall daily acquisition:             ", get_post("daily_acquisition_overall"))
message("Overall daily clearance:               ", get_post("daily_clearance_overall"))
message("Site-level SD (sigma_site):            ", get_post("sigma_site"))

## ============================================================
## END
## ============================================================
