########################################
# 1) Load Libraries
########################################
library(dplyr)
library(lubridate)
library(ggplot2)

########################################
# 2) Process COVID-19 Data
########################################
# Read daily COVID-19 data
covid19_ct <- read.csv("covid19_ct.csv", stringsAsFactors = FALSE)
covid19_ct$date <- as.Date(covid19_ct$date, format = "%Y-%m-%d")
# Create a week identifier (year-week)
covid19_ct$week <- format(covid19_ct$date, "%Y-%U")

# Aggregate to weekly totals for cases and deaths, and compute weekly CFR
weekly_data <- covid19_ct %>% 
  group_by(week) %>% 
  summarise(
    weekly_cases  = sum(cases, na.rm = TRUE),
    weekly_deaths = sum(deaths, na.rm = TRUE)
  ) %>% 
  mutate(weekly_CFR = (weekly_deaths / weekly_cases) * 100)

########################################
# 3) Process Vaccination Data
########################################
# Read vaccination data
ct_data <- read.csv("connecticut_vaccination_data.csv", stringsAsFactors = FALSE)
ct_data$Date <- as.Date(ct_data$Date)
# Assign total population based on date ranges
ct_data <- ct_data %>% 
  mutate(TotalPopulation = case_when(
    Date < as.Date("2021-07-01") ~ 3603448,
    Date < as.Date("2022-07-01") ~ 3605597,
    TRUE ~ 3626205
  )) %>% 
  mutate(
    Vaccination_Rate = (People_at_least_one_dose / TotalPopulation) * 100,
    Fully_Vaccinated_Rate = (People_fully_vaccinated / TotalPopulation) * 100
  )
# Create a week identifier for vaccination data
ct_data$week <- format(ct_data$Date, "%Y-%U")
# For each week, keep the last observation (latest vaccination rate)
weekly_vaccination <- ct_data %>% 
  arrange(Date) %>% 
  group_by(week) %>% 
  summarise(weekly_vac_rate = last(Vaccination_Rate)) %>% 
  mutate(weekly_vac_rate = ifelse(is.na(weekly_vac_rate), 0, weekly_vac_rate))

########################################
# 4) Prepare Weekly Summary Data
########################################
# Create a weekly summary with the start date of the week
weekly_summary <- covid19_ct %>% 
  group_by(week) %>% 
  summarise(week_start = min(date)) %>% 
  left_join(weekly_vaccination, by = "week") %>% 
  arrange(week_start) %>% 
  mutate(weekly_vac_rate = ifelse(is.na(weekly_vac_rate), 0, weekly_vac_rate))
# Merge the outcome data (cases, deaths, CFR) into the weekly summary
analysis_data <- weekly_summary %>% 
  left_join(dplyr::select(weekly_data, week, weekly_cases, weekly_deaths, weekly_CFR), by = "week")

########################################
# 5) Restrict Analysis to the First Two Years
########################################
start_date <- min(analysis_data$week_start)
end_date <- start_date + years(2)
analysis_data <- analysis_data %>% 
  filter(week_start < end_date)

########################################
# 6) Create Time and Intervention Variables
########################################
# Create a full-period time variable: weeks since the start of data
analysis_data <- analysis_data %>% 
  mutate(
    time_since_start = as.numeric(week_start - start_date) / 7  # in weeks
  )
# Define the vaccination rollout date (instant effect date)
rollout_date <- as.Date("2020-12-14")
analysis_data <- analysis_data %>% 
  mutate(
    # Instant effect dummy: 0 before Dec 14, 2020; 1 on/after
    post_rollout = ifelse(week_start >= rollout_date, 1, 0),
    # For weeks after rollout, measure time since rollout in weeks; 0 for pre-rollout
    time_since_rollout = ifelse(post_rollout == 1, as.numeric(week_start - rollout_date) / 7, 0)
  )

########################################
# 7) Create Log-Transformed Outcome Variables
########################################
analysis_data <- analysis_data %>% 
  mutate(
    log_weekly_cases  = log(weekly_cases + 1),
    log_weekly_deaths = log(weekly_deaths + 1),
    log_weekly_CFR    = log(weekly_CFR + 1e-6)  # add a small constant to avoid log(0)
  )

########################################
# 8) Create a Lagged Vaccination Variable (Lag = 2 Weeks)
########################################
analysis_data <- analysis_data %>% arrange(week_start)
analysis_data <- analysis_data %>% 
  mutate(weekly_vac_rate_lag2 = dplyr::lag(weekly_vac_rate, n = 2))

########################################
# 9) Cumulative Effect Models (Post-Rollout, with 2-Week Lag)
########################################
cat("\n=== Cumulative Effect Models (Post-Rollout, 2-Week Lag) ===\n")
# Restrict data to post-rollout period (from Dec 14, 2020 onward)
post_vacc_data <- analysis_data %>% filter(week_start >= rollout_date)

# Cumulative Effect Model for Weekly Cases using 2-week lagged vaccination rate
mod_cases_lag <- lm(log_weekly_cases ~ time_since_rollout + weekly_vac_rate_lag2, data = post_vacc_data)
cat("\nCumulative Effect Model (2-Week Lag) - Weekly Cases\n")
summary(mod_cases_lag)

# Cumulative Effect Model for Weekly Deaths using 2-week lagged vaccination rate
mod_deaths_lag <- lm(log_weekly_deaths ~ time_since_rollout + weekly_vac_rate_lag2, data = post_vacc_data)
cat("\nCumulative Effect Model (2-Week Lag) - Weekly Deaths\n")
summary(mod_deaths_lag)

# Cumulative Effect Model for Weekly CFR using 2-week lagged vaccination rate
mod_cfr_lag <- lm(log_weekly_CFR ~ time_since_rollout + weekly_vac_rate_lag2, data = post_vacc_data)
cat("\nCumulative Effect Model (2-Week Lag) - Weekly CFR\n")
summary(mod_cfr_lag)

########################################
# 10) Compare Pre and Post Vaccine Differences: Descriptive Comparison
########################################
# Split the data into pre-vaccine and post-vaccine periods
pre_vaccine <- analysis_data %>% filter(week_start < rollout_date)
post_vaccine <- analysis_data %>% filter(week_start >= rollout_date)

# Calculate mean values for outcomes in each period
mean_pre_cases   <- mean(pre_vaccine$weekly_cases, na.rm = TRUE)
mean_post_cases  <- mean(post_vaccine$weekly_cases, na.rm = TRUE)
mean_pre_deaths  <- mean(pre_vaccine$weekly_deaths, na.rm = TRUE)
mean_post_deaths <- mean(post_vaccine$weekly_deaths, na.rm = TRUE)
mean_pre_CFR     <- mean(pre_vaccine$weekly_CFR, na.rm = TRUE)
mean_post_CFR    <- mean(post_vaccine$weekly_CFR, na.rm = TRUE)

cat("\n=== Descriptive Pre vs. Post Vaccine Comparison ===\n")
cat("Pre-vaccine Mean Cases: ", mean_pre_cases, "\n")
cat("Post-vaccine Mean Cases: ", mean_post_cases, "\n")
cat("Pre-vaccine Mean Deaths: ", mean_pre_deaths, "\n")
cat("Post-vaccine Mean Deaths: ", mean_post_deaths, "\n")
cat("Pre-vaccine Mean CFR: ", mean_pre_CFR, "\n")
cat("Post-vaccine Mean CFR: ", mean_post_CFR, "\n")

# Conduct t-tests to formally compare the differences between pre and post periods
t_test_cases  <- t.test(weekly_cases ~ post_rollout, data = analysis_data)
t_test_deaths <- t.test(weekly_deaths ~ post_rollout, data = analysis_data)
t_test_CFR    <- t.test(weekly_CFR ~ post_rollout, data = analysis_data)

print(t_test_cases)
print(t_test_deaths)
print(t_test_CFR)

########################################
# 11) Compare Pre and Post Vaccine Differences: Regression with Interaction
########################################
# Use an interaction model to test if the time trend changes after the rollout
mod_interaction <- lm(log_weekly_cases ~ time_since_start * post_rollout, data = analysis_data)
cat("\n=== Regression Interaction Model (Log Weekly Cases) ===\n")
summary(mod_interaction)
