library(tidyverse)
library(lubridate)
library(zoo)

# 1) Load, prep & restrict to Mar 8 2020–Mar 8 2022
ct <- read_csv("covid19_ct.csv") %>%
  mutate(date = as.Date(date)) %>%
  arrange(date) %>%
  filter(date >= as.Date("2020-03-08") &
           date <= as.Date("2022-03-08"))

# 2) Weekly first‑dose anchors (counts)
weekly_anchors <- tribble(
  ~date,        ~first_doses,
  "2021-01-07",     99929L,
  "2021-01-14",    154994L,
  "2021-01-21",    226930L,
  "2021-01-28",    299876L,
  "2021-02-11",    417644L
) %>% mutate(date = as.Date(date))

# 3) Stamp into `vaccinated` up through Feb 15
ct <- ct %>%
  left_join(weekly_anchors, by = "date") %>%
  arrange(date) %>%
  mutate(
    vaccinated = if_else(
      is.na(vaccinated) & date <= as.Date("2021-02-15"),
      na.locf(first_doses / 3603448 * 100, na.rm = FALSE),
      vaccinated
    )
  ) %>%
  select(-first_doses)

# 4) Zero‑out pre‑vaccine & LOCF
first_vax_date <- as.Date("2020-12-14")
ct <- ct %>%
  mutate(
    vaccinated = if_else(date < first_vax_date, 0L, vaccinated),
    vaccinated = na.locf(vaccinated, na.rm = FALSE)
  )

# 5) Piecewise CT population helper
pop_by_date <- function(d) {
  case_when(
    d < as.Date("2021-07-01") ~ 3603448,
    d < as.Date("2022-07-01") ~ 3605597,
    TRUE                      ~ 3626205
  )
}

# 6) Official anchors & published_pct
anchors <- tribble(
  ~date,          ~first_doses, ~pct,
  "2020-12-14",       0L,         NA,
  "2021-01-04",   75180L,         NA,
  "2021-02-16",  473784L,         NA,
  "2021-02-22",  547536L,         NA,
  "2021-03-04",  681488L,         NA,
  "2021-05-20",2068087L,         NA,
  "2021-07-13",       NA,        67,
  "2021-08-26",2438692L,         NA,
  "2021-09-30",2534453L,         NA,
  "2021-11-24",2732521L,         NA,
  "2022-02-03",2976121L,         NA
) %>%
  mutate(
    date          = as.Date(date),
    pop           = pop_by_date(date),
    published_pct = if_else(is.na(pct),
                            first_doses / pop * 100,
                            pct)
  ) %>%
  select(date, published_pct)

# 7) Back‑fill & interpolate
df_anchor_raw <- anchors %>%
  left_join(ct %>% select(date, vaccinated), by = "date") %>%
  arrange(date) %>%
  mutate(
    vaccinated_filled = if_else(
      is.na(vaccinated) & !is.na(published_pct),
      published_pct/100 * pop_by_date(date),
      vaccinated
    ),
    vaccinated_interp = na.approx(vaccinated_filled, x = date, rule = 2)
  )

# 8) Compute scaling factors
df_factors <- df_anchor_raw %>%
  transmute(date, factor = published_pct / vaccinated_interp)

# 9) Merge & scale
df_scaled <- ct %>%
  left_join(df_factors, by = "date") %>%
  arrange(date) %>%
  mutate(
    factor     = na.locf(factor, na.rm = FALSE),
    factor     = na.locf(factor, fromLast = TRUE, na.rm = FALSE),
    factor     = na.approx(factor, x = date, rule = 2),
    scaled_pct = vaccinated * factor
  )

# 10) Overwrite early scaled_pct with the flat‑week pct
df_final <- df_scaled %>%
  mutate(
    scaled_pct = if_else(
      date <= as.Date("2021-02-15"),
      vaccinated,
      scaled_pct
    )
  )

# 11) Export
write_csv(df_final, "ct_vax_scaled.csv")
