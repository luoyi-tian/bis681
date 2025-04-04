library(readr)
us_data <- read_csv("C:/Users/zlsha/Downloads/time_series_covid19_vaccine_us.csv")
us_admin_data <- read_csv("C:/Users/zlsha/Downloads/time_series_covid19_vaccine_doses_admin_US.csv")

us_admin_data = us_admin_data  %>%
  filter(Province_State == "Connecticut")
#View(time_series_covid19_vaccine_us)
us_admin_data = t(us_admin_data[,-c(1:12)])
View(us_admin_data)


library(tidyverse)
us_data = us_data %>%
  filter(Province_State == "Connecticut") %>%
  select(Date, Doses_admin, People_at_least_one_dose,People_fully_vaccinated, Total_additional_doses)

us_data

