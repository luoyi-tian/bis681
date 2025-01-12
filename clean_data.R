rm(list=ls())


library(tidyverse)
library(lubridate)


cases = read_csv("raw/cases.csv") 
cases$date = mdy(cases$Date)
cases = cases %>% 
         select(date, "Total Cases", "Total Deaths") %>% 
         rename(cases=`Total Cases`, deaths=`Total Deaths`) 


vax = read_csv("raw/vaccinations.csv") %>%
        filter(`County of residence` != "Residence out of state", 
               `County of residence` != "Resident out of state", 
               `County of residence` != "Address pending validation")

vax$date = mdy(vax$Date)
vax = vax %>% group_by(date) %>%
              summarize(vaccinated=sum(`At least one dose percent`)) %>%
              mutate(vaccinated=vaccinated/6) %>% 
              select(date, vaccinated) %>%
              arrange(date)

dat = left_join(cases, vax, by="date")
write_csv(dat, "covid19_ct.csv")

p1 = ggplot(dat, aes(x=date, y=cases)) + geom_line()
ggsave(p1, filename="img/cases.png")

p2 = ggplot(dat, aes(x=date, y=deaths)) + geom_line()
ggsave(p2, filename="img/deaths.png")

p3 = ggplot(dat, aes(x=date, y=vaccinated)) + geom_point() + ylim(0,NA)
ggsave(p3, filename="img/vaccinated.png")







