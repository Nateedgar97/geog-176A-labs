#"Geography 176A"
#author: "[Nate Edgar](https://nateedgar97.github.io)"
#subtitle: 'Lab 02: COVID-19 Pandemic'
#output:
#html_document:
#theme: journal
#---

x=3

library(tidyverse)
library(readr)

install.packages("readxl")

home=read_csv("data/landdata-states.csv")

library(readxl)
pop<-read_excel("data/PopulationEstimates.xls", skip=2)

pop %>%
select(fips=FIPStxt, state=state, Area_Names, pop2019=Pop_ESTIMATE_2019) %>%
group_by(state) %>%
slice_max(pop2019,n=1)

summary(p2)
str(p2)

