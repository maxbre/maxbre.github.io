library(tidyverse)
library(lubridate)
library(scales)

#heavy metals
hm <- read_csv("./input_murano_2021.csv") %>% 
  mutate(data=as_date(data, format="%d/%m/%Y"))


Cd <- hm %>%
  select(stazione, data, Cd, Cd_flag)%>%
  group_by(data) %>%
  summarise(recs=n(),
            Cd_mean = mean(Cd),
            Cd_min = min(Cd),
            Cd_max = max(Cd),
            Cd_median = median(Cd))

