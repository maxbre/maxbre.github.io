# aaaa readme 
# here a series of chucks were defined 
# by means of the label '## ---- something ----'
# see as follows
# to be later recalled as multiple code chunck from this external file

library(tidyverse)
library(lubridate)
library(scales)

## ---- read-pm10-foscolo ----

# pm10
pm10 <- read_csv("./data_input/input_murano_pm10.csv") %>%
  pivot_longer(-data, names_to = "stazione", values_to = "pm10") %>% 
  separate(stazione, c('sito','nome_staz'), sep = "_", extra='merge') %>% # this is to deal with the second underscore
  mutate(data = dmy(data), anno=year(data), mese=month(data), .after=data)

## ---- pm10-boxplot ----

staz.labs <- c("Murano - Foscolo", "Venezia - Sacca Fisola")
names(staz.labs) <- c("foscolo", "fisola")

pm10 %>%
  mutate(anno=factor(anno))%>%
  filter(nome_staz %in% c("foscolo", "fisola"), 
         data >="2017-01-01")%>%
  ggplot(mapping=aes(x = anno, y = pm10)) +
  geom_boxplot(notch=TRUE) +
  geom_jitter(width = 0.2, alpha=0.1)+
  stat_summary(fun = "mean", color="red", size = 0.4, shape = 8)+
  labs(y=expression(paste("PM10 [ ", mu*g~m^-3, " ]")), 
       x=NULL)+
  facet_wrap(nome_staz~., labeller = labeller(nome_staz=staz.labs))+
  theme_bw()

