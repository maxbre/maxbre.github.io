## ---- read-data-output-lap ----

library(tidyverse)
#library(patchwork)

# read discrete receptors
lap_mis_l <- read_csv('./data_input/recettori_lapmod_base_hbdpr_vs_misura.csv')

## ---- summary-lap-base-hbdnpr-mis ----

summary_lap_mis <- lap_mis_l %>% 
  group_by(recettore, stima) %>% 
  summarise(n=sum(!is.na(valore)),
            #min=min(valore, na.rm=TRUE),
            p25=quantile(valore, 0.25, na.rm=TRUE),
            avg=mean(valore, na.rm=TRUE),
            #median=median(valore, na.rm=TRUE),
            p75=quantile(valore, 0.75, na.rm=TRUE),
            p98=quantile(valore, 0.98, na.rm=TRUE),
            max=max(valore, na.rm=TRUE)) 


## ---- ratio-mis-lap-hbdnpr ----

# foscolo

avg_foscolo <- lap_mis_l %>%
  filter(recettore=='foscolo', stima!="base") %>% 
  group_by(recettore, stima) %>%
  summarise(avg=round(mean(valore, na.rm=TRUE),1), .groups = 'drop') 


r_fos_lap <- round(avg_foscolo %>% filter(stima=='misure') %>% pull() / avg_foscolo %>% filter(stima=='bdnpr') %>% pull(),0)

# marco

avg_marco <- lap_mis_l %>%
  filter(recettore=='marco', stima!="base") %>% 
  group_by(recettore, stima) %>%
  summarise(avg=round(mean(valore, na.rm=TRUE),1), .groups = 'drop') 

r_mar_lap <- round(avg_marco %>% filter(stima=='misure') %>% pull() / avg_marco %>% filter(stima=='bdnpr') %>% pull(),0)

# serenella

avg_serenella <- lap_mis_l %>%
  filter(recettore=='serenella', stima!="base") %>% 
  group_by(recettore, stima) %>%
  summarise(avg=round(mean(valore, na.rm=TRUE),1), .groups = 'drop') 

r_ser_lap <- round(avg_serenella%>% filter(stima=='misure') %>% pull() / avg_serenella %>% filter(stima=='bdnpr') %>% pull(),0)

# average of ratios

r_avg_receptors <- round(mean(c(r_fos_lap, r_mar_lap, r_ser_lap),0))
