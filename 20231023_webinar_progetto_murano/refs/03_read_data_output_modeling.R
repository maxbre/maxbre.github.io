## ---- read-data-output ----

library(tidyverse)
library(patchwork)

# read discrete receptors
cpf_lap_mis <- read_csv('./data_input/recettori_calpuff_lapmod_misure.csv')

## ---- summary-cpf-lap-mis ----

summary_cpf_lap_mis <-cpf_lap_mis %>%
  group_by(recettore, stima) %>%
  summarise(#n=n(), this is accounting for the group size (including NA)
            n = sum(!is.na(valore)), # this accounting for the number of observations excluding NA
            min=min(valore, na.rm=TRUE),
            p25=quantile(valore, 0.25, na.rm=TRUE),
            avg=mean(valore, na.rm=TRUE),
            median=median(valore, na.rm=TRUE),
            p75=quantile(valore, 0.75, na.rm=TRUE),
            p98=quantile(valore, 0.98, na.rm=TRUE),
            max=max(valore, na.rm=TRUE)) 

## ---- fig-rec-facetgrid ----

# here define a named vectos for colors
cols <- c("misura" = "#00BA38", "lapmod" = "#619CFF", "calpuff"="#F8766D")

# chart facet grid
cpf_lap_mis %>%
  ggplot(aes(x=data, y=valore))+
  geom_line(aes(colour = stima), linewidth = 1)+
  scale_x_date(name=NULL, breaks="month", date_labels = "%d-%b-%y")+
  #facet_wrap(stima~recettore, scales = "free_y")+
  facet_grid(rows=vars(stima), cols=vars(recettore), scales="free_y") +
  labs(title="Cadmio: modello vs. misura", y=expression(paste("Cd [ ", ng~m^-3, " ]")))+
  scale_color_manual(values=cols)+
  theme_minimal()+
  theme(legend.position="top", legend.title = element_blank(),
        axis.text.x = element_text(angle=90))

## ---- fos-cpuf-lap ----

# here define a named vectos for colors
cols <- c("misura" = "#00BA38", "lapmod" = "#619CFF", "calpuff"="#F8766D")

# transformation factor
coeff<-1000

fos_cal <- cpf_lap_mis %>% 
  filter(recettore=="foscolo") %>% 
  pivot_wider(id_cols=c(data,recettore), names_from = stima, values_from = valore) %>% 
  ggplot() + 
  geom_line(aes(x=data, y = misura, colour="misura"), linewidth = 0.75)+
  geom_line(aes(x=data, y = calpuff*coeff, colour = "calpuff"), linewidth = 0.75)+
  scale_y_continuous(name = expression(paste("misura [ ", ng~m^-3, " ]")), 
                     #limits=c(0,2100), 
                     sec.axis = sec_axis(~./coeff, name=expression(paste("calpuff [ ", ng~m^-3, " ]"))))+
  #ylim(0, 2100)+
  coord_cartesian(ylim = c(0, 2500)) + #this is the safest option to avoid any conversion of data to NA
  scale_x_date(name=NULL, breaks="month", date_labels = "%d-%b-%y")+
  scale_color_manual(values=cols)+
  theme_minimal()+
  theme(legend.position="top", legend.title = element_blank())

fos_lap <- cpf_lap_mis %>% 
  filter(recettore=="foscolo") %>% 
  pivot_wider(id_cols=c(data,recettore), names_from = stima, values_from = valore) %>% 
  ggplot() + 
  geom_line(aes(x=data, y = misura, colour="misura"), linewidth = 0.75)+
  geom_line(aes(x=data, y = lapmod*coeff, colour = "lapmod"), linewidth = 0.75)+
  scale_y_continuous(name = expression(paste("misura [ ", ng~m^-3, " ]")), 
                     #limits=c(0,2100), 
                     sec.axis = sec_axis(~./coeff, name=expression(paste("lapmod [ ", ng~m^-3, " ]"))))+
  #ylim(0, 2100)+
  coord_cartesian(ylim = c(0, 2500)) + #this is the safest option to avoid any conversion of data to NA
  scale_x_date(name=NULL, breaks="month", date_labels = "%d-%b-%y")+
  scale_color_manual(values=cols)+
  theme_minimal()+
  theme(legend.position="top", legend.title = element_blank())

# compose two charts
fos_cal / fos_lap + plot_annotation(title = "Foscolo: misura vs. lapmod vs. calpuff")

## ---- mar-cpuf-lap ----

# here define a named vectos for colors
cols <- c("misura" = "#00BA38", "lapmod" = "#619CFF", "calpuff"="#F8766D")

coeff<-1000

mar_cal <- cpf_lap_mis %>% 
  filter(recettore=="marco") %>% 
  pivot_wider(c(data,recettore), names_from = stima, values_from = valore) %>% 
  ggplot() + 
  geom_line(aes(x=data, y = misura, colour="misura"), linewidth = 0.75)+
  geom_line(aes(x=data, y = calpuff*coeff, colour = "calpuff"), linewidth = 0.75)+
  scale_y_continuous(name = expression(paste("misura [ ", ng~m^-3, " ]")), 
                     #limits=c(0,2100), 
                     sec.axis = sec_axis(~./coeff, name=expression(paste("calpuff [ ", ng~m^-3, " ]"))))+
  #ylim(0, 2100)+
  coord_cartesian(ylim = c(0, 2500)) + #this is the safest option to avoid any conversion of data to NA
  scale_x_date(name=NULL, breaks="month", date_labels = "%d-%b-%y")+
  scale_color_manual(values=cols)+
  theme_minimal()+
  theme(legend.position="top", legend.title = element_blank())

mar_lap <- cpf_lap_mis %>% 
  filter(recettore=="marco") %>% 
  pivot_wider(c(data,recettore), names_from = stima, values_from = valore) %>% 
  ggplot() + 
  geom_line(aes(x=data, y = misura, colour="misura"), linewidth = 0.75)+
  geom_line(aes(x=data, y = lapmod*coeff, colour = "lapmod"), linewidth = 0.75)+
  scale_y_continuous(name = expression(paste("misura [ ", ng~m^-3, " ]")), 
                     #limits=c(0,2100), 
                     sec.axis = sec_axis(~./coeff, name=expression(paste("lapmod [ ", ng~m^-3, " ]"))))+
  #ylim(0, 2100)+
  coord_cartesian(ylim = c(0, 2500)) + #this is the safest option to avoid any conversion of data to NA
  scale_x_date(name=NULL, breaks="month", date_labels = "%d-%b-%y")+
  scale_color_manual(values=cols)+
  theme_minimal()+
  theme(legend.position="top", legend.title = element_blank())

# compose two charts
mar_cal / mar_lap + plot_annotation(title = "Marco: misura vs. lapmod vs. calpuff")

## ---- ser-cpuf-lap ----

# here define a named vectos for colors
cols <- c("misura" = "#00BA38", "lapmod" = "#619CFF", "calpuff"="#F8766D")

coeff<-1000

ser_cal <- cpf_lap_mis %>% 
  filter(recettore=="serenella") %>% 
  pivot_wider(c(data,recettore), names_from = stima, values_from = valore) %>% 
  ggplot() + 
  geom_line(aes(x=data, y = misura, colour="misura"), linewidth = 0.75)+
  geom_line(aes(x=data, y = calpuff*coeff, colour = "calpuff"), linewidth = 0.75)+
  scale_y_continuous(name = expression(paste("misura [ ", ng~m^-3, " ]")), 
                     #limits=c(0,2100), 
                     sec.axis = sec_axis(~./coeff, name=expression(paste("calpuff [ ", ng~m^-3, " ]"))))+
  #ylim(0, 2100)+
  coord_cartesian(ylim = c(0, 1200)) + #this is the safest option to avoid any conversion of data to NA
  scale_x_date(name=NULL, breaks="month", date_labels = "%d-%b-%y")+
  scale_color_manual(values=cols)+
  theme_minimal()+
  theme(legend.position="top", legend.title = element_blank())

ser_lap <- cpf_lap_mis %>% 
  filter(recettore=="serenella") %>% 
  pivot_wider(c(data,recettore), names_from = stima, values_from = valore) %>% 
  ggplot() + 
  geom_line(aes(x=data, y = misura, colour="misura"), linewidth = 0.75)+
  geom_line(aes(x=data, y = lapmod*coeff, colour = "lapmod"), linewidth = 0.75)+
  scale_y_continuous(name = expression(paste("misura [ ", ng~m^-3, " ]")), 
                     #limits=c(0,2100), 
                     sec.axis = sec_axis(~./coeff, name=expression(paste("lapmod [ ", ng~m^-3, " ]"))))+
  #ylim(0, 2100)+
  coord_cartesian(ylim = c(0, 1200)) + #this is the safest option to avoid any conversion of data to NA
  scale_x_date(name=NULL, breaks="month", date_labels = "%d-%b-%y")+
  scale_color_manual(values=cols)+
  theme_minimal()+
  theme(legend.position="top", legend.title = element_blank())

# compose two charts
ser_cal / ser_lap + plot_annotation(title = "Serenella: misura vs. lapmod vs. calpuff")

