# aaaa readme 
# here a series of chucks were defined 
# by means of the label '## ---- something ----'
# see as follows
# to be later recalled as multiple code chunck from this external file

library(tidyverse)
library(lubridate)
library(scales)

## ---- read-hm-foscolo ----

#heavy metals
hm <- read_csv("./data_input/input_murano_metalli.csv") %>% 
  mutate(data=dmy(data),
         anno=year(data))

tab_2021<- hm %>%
  filter(stazione == "foscolo",
         data >= "2021-01-01")%>%
  group_by(stazione)%>%
  summarise(As=format(round(mean(As),1), nsmall=1),
            Cd=format(round(mean(Cd),1), nsmall=1),
            Ni=format(round(mean(Ni),1), nsmall=1),
            Pb=format(round(mean(Pb),0), nsmall=0))

## ---- tab-hm-foscolo-2017 ----

tab_2017<- hm %>%
  filter(stazione == "foscolo",
         data >= "2017-01-01")%>%
  group_by(stazione, anno)%>%
  summarise(n=n(),
            As=format(round(mean(As),1), nsmall=1),
            Cd=format(round(mean(Cd),1), nsmall=1),
            Ni=format(round(mean(Ni),1), nsmall=1),
            Pb=format(round(mean(Pb),0), nsmall=0))

## ---- cd-boxplot ----

# staz.labs <- c("Murano - Foscolo", "Venezia - Sacca Fisola")
# names(staz.labs) <- c("foscolo", "fisola")
# 
# hm %>%
#   mutate(anno=factor(year(data)))%>%
#   filter(stazione %in% c("foscolo", "fisola"), 
#          data >="2017-01-01")%>%
#   ggplot(mapping=aes(x = anno, y = Cd)) +
#   geom_boxplot() +
#   geom_jitter(width = 0.2, alpha=0.1)+
#   stat_summary(fun = "mean", color="red", shape=8, size=0.4)+
#   coord_trans(y="log10")+
#   scale_y_continuous(breaks = trans_breaks("log10", function(x) 10^x),
#                      labels = trans_format("log10", math_format(10^.x)),
#                      name="Cd [ng/m3]")+
#   labs(x=NULL)+
#   facet_wrap(.~stazione, labeller = labeller(stazione=staz.labs))+
#   theme_bw()

staz.labs <- c("Murano - Foscolo", "Venezia - Sacca Fisola")
names(staz.labs) <- c("foscolo", "fisola")

hm %>%
filter(stazione == "foscolo" | stazione == "fisola", anno >=2017) %>% 
  ggplot(aes(x = factor(anno), y = Cd))+
  geom_boxplot(notch=TRUE)+
  geom_jitter(width = 0.2, alpha=0.1)+
  stat_summary(fun = function(x) log10(mean(10^x)), geom='point', colour ="red", size = 2, shape = 8)+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="l", colour = "gray")+
  labs(x=NULL,
       y=expression(paste("Cd [ ", ng~m^-3, " ]"))
       )+
  facet_wrap(stazione ~., labeller = labeller(stazione=staz.labs))+
  theme_bw() 

## ---- cd-boxplot-month ----

hm %>%
  filter(stazione == "foscolo" | stazione == "fisola", anno >=2017) %>% 
  ggplot(aes(x = factor(month(data, label=TRUE)), y = Cd, colour = stazione))+
  geom_boxplot(notch=TRUE)+
  stat_summary(fun = function(x) log10(mean(10^x)), geom='point', size = 1.5, shape = 8, position=position_dodge(width = 0.8), show.legend = FALSE)+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="l", colour = "gray")+
  scale_colour_manual(labels=c("Sacca Fisola", "Foscolo"), values = c("#F8766D", "#00BFC4"))+
  labs(colour = "stazione", 
       x = NULL, 
       y = expression(paste("Cd [ ", ng~m^-3, " ]"))
       )+
  guides(color=guide_legend("sito"))+ 
  facet_wrap(anno ~.)+  
  theme_bw()+
  theme(legend.position = "bottom")

## ---- cd-boxplot-month-type ----

hm %>%
  filter(stazione == "foscolo" | stazione == "fisola", anno >=2017) %>% 
  ggplot(aes(x = factor(month(data, label=TRUE)), y = Cd, colour = stazione))+
  geom_boxplot(notch=TRUE)+
  stat_summary(fun = function(x) log10(mean(10^x)), geom='point', size = 1.5, shape = 8, position=position_dodge(width = 0.8), show.legend = FALSE)+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="l", colour = "gray")+
  scale_colour_manual(labels=c("Sacca Fisola", "Foscolo"), values = c("#00BFC4", "#F8766D"))+
  labs(colour = "stazione",
       x = NULL,
       y =expression(paste("Cd [ ", ng~m^-3, " ]"))
       )+
  guides(color=guide_legend("sito"))+ 
  theme_bw()+
  theme(legend.position = "bottom")

## ---- cd-ts24h ----

hm %>%
  filter(stazione == "foscolo",
         data >= "2016-01-01")%>%
  ggplot(mapping=aes(x = data, y = Cd))+
  geom_point(alpha=0.2)+
  geom_line(alpha=0.2)+
  geom_smooth()+
  scale_x_date(name = NULL, date_breaks = "2 months", expand = c(0.01, 0.01))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                name = expression(paste("Cd [ ", ng~m^-3, " ]"))
                ) +
  geom_vline(xintercept = as.numeric(ymd("2020-03-08")), linetype="dashed", color = "red", size=0.6)+
  geom_vline(xintercept = as.numeric(ymd("2020-06-03")), linetype="dashed", color = "red", size=0.6)+
  annotate(geom="text", x=ymd("2020-02-10"), y=1000, label="2020-03-08, inizio lockdown", color="red", size=3, angle =90)+
  annotate(geom="text", x=ymd("2020-05-10"), y=1000, label="2020-06-03, fine lockdown", color="red", size=3, angle =90)+
  annotation_logticks(sides="l")+
  geom_hline(yintercept = 0.2, linetype="dashed", color = "green", size=0.5)+
  annotate(geom="text", x=ymd("2016-10-01"), y=0.15, label="LQ", colour="green", size=3.5)+
  #labs(title = "Stazione c/o Scuola Ugo Foscolo - Murano VE")+
  theme_bw()+
  theme(axis.text.x = element_text(size=6, angle = 90))

## ---- as-ts24h ----

hm %>%
  filter(stazione == "foscolo",
         data >= "2016-01-01")%>%
  ggplot(mapping=aes(x = data, y = As))+
  geom_point(alpha=0.2)+
  geom_line(alpha=0.2)+
  geom_smooth()+
  scale_x_date(name = NULL, date_breaks = "2 months", expand = c(0.01, 0.01))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                name = expression(paste("As [ ", ng~m^-3, " ]"))
                ) +
  annotation_logticks(sides="l")+
  geom_vline(xintercept = as.numeric(ymd("2020-03-08")), linetype="dashed", color = "red", size=0.6)+
  geom_vline(xintercept = as.numeric(ymd("2020-06-03")), linetype="dashed", color = "red", size=0.6)+
  annotate(geom="text", x=ymd("2020-02-10"), y=10, label="2020-03-08, inizio lockdown", color="red", size=3, angle =90)+
  annotate(geom="text", x=ymd("2020-05-10"), y=10, label="2020-06-03, fine lockdown", color="red", size=3, angle =90)+
  geom_hline(yintercept = 1, linetype="dashed", color = "green", size=0.5)+
  annotate(geom="text", x=ymd("2017-03-01"), y=0.85, label="LQ", colour="green", size=3.5)+
  #labs(title = "Stazione c/o Scuola Ugo Foscolo - Murano VE")+
  theme_bw()+
  theme(axis.text.x = element_text(size=6, angle = 90))

## ---- sb-ts24h ----

hm %>%
  filter(stazione == "foscolo",
         data >= "2016-01-01")%>%
  ggplot(mapping=aes(x = data, y = Sb))+
  geom_point(alpha=0.2)+
  geom_line(alpha=0.2)+
  geom_smooth()+
  scale_x_date(name = NULL, date_breaks = "2 months", expand = c(0.01, 0.01))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                name = expression(paste("Sb [ ", ng~m^-3, " ]"))
                ) +
  annotation_logticks(sides="l")+
  geom_vline(xintercept = as.numeric(ymd("2020-03-08")), linetype="dashed", color = "red", size=0.6)+
  geom_vline(xintercept = as.numeric(ymd("2020-06-03")), linetype="dashed", color = "red", size=0.6)+
  annotate(geom="text", x=ymd("2020-02-10"), y=100, label="2020-03-08, inizio lockdown", color="red", size=3, angle =90)+
  annotate(geom="text", x=ymd("2020-05-10"), y=100, label="2020-06-03, fine lockdown", color="red", size=3, angle =90)+
  geom_hline(yintercept = 1, linetype="dashed", color = "green", size=0.5)+
  annotate(geom="text", x=ymd("2016-12-01"), y=0.85, label="LQ", colour="green", size=3.5)+
  #labs(title = "Stazione c/o Scuola Ugo Foscolo - Murano VE")+
  theme_bw()+
  theme(axis.text.x = element_text(size=6, angle = 90))


## ---- cr-ts24h ----

hm %>%
  filter(stazione == "foscolo",
         data >= "2016-01-01")%>%
  ggplot(mapping=aes(x = data, y = Cr))+
  geom_point(alpha=0.2)+
  geom_line(alpha=0.2)+
  geom_smooth()+
  scale_x_date(name = NULL, date_breaks = "2 months", expand = c(0.01, 0.01))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                name = expression(paste("Cr [ ", ng~m^-3, " ]"))
                ) +
  annotation_logticks(sides="l")+
  geom_vline(xintercept = as.numeric(ymd("2020-03-08")), linetype="dashed", color = "red", size=0.6)+
  geom_vline(xintercept = as.numeric(ymd("2020-06-03")), linetype="dashed", color = "red", size=0.6)+
  annotate(geom="text", x=ymd("2020-02-10"), y=30, label="2020-03-08, inizio lockdown", color="red", size=3, angle =90)+
  annotate(geom="text", x=ymd("2020-05-10"), y=30, label="2020-06-03, fine lockdown", color="red", size=3, angle =90)+
  geom_hline(yintercept = 0.5, linetype="dashed", color = "green", size=0.5)+
  annotate(geom="text", x=ymd("2016-10-01"), y=0.45, label="LQ", colour="green", size=3.5)+
  #labs(title = "Stazione c/o Scuola Ugo Foscolo - Murano VE")+
  theme_bw()+
  theme(axis.text.x = element_text(size=6, angle = 90))

## ---- ni-ts24h ----

hm %>%
  filter(stazione == "foscolo",
         data >= "2016-01-01")%>%
  ggplot(mapping=aes(x = data, y = Ni))+
  geom_point(alpha=0.2)+
  geom_line(alpha=0.2)+
  geom_smooth()+
  scale_x_date(name = NULL, date_breaks = "2 months", expand = c(0.01, 0.01))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                name = expression(paste("Ni [ ", ng~m^-3, " ]"))
                ) +
  annotation_logticks(sides="l")+
  geom_vline(xintercept = as.numeric(ymd("2020-03-08")), linetype="dashed", color = "red", size=0.6)+
  geom_vline(xintercept = as.numeric(ymd("2020-06-03")), linetype="dashed", color = "red", size=0.6)+
  annotate(geom="text", x=ymd("2020-02-10"), y=45, label="2020-03-08, inizio lockdown", color="red", size=3, angle =90)+
  annotate(geom="text", x=ymd("2020-05-10"), y=45, label="2020-06-03, fine lockdown", color="red", size=3, angle =90)+
  geom_hline(yintercept = 1, linetype="dashed", color = "green", size=0.5)+
  annotate(geom="text", x=ymd("2016-10-01"), y=0.85, label="LQ", colour="green", size=3.5)+
  #labs(title = "Stazione c/o Scuola Ugo Foscolo - Murano VE")+
  theme_bw()+
  theme(axis.text.x = element_text(size=6, angle = 90))

## ---- pb-ts24h ----

hm %>%
  filter(stazione == "foscolo",
         data >= "2016-01-01")%>%
  ggplot(mapping=aes(x = data, y = Pb))+
  geom_point(alpha=0.2)+
  geom_line(alpha=0.2)+
  geom_smooth()+
  scale_x_date(name = NULL, date_breaks = "2 months", expand = c(0.01, 0.01))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                name = expression(paste("Pb [ ", ng~m^-3, " ]"))
                ) +
  annotation_logticks(sides="l")+
  geom_vline(xintercept = as.numeric(ymd("2020-03-08")), linetype="dashed", color = "red", size=0.6)+
  geom_vline(xintercept = as.numeric(ymd("2020-06-03")), linetype="dashed", color = "red", size=0.6)+
  annotate(geom="text", x=ymd("2020-02-10"), y=500, label="2020-03-08, inizio lockdown", color="red", size=3, angle =90)+
  annotate(geom="text", x=ymd("2020-05-10"), y=500, label="2020-06-03, fine lockdown", color="red", size=3, angle =90)+
  geom_hline(yintercept = 0.5, linetype="dashed", color = "green", size=0.5)+
  annotate(geom="text", x=ymd("2016-10-01"), y=0.45, label="LQ", colour="green", size=3.5)+
  #labs(title = "Stazione c/o Scuola Ugo Foscolo - Murano VE")+
  theme_bw()+
  theme(axis.text.x = element_text(size=6, angle = 90))

