library(tidyverse)
library(lubridate)
library(scales)

#heavy metals
hm <- read_csv("./data_input/input_murano_2021.csv") %>% 
  mutate(data=as_date(data, format="%d/%m/%Y"),
         anno=year(data))

hm %>%
  filter(stazione == "foscolo",
         data >= "2021-01-01")%>%
  group_by(stazione)%>%
  summarise(As=mean(As),
            Cd=mean(Cd),
            Ni=mean(Ni),
            Pb=mean(Pb))

# https://www.statology.org/ggplot2-log-scale/
# https://www.youtube.com/watch?v=J1CMhV20Wls

hm %>%
  filter(stazione == "foscolo")%>%
  ggplot(mapping=aes(x = factor(anno), y = Cd)) +
  geom_jitter(width = 0.2, alpha=0.2)+
  geom_boxplot(varwidth = TRUE, notch=TRUE) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                name="Cd [ng/m3]") +
  annotation_logticks(sides="l")

# https://www.youtube.com/watch?v=J1CMhV20Wls
# NB ricorda che
# stat_summary() opera dopo la trasformazione logartimica scale_y_log10()
# usa coord_trans() quando effettui una trasformazione logaritmica
# https://github.com/tidyverse/ggplot2/issues/2804

# rivedere i boxplot e la trasformazione logaritmica
# usa coord_trans(trans="log10") invece di coord_y_log10()
###########################################
# rivedi questa sezione
hm %>%
  filter(stazione == "foscolo",
         anno >=2021)%>%
  ggplot(mapping=aes(x=factor(anno), y = Cd)) +
  geom_jitter(width = 0.2, alpha=0.2)+
  geom_boxplot()+
  stat_summary(fun = "mean", color="red", shape=15)+
  coord_trans(y="log10")


# this seems ok!!!!!!!!!!!!

hm %>%
  filter(stazione == "foscolo")%>%
  mutate(anno=factor(year(data)))%>%
  ggplot(mapping=aes(x = anno, y = Cd)) +
  geom_jitter(width = 0.2, alpha=0.2)+
  geom_boxplot() +
  stat_summary(fun = "mean", color="red", shape=15)+
  coord_trans(y="log10")+
  scale_y_continuous(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                name="Cd [ng/m3]")

# The difference between transforming the scales and
# transforming the coordinate system is that scale
# transformation occurs BEFORE statistics, and coordinate
# transformation afterwards.  Coordinate transformation also
# changes the shape of geoms:


# new facet label names for stazione
staz.labs <- c("Murano - SM Foscolo", "Venezia - Sacca Fisola")
names(staz.labs) <- c("foscolo", "fisola")

hm %>%
  mutate(anno=factor(year(data)))%>%
  filter(stazione %in% c("foscolo", "fisola"), 
         data >="2017-01-01")%>%
  ggplot(mapping=aes(x = anno, y = Cd)) +
  geom_jitter(width = 0.2, alpha=0.2)+
  geom_boxplot() +
  stat_summary(fun = "mean", color="red", shape=15)+
  coord_trans(y="log10")+
  scale_y_continuous(breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x)),
                     name="Cd [ng/m3]")+
  facet_wrap(.~stazione, labeller = labeller(stazione=staz.labs))+
  theme_bw()

######################################àà

#### Cd ---------------------------------------------------------------
hm %>%
  filter(stazione == "foscolo",
         data >= "2016-01-01")%>%
  ggplot(mapping=aes(x = data, y = Cd))+
  geom_point(alpha=0.2)+
  geom_line(alpha=0.2)+
  geom_smooth()+
  scale_x_date(name = NULL, date_breaks = "month", expand = c(0.01, 0.01))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                name = "Cd [ng/m3]") +
  geom_vline(xintercept = as.numeric(ymd("2020-03-08")), linetype="dashed", color = "red", size=0.6)+
  geom_vline(xintercept = as.numeric(ymd("2020-06-03")), linetype="dashed", color = "red", size=0.6)+
  annotate(geom="text", x=ymd("2020-02-10"), y=2000, label="2020-03-08, inizio lockdown", color="red", size=3.5, angle =90)+
  annotate(geom="text", x=ymd("2020-05-10"), y=2000, label="2020-06-03, fine lockdown", color="red", size=3.5, angle =90)+
  annotation_logticks(sides="l")+
  labs(title = "Stazione c/o Scuola Ugo Foscolo - Murano VE")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))

#### As ---------------------------------------------------------------
hm %>%
  filter(stazione == "foscolo",
         data >= "2016-01-01")%>%
  ggplot(mapping=aes(x = data, y = As))+
  geom_point(alpha=0.2)+
  geom_line(alpha=0.2)+
  geom_smooth()+
  scale_x_date(name = NULL, date_breaks = "month", expand = c(0.01, 0.01))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                name = "As [ng/m3]") +
  annotation_logticks(sides="l")+
  geom_vline(xintercept = as.numeric(ymd("2020-03-08")), linetype="dashed", color = "red", size=0.6)+
  geom_vline(xintercept = as.numeric(ymd("2020-06-03")), linetype="dashed", color = "red", size=0.6)+
  annotate(geom="text", x=ymd("2020-02-10"), y=10, label="2020-03-08, inizio lockdown", color="red", size=3.5, angle =90)+
  annotate(geom="text", x=ymd("2020-05-10"), y=10, label="2020-06-03, fine lockdown", color="red", size=3.5, angle =90)+
  labs(title = "Stazione c/o Scuola Ugo Foscolo - Murano VE")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))

#### Sb ---------------------------------------------------------------
hm %>%
  filter(stazione == "foscolo",
         data >= "2016-01-01")%>%
  ggplot(mapping=aes(x = data, y = Sb))+
  geom_point(alpha=0.2)+
  geom_line(alpha=0.2)+
  geom_smooth()+
  scale_x_date(name = NULL, date_breaks = "month", expand = c(0.01, 0.01))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                name = "Sb [ng/m3]") +
  annotation_logticks(sides="l")+
  geom_vline(xintercept = as.numeric(ymd("2020-03-08")), linetype="dashed", color = "red", size=0.6)+
  geom_vline(xintercept = as.numeric(ymd("2020-06-03")), linetype="dashed", color = "red", size=0.6)+
  annotate(geom="text", x=ymd("2020-02-10"), y=150, label="2020-03-08, inizio lockdown", color="red", size=3.5, angle =90)+
  annotate(geom="text", x=ymd("2020-05-10"), y=150, label="2020-06-03, fine lockdown", color="red", size=3.5, angle =90)+
  labs(title = "Stazione c/o Scuola Ugo Foscolo - Murano VE")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))

#### Cr ---------------------------------------------------------------
hm %>%
  filter(stazione == "foscolo",
         data >= "2016-01-01")%>%
  ggplot(mapping=aes(x = data, y = Cr))+
  geom_point(alpha=0.2)+
  geom_line(alpha=0.2)+
  geom_smooth()+
  scale_x_date(name = NULL, date_breaks = "month", expand = c(0.01, 0.01))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                name = "Cr [ng/m3]") +
  annotation_logticks(sides="l")+
  geom_vline(xintercept = as.numeric(ymd("2020-03-08")), linetype="dashed", color = "red", size=0.6)+
  geom_vline(xintercept = as.numeric(ymd("2020-06-03")), linetype="dashed", color = "red", size=0.6)+
  annotate(geom="text", x=ymd("2020-02-10"), y=45, label="2020-03-08, inizio lockdown", color="red", size=3.5, angle =90)+
  annotate(geom="text", x=ymd("2020-05-10"), y=45, label="2020-06-03, fine lockdown", color="red", size=3.5, angle =90)+
  labs(title = "Stazione c/o Scuola Ugo Foscolo - Murano VE")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))

#### Ni ---------------------------------------------------------------
hm %>%
  filter(stazione == "foscolo",
         data >= "2016-01-01")%>%
  ggplot(mapping=aes(x = data, y = Ni))+
  geom_point(alpha=0.2)+
  geom_line(alpha=0.2)+
  geom_smooth()+
  scale_x_date(name = NULL, date_breaks = "month", expand = c(0.01, 0.01))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                name = "Ni [ng/m3]") +
  annotation_logticks(sides="l")+
  geom_vline(xintercept = as.numeric(ymd("2020-03-08")), linetype="dashed", color = "red", size=0.6)+
  geom_vline(xintercept = as.numeric(ymd("2020-06-03")), linetype="dashed", color = "red", size=0.6)+
  annotate(geom="text", x=ymd("2020-02-10"), y=45, label="2020-03-08, inizio lockdown", color="red", size=3.5, angle =90)+
  annotate(geom="text", x=ymd("2020-05-10"), y=45, label="2020-06-03, fine lockdown", color="red", size=3.5, angle =90)+
  labs(title = "Stazione c/o Scuola Ugo Foscolo - Murano VE")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))

#### Pb ---------------------------------------------------------------
hm %>%
  filter(stazione == "foscolo",
         data >= "2016-01-01")%>%
  ggplot(mapping=aes(x = data, y = Pb))+
  geom_point(alpha=0.2)+
  geom_line(alpha=0.2)+
  geom_smooth()+
  scale_x_date(name = NULL, date_breaks = "month", expand = c(0.01, 0.01))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                name = "Pb [ng/m3]") +
  annotation_logticks(sides="l")+
  geom_vline(xintercept = as.numeric(ymd("2020-03-08")), linetype="dashed", color = "red", size=0.6)+
  geom_vline(xintercept = as.numeric(ymd("2020-06-03")), linetype="dashed", color = "red", size=0.6)+
  annotate(geom="text", x=ymd("2020-02-10"), y=900, label="2020-03-08, inizio lockdown", color="red", size=3.5, angle =90)+
  annotate(geom="text", x=ymd("2020-05-10"), y=900, label="2020-06-03, fine lockdown", color="red", size=3.5, angle =90)+
  labs(title = "Stazione c/o Scuola Ugo Foscolo - Murano VE")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))
