## ---- input-prtmet-22 ----

library(tidyverse)
library(scales)
library(openair)

# read prtmet dat
# read headings, first row, i.e. col names

col_names <- read_table('./data_input/prtmet_2022.dat', skip= 1, n_max = 0) %>% names() %>% tolower()

# remove empty column
col_names <- head(col_names, -1)

met<-read_table('./data_input/prtmet_2022.dat', 
                col_names= col_names, 
                skip=4, 
                na='********' # pay attention to this!
                )%>%  
  rename_with(~gsub("*", "_star", .x, fixed = TRUE))%>%
  rename_with(~gsub(".", "_", .x, fixed = TRUE)) # gsub dot "." with "_", note the use of fixed

 


# create and format date column with lubridate
met$date <- ymd_h(paste0(met$year, '-', met$month, '-', met$day, ' ', met$hour))

# eventually delete last row 
met<-met[-nrow(met),]

#cut wind classes
met <- met %>% 
  mutate(wsf = cut(ws,
                   breaks= c(0, 0.5, 1, 2, 3, 6, max(ws)),
                   labels= c('< 0.5','[ 0.5, 1 )','[ 1, 2 )','[ 2, 3 )','[ 3, 6 )','>= 6'),
                   include.lowest=TRUE, right=FALSE))

## ---- wind-rose-22 ----

# windrose by month

met %>% 
  windRose(.,
         #main="",
         paddle=FALSE, 
         cols='jet', 
         ws.int=0.5, 
         breaks=c(0, 0.5,1, 2, 3, 6, round(max(met$ws),0)),
         grid.line = 10,
         angle=22.5,
         key.position = 'right', 
         annotate =FALSE, 
         type='month')

## ---- wind-rose-day-night-22 ----

# windrose by daylight nigthtime

met %>% 
  windRose(.,
         #main="",
         paddle=FALSE, 
         cols='jet', 
         ws.int=0.5, 
         breaks=c(0, 0.5,1, 2, 3, 6, round(max(met$ws),0)),
         grid.line = 10,
         angle=22.5,
         key.position = 'right', 
         annotate =FALSE, 
         type = "daylight",
         latitude = 45,
         longitude = 12)

#see openair::cutData 

## ---- wind-barplot-22 ----

# met %>% 
#   filter(month>8) %>%
#   mutate(recs_all=n())%>%
#   group_by(ws_class=wsf) %>%
#   summarise(recs=n(),
#             perc=recs/first(recs_all)*100) %>% 
#   ggplot(aes(x=ws_class, y=perc, fill=ws_class))+
#   geom_col(show.legend=FALSE)+
#   geom_text(aes(label = format(round(perc, 1), nsmall=1)), vjust=-0.3, size=3.5)+
#   ylim(0,100)+
#   #scale_y_continuous('%', limits=c(0,100))+
#   labs(#title=mysite, 
#     y='frequenza %',
#     x=expression(paste("[ ", m~s^-1, " ]")))+
#   theme_bw()

# this is showing counting by using geom_bar
# met %>% 
#   filter(month>8) %>% 
#   ggplot(aes(x = wsf, fill=wsf))+
#   geom_bar(show.legend=FALSE)

# this is usign after_stat for computing percentages
met %>% 
  ggplot(aes(x=wsf,  y = after_stat(count/sum(count)))) +
  geom_bar()+
  geom_text(
    aes(y = after_stat(count/sum(count))+0.1,
        label = percent(round(after_stat(count/sum(count)),3))
        ),
    stat ="count", size=3)+
  scale_y_continuous(name= 'frequenza %', labels = percent, limits=c(0,1))+
  labs(x=expression(paste("classi velocità [ ", m~s^-1, " ]")))+
  theme_bw()

## ---- temp-line-year-month-22 ----
# temperature

met %>% 
  ggplot(aes(x=factor(format(date, '%H')), y=t-273)) +
  geom_point(alpha=0.2)+
  #geom_smooth(aes(group=1))+
  stat_summary(aes(group=1), # dummy group all points
               fun='mean', colour='red', linewidth=0.75, geom='line')+
  facet_grid(~factor(format(date, '%m')))+
  xlab('ora')+
  ylab('temperatura [ °C ]')+
  scale_x_discrete(name=NULL,
                   breaks=c('00','12', '23'),
                   labels=c('0','12', '23'))+
  theme_bw()+
  theme(axis.text.x = element_text(size = 8, angle=90, vjust = 0.5, hjust = 0.5))

## ---- temp-boxplot-year-month-22 ----

met %>% 
  ggplot(aes(x=factor(format(date, '%m')), y=t-273))+
  #geom_jitter(alpha=0.2)+
  geom_boxplot()+
  stat_summary(fun = "mean", color="red", size = 0.2, shape = 8)+
  xlab('mesi')+
  ylab('temperatura [ °C ]')+
  theme_bw()

## ---- prec-year-month-22 ----

#precipitation
# met %>% 
#   ggplot(aes(x=factor(format(date, '%m'))))+
#   geom_bar(aes(weight=prec_rate))+
#   xlab('mesi')+
#   ylab('precipitazione [ mm ]')+
#   theme_bw()

met %>% 
  ggplot(aes(x=factor(format(date, '%m')), weight=prec_rate))+
  geom_bar()+
  geom_text(aes(label = round(after_stat(count),1)), stat = 'count', size=3, nudge_y = 6)+
  xlab('mesi')+
  ylab('precipitazioni [ mm ]')+
  theme_bw()

# met %>% 
#  group_by(month) %>% 
#  summarise(sum_prec=sum(prec_rate))


## ---- hmix-year-22 ----
# mixing height

# line and points by month and hour
met %>% 
ggplot(aes(x=factor(format(date, '%H')), y=mix_hgt)) +
  geom_point(alpha=0.1)+
  #geom_smooth(aes(group=1))+
  stat_summary(aes(group=1), # dummy group all points
               fun='mean', colour='red', linewidth=0.6, geom='line')+
  facet_grid(~factor(format(date, '%m')))+
  xlab('ora')+
  ylab('Hmix [ m ]')+
  scale_x_discrete(breaks=c('00','12', '23'),
                   labels=c('0','12', '23'))+
  theme_bw()+
  theme(axis.text.x = element_text(size = 8, angle=90, vjust = 0.5, hjust = 0.5))


## ---- hmix-day-22----
# hmix giorno tipo
met %>%
  ggplot(aes(x=factor(format(date, '%H')), y=mix_hgt)) +
  geom_point(alpha=0.1)+
  #geom_smooth(aes(group=1))+
  stat_summary(aes(group=1), # dummy group all points
               fun='mean', colour='red', size=0.75, geom='line')+
  xlab('ora')+
  ylab('Hmix [ m ]')+
  scale_x_discrete(breaks=c('00','08','16', '23'),
                   labels=c('00','08','16', '23'))+
  theme_bw()

## ---- pg-year-month-set-dic ----

# met %>% 
#   mutate(pg_f=factor(pg, levels=1:6, labels=LETTERS[1:6])) %>% 
#   ggplot(aes(x=pg_f))+
#   geom_bar() +
#   #geom_bar(aes(weight=pg)) +
#   facet_wrap(~factor(format(date, '%m')))+
#   xlab('classi stabilità atmosferica (Pasquill-Gifford-Turner)')+
#   ylab('conteggio')+
#   theme_bw()

 # met %>%
 #   mutate(pg_f=factor(pg, levels=1:6, labels=LETTERS[1:6])) %>%
 #   filter(month>8) %>%
 #   group_by(month, pg_f) %>%
 #   summarise(sum_pg=sum(pg))

met %>% 
  mutate(pg_f=factor(pg, levels=1:6, labels=LETTERS[1:6])) %>% 
  ggplot(aes(x=pg_f))+
  geom_bar() +
  #geom_bar(aes(weight=pg)) +
  facet_wrap(~factor(format(date, '%m')))+
  xlab('classi stabilità atmosferica (Pasquill-Gifford-Turner)')+
  ylab('conteggio')+
  theme_bw()

# met %>% 
#   filter(month>8) %>% 
#   mutate(pg_f=factor(pg, levels=1:6, labels=LETTERS[1:6])) %>% 
#   ggplot(aes(x=pg_f))+
#   geom_bar() +
#   #geom_bar(aes(weight=pg)) +
#   xlab('classi stabilità atmosferica (Pasquill-Gifford-Turner)')+
#   ylab('conteggio')+
#   theme_bw()

  