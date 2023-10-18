## ---- read-data-input-grid-p98-24h ----

library(tidyverse)
library(raster)
library(metR)
library(patchwork)

#####
# points vetrerie
vetr_df<-read_csv('./data_input/lista_25_vetrerie.csv')

#### recettoti
# file recettori
fr <- './data_input/recettori.rec'
rec_df<-read_table(fr, skip=1, col_names = c("x","y","h","name")) 

##### CALPUFF
fc <- './data_input/calpost_grid/rank(2)_cd_24hr_conc_cd.grd'

# calpost from raster to dataframe average
cp_p98 <- raster(fc) %>%
  as.data.frame(., xy=TRUE) %>%
  mutate(x = x*1000, y = y*1000) %>% 
  rename(z=3)

###### LAPMOD
fl <- './data_input/lapost_grid/pct24h98.grd'

# lapost from raster to dataframe average
lp_p98 <- raster(fl) %>%
  as.data.frame(., xy=TRUE) %>%
  rename(z=3)

## ---- ggplot-contour-cpf-lap-p98-24h ----

##### cp vs lp

ll <- c(761000, 5036750)
xlim <- c(ll[1], ll[1]+2500)
ylim <- c(ll[2], ll[2]+3000)

cp_max_z <- cp_p98 %>% 
  summarise(round(max(z),1)) %>% 
  pull()

cp_min_z <- cp_p98 %>% 
  summarise(round(min(z),1)) %>% 
  pull()

lp_max_z <- lp_p98 %>%
  summarise(round(max(z),1)) %>%
  pull()

lp_min_z <- lp_p98 %>%
  summarise(round(min(z),1)) %>%
  pull()

# set the binwidth, min and max of contourlines
#binwidth <- 0.1
min_level <- min(cp_min_z, lp_min_z)
max_level <- max(cp_max_z, lp_max_z)

c <- cp_p98 %>% 
  ggplot() +
  #geom_contour(aes(x = x, y = y, z = z, colour = as.factor(after_stat(level))), binwidth=binwidth)+
  #geom_text_contour(aes(x = x, y = y, z = z, colour = as.factor(after_stat(level))), 
  #                  binwidth = binwidth, 
  #                  stroke = 0.3, size = 3, skip = 1, show.legend = FALSE) +
  #geom_contour(aes(x = x, y = y, z = z, colour = after_stat(level)), binwidth = binwidth)+
  geom_contour(aes(x = x, y = y, z = z, colour = as.factor(after_stat(level))), 
               #binwidth = binwidth, 
               breaks = c(0.3, 0.5, 1, 2, 3)
               )+
  geom_text_contour(aes(x = x, y = y, z = z ), 
                    colour = "black", alpha= 0.8, 
                    #binwidth = binwidth, 
                    breaks = c(0.3, 0.5, 1, 2, 3),
                    stroke = 0.3, size = 3, skip = 0, show.legend = FALSE) +
  geom_point(data = vetr_df, mapping = aes(x = x, y = y), 
             color = "red", size = 1, alpha = 0.5, shape=3)+
  geom_point(data = rec_df, mapping = aes(x = x, y = y), 
             color = "red", size = 1, alpha = 0.5, shape=20)+
  scale_x_continuous(breaks = seq(xlim[1], xlim[2], 500), 
                     minor_breaks = seq(xlim[1], xlim[2], 100))+
  scale_y_continuous(breaks = seq(ylim[1], ylim[2], 500), 
                     minor_breaks = seq(ylim[1], ylim[2], 100))+
  #scale_colour_viridis_d(limits = factor(seq(min_level, max_level, binwidth)))+
  #scale_colour_viridis_c(limits = c(min_level, max_level), begin = 0, direction = 1, option = "D")+
  scale_colour_viridis_d(begin = 0, end = 1)+
  coord_equal(xlim = xlim, ylim = ylim)+
  labs(colour = NULL, title = paste0("CALPUFF [max = ", cp_max_z, "]"), x = NULL, y = NULL)+
  theme_minimal()+
  theme(panel.grid.minor = element_line(linetype = "dashed"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, size=8),
        axis.text.y = element_text(vjust = 0.5, size=8))

l <- lp_p98 %>%
  ggplot() +
  #geom_contour(aes(x = x, y = y, z = z, colour = as.factor(after_stat(level))), binwidth = binwidth)+
  #geom_text_contour(aes(x = x, y = y, z = z, colour = as.factor(after_stat(level))), 
  #                  binwidth = binwidth, stroke = 0.3, size = 3, skip = 1, show.legend = FALSE) +
  #geom_contour(aes(x = x, y = y, z = z, colour = after_stat(level)), binwidth = binwidth)+
  geom_contour(aes(x = x, y = y, z = z, colour = as.factor(after_stat(level))), 
               #binwidth = binwidth, 
               breaks = c(0.3, 0.5, 1, 2, 3)
               )+
  geom_text_contour(aes(x = x, y = y, z = z ), 
                    colour = "black", alpha = 0.8, 
                    #binwidth = binwidth, 
                    breaks = c(0.3, 0.5, 1, 2, 3),
                    stroke = 0.3, size = 3, skip = 0, show.legend = FALSE) +
  geom_point(data = vetr_df, mapping = aes(x = x, y = y), 
             color = "red", size = 1, alpha = 0.5, shape=3)+
  geom_point(data = rec_df, mapping = aes(x = x, y = y), 
             color = "red", size = 1, alpha = 0.5, shape=20)+
  scale_x_continuous(breaks = seq(xlim[1], xlim[2], 500), 
                     minor_breaks = seq(xlim[1], xlim[2], 100))+
  scale_y_continuous(breaks = seq(ylim[1], ylim[2], 500), 
                     minor_breaks = seq(ylim[1], ylim[2], 100))+
  #scale_colour_viridis_d(limits = factor(seq(min_level, max_level, binwidth)))+
  scale_colour_viridis_d(begin = 0, end = 1)+
  #scale_colour_viridis_c(limits = c(min_level, max_level), begin = 0, direction = 1, option = "D")+
  #scale_colour_viridis_c(breaks = c(min_level, max_level), expand = expansion(mult = 0), begin = 0, direction = 1, option = "D")+
  coord_equal(xlim = xlim, ylim = ylim)+
  labs(colour = NULL, title = paste0("LAPMOD [max = ", lp_max_z, "]"), x = NULL, y = NULL)+
  theme_minimal()+
  theme(panel.grid.minor = element_line(linetype = "dashed"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, size=8),
        axis.text.y = element_text(vjust = 0.5, size=8))

c + l + plot_layout(guides = 'collect') & theme(legend.position='bottom') & guides(colour = guide_legend(nrow = 1))

#c + l + plot_layout(guides = 'collect', ncol=1, nrow = 2)
#c + l + plot_layout(guides = 'collect', nrow = 2) & theme(legend.position='bottom')

#f_chart <- paste0('capuff_vs_lapmod_', '_average_period' , '.jpg')
#ggsave(f_chart, width = 36, height = 18, units = "cm")

