## ---- read-data-grid-f3-sc2 ----

library(tidyverse)
library(sf)
library(raster)
library(metR)
library(patchwork)

# file point f3
f3_df<-read_csv('./data_input/f3_camino_impianto_sperimentale.csv')

# file recettori
fr <- './data_input/recettori.rec'
rec_df<-read_table(fr, skip=1, col_names = c("x","y","h","name")) 

# shapefile murano
shp_murano <- read_sf('./data_input/shp_murano/murano.shp')

## ---- ggplot-f3-sc2-avg ----

# file lapmod avg period
fl <- './data_input/grd_f3_sc_2/Average_Period.grd '

# lapost from raster to dataframe average
lp_avg <- raster(fl) %>%
  as.data.frame(., xy=TRUE) %>%
  rename(z=3)

# define xy extent
ll <- c(761500, 5038000)
xlim <- c(ll[1], ll[1]+1500)
ylim <- c(ll[2], ll[2]+1500)

# define breaks
breaks <- c(0.005, 0.01, 0.02, 0.05, 0.1)

# define title
title <- " Scenario 2 - media annuale" 

lp_avg %>%
  ggplot() +
  geom_sf(colour="gray",  alpha=0.5, data=shp_murano) +
  geom_point(data = f3_df, 
             mapping = aes(x = x, y = y, shape="sorgente"), # a sort of trick
             color = "red", 
             size = 1, alpha = 0.8)+
  geom_point(data = rec_df, 
             mapping = aes(x = x, y = y, shape="recettori"), # a sort of trick
             color = "red", 
             size = 1, alpha = 0.8)+
  geom_contour(aes(x = x, 
                   y = y, z = z, 
                   colour = as.factor(after_stat(level))),
                   linewidth=0.5,
                   breaks = breaks)+
  geom_text_contour(aes(x = x, y = y, z = z ), 
                    colour = "black", 
                    alpha = 0.8, 
                    breaks = breaks,
                    stroke = 0.5, 
                    size = 2.5, 
                    skip = 0, 
                    show.legend = FALSE,
                    #label.placer = label_placer_flattest()
                    label.placer = label_placer_fraction(frac=0.4, rot_adjuster = isoband::angle_halfcircle_right())
                    ) +
  scale_colour_viridis_d(begin = 0, end = 1, option = "D")+
  scale_shape_manual(name=NULL, values = c(5,3))+ # see above, about the sort of trick
  labs(colour = expression(paste("Cd [ ", ng~m^-3, " ]")), 
       title = title,
       x = NULL, y = NULL)+
  coord_sf(xlim = xlim,
           ylim = ylim,
           expand = FALSE,
           datum = st_crs(shp_murano))+
  scale_x_continuous(breaks = seq(xlim[1], xlim[2], 250))+
  scale_y_continuous(breaks = seq(ylim[1], ylim[2], 250))+
  theme_minimal()+
  theme(panel.grid.major = element_line(linetype = 3),
        axis.text.x = element_text(angle = 90, vjust = 0.5, size=8),
        axis.text.y = element_text(vjust = 0.5, size=8))

## ---- ggplot-f3-sc2-p98 ----

# file p98 avg 24h
fl <- './data_input/grd_f3_sc_2/Percentile_24h.grd'

# lapost from raster to dataframe average
lp_p98 <- raster(fl) %>%
  as.data.frame(., xy=TRUE) %>%
  rename(z=3)

# define xy extent
ll <- c(761500, 5037750)
xlim <- c(ll[1], ll[1]+1500)
ylim <- c(ll[2], ll[2]+1750)

# define breaks
breaks <- c(0.02, 0.05, 0.10, 0.25, 0.5)

# define title
title <- " Scenario 2 - 98° perc. 24h" 

lp_p98 %>%
  ggplot() +
  geom_sf(colour="gray",  alpha=0.5, data=shp_murano) +
  geom_point(data = f3_df, 
             mapping = aes(x = x, y = y, shape="sorgente"), # a sort of trick
             color = "red", 
             size = 1, alpha = 0.8)+
  geom_point(data = rec_df, 
             mapping = aes(x = x, y = y, shape="recettori"), # a sort of trick
             color = "red", 
             size = 1, alpha = 0.8)+
  geom_contour(aes(x = x, 
                   y = y, z = z, 
                   colour = as.factor(after_stat(level))),
               linewidth=0.5,
               breaks = breaks)+
  geom_text_contour(aes(x = x, y = y, z = z ), 
                    colour = "black", 
                    alpha = 0.8, 
                    breaks = breaks,
                    stroke = 0.5, 
                    size = 2.5, 
                    skip = 0, 
                    show.legend = FALSE,
                    label.placer = label_placer_flattest()) +
  scale_colour_viridis_d(begin = 0, end = 1, option = "D")+
  scale_shape_manual(name="", values = c(5,3))+ # see above, about the sort of trick
  labs(colour = expression(paste("Cd [ ", ng~m^-3, " ]")), 
       title = title,
       x = NULL, y = NULL)+
  coord_sf(xlim = xlim,
           ylim = ylim,
           expand = FALSE,
           datum = st_crs(shp_murano))+
  scale_x_continuous(breaks = seq(xlim[1], xlim[2], 250))+
  scale_y_continuous(breaks = seq(ylim[1], ylim[2], 250))+
  theme_minimal()+
  theme(panel.grid.major = element_line(linetype = 3),
        axis.text.x = element_text(angle = 90, vjust = 0.5, size=8),
        axis.text.y = element_text(vjust = 0.5, size=8))

## ---- ggplot-f3-sc2-max ----

# file max avg 24h
fl <- './data_input/grd_f3_sc_2/Maximum_24h.grd'

# lapost from raster to dataframe average
lp_max <- raster(fl) %>%
  as.data.frame(., xy=TRUE) %>%
  rename(z=3)

# define xy extent
ll <- c(761500, 5037750)
xlim <- c(ll[1], ll[1]+1750)
ylim <- c(ll[2], ll[2]+1750)

# define breaks
breaks <- c(0.05, 0.1, 0.2, 0.5, 1)

# define title
title <- " Scenario 2 - max 24h" 

lp_max %>%
  ggplot() +
  geom_sf(colour="gray",  alpha=0.5, data=shp_murano) +
  geom_point(data = f3_df, 
             mapping = aes(x = x, y = y, shape="sorgente"), # a sort of trick
             color = "red", 
             size = 1, alpha = 0.8)+
  geom_point(data = rec_df, 
             mapping = aes(x = x, y = y, shape="recettori"), # a sort of trick
             color = "red", 
             size = 1, alpha = 0.8)+
  geom_contour(aes(x = x, 
                   y = y, z = z, 
                   colour = as.factor(after_stat(level))),
               linewidth=0.5,
               breaks = breaks)+
  geom_text_contour(aes(x = x, y = y, z = z ), 
                    colour = "black", 
                    alpha = 0.8, 
                    breaks = breaks,
                    stroke = 0.5, 
                    size = 2.5, 
                    skip = 0, 
                    show.legend = FALSE,
                    label.placer = label_placer_flattest(rot_adjuster = isoband::angle_halfcircle_right())
                    #label.placer = label_placer_fraction(frac = 0.5,
                    #                      rot_adjuster = isoband::angle_halfcircle_right())
                    ) +
  scale_colour_viridis_d(begin = 0, end = 1, option = "D")+
  scale_shape_manual(name="", values = c(5,3))+ # see above, about the sort of trick
  labs(colour = expression(paste("Cd [ ", ng~m^-3, " ]")), 
       title = title,
       x = NULL, y = NULL)+
  coord_sf(xlim = xlim,
           ylim = ylim,
           expand = FALSE,
           datum = st_crs(shp_murano))+
  scale_x_continuous(breaks = seq(xlim[1], xlim[2], 250))+
  scale_y_continuous(breaks = seq(ylim[1], ylim[2], 250))+
  theme_minimal()+
  theme(panel.grid.major = element_line(linetype = 3),
        axis.text.x = element_text(angle = 90, vjust = 0.5, size=8),
        axis.text.y = element_text(vjust = 0.5, size=8))
