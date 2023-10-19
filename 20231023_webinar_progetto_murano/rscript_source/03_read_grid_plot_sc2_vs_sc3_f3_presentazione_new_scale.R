## ---- read-data-grid-f3-sc1 ----

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

# define breaks, unique for both plots
breaks <- c(0.002, 0.005, 0.01, 0.02, 0.06, 0.1)

# define manual colors

cols <- c("0.002" = '#3288bd',
          "0.005" =  '#99d594', 
          "0.01" = '#e6f598',
          "0.02" = '#fee08b', 
          "0.06" = '#fc8d59', 
          "0.1" = '#d53e4f')


# define title
title <- "Scenario 2 - media anno" 

lp_avg %>%
  ggplot() +
  geom_sf(colour="gray",  alpha=0.5, data=shp_murano) +
  geom_point(data = f3_df, 
             mapping = aes(x = x, y = y),
             shape = 1, 
             color = "black", 
             size = 1, alpha = 0.7, show.legend = FALSE)+
  geom_point(data = rec_df, 
             mapping = aes(x = x, y = y),
             shape = 3,
             color = "black", 
             size = 1, 
             alpha = 0.7)+
  geom_contour(aes(x = x, y = y, z = z, colour = as.factor(after_stat(level))),
                   linewidth = 0.5,
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
  #scale_colour_viridis_d(begin = 0, end = 1, option = "D")+
  scale_colour_manual(values = cols)+
  #scale_shape_manual(name=NULL, values = c(5,3))+ # see above, about the sort of trick
  labs(colour = NULL,
       #colour = expression(paste("Cd [ ", ng~m^-3, " ]")), 
       title = title,
       x = NULL, y = NULL)+
  coord_sf(xlim = xlim,
           ylim = ylim,
           expand = FALSE,
           datum = st_crs(shp_murano))+
  scale_x_continuous(breaks = seq(xlim[1], xlim[2], 250))+
  scale_y_continuous(breaks = seq(ylim[1], ylim[2], 250))+
  guides(colour = guide_legend(nrow = 1))+
  theme_minimal()+
  theme(panel.grid.major = element_line(linetype = 3),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, size=8),
        axis.text.y = element_text(vjust = 0.5, size=8)) -> sc2_avg

## ---- ggplot-f3-sc3-avg ----

# file lapmod avg period
fl <- './data_input/grd_f3_sc_3/Average_Period.grd '

# lapost from raster to dataframe average
lp_avg <- raster(fl) %>%
  as.data.frame(., xy=TRUE) %>%
  rename(z=3)

# define xy extent
#ll <- c(761500, 5037750)
#xlim <- c(ll[1], ll[1]+1500)
#ylim <- c(ll[2], ll[2]+1750)

# define title
title <- "Scenario 3 - media anno" 

lp_avg %>%
  ggplot() +
  geom_sf(colour="gray",  alpha=0.5, data = shp_murano) +
  geom_point(data = f3_df, 
             mapping = aes(x = x, y = y),
             shape = 1, 
             color = "black", 
             size = 1, alpha = 0.7, show.legend = FALSE)+
  geom_point(data = rec_df, 
             mapping = aes(x = x, y = y),
             shape = 3,
             color = "black", 
             size = 1, 
             alpha = 0.7)+
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
                    #label.placer = label_placer_fraction(frac=0.4, rot_adjuster = isoband::angle_halfcircle_right())
                    label.placer = label_placer_flattest()
                    ) +
  #scale_colour_viridis_d(begin = 0, end = 1, option = "D")+
  scale_colour_manual(values = cols)+
  #scale_shape_manual(name = NULL, values = c(5,3))+ # see above, about the sort of trick
  labs(colour = NULL, 
       #colour = expression(paste("Cd [ ", ng~m^-3, " ]")), 
       title = title,
       x = NULL, y = NULL) +
  coord_sf(xlim = xlim,
           ylim = ylim,
           expand = FALSE,
           datum = st_crs(shp_murano))+
  scale_x_continuous(breaks = seq(xlim[1], xlim[2], 250)) +
  scale_y_continuous(breaks = seq(ylim[1], ylim[2], 250)) +
  guides(colour = guide_legend(nrow = 1))+ 
  theme_minimal() +
  theme(panel.grid.major = element_line(linetype = 3),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, size=8),
        axis.text.y = element_text(vjust = 0.5, size=8)) -> sc3_avg

#########################################################################################################

# plot

sc2_avg | sc3_avg + plot_layout(nrow = 1, widths = 1, heights = 1)

ggsave('contour_sc2_vs_sc3_avg_new_scale.png', path='./img_pres/', width = 10, height = 5)
