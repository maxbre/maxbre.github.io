
library(tidyverse)
library(sf)
library(raster)
library(metR)
library(patchwork)

## ---- ggplot-f3-sc1-avg ----

# file lapmod avg period
fl <- './data_input/grd_f3_sc_1/Average_Period.grd '

# lapost from raster to dataframe average
lp_avg <- raster(fl) %>%
  as.data.frame(., xy=TRUE) %>%
  rename(z=3)

# define xy extent
ll <- c(761500, 5038000)
xlim <- c(ll[1], ll[1]+1500)
ylim <- c(ll[2], ll[2]+1500)

# define breaks
breaks <- c(0.05, 0.1, 0.5, 1, 2, 3)
# define colors
#cols <- c("0.05" = "cyan", "0.1" = "blue", "0.5" = "darkgreen", "1" = "orange", "2" = "red", "3"="purple")

cols <- c("0.05" = '#4575b4',
          "0.1" =  '#91bfdb', 
          "0.5" = '#e0f3f8',
          "1" = '#fee090', 
          "2" = '#fc8d59', 
          "3" = '#d73027')

title <- "Scenario 1 - media anno"

lp_avg %>%
  ggplot() +
  geom_contour(aes(x = x, y = y, z = z, 
                   colour = as.factor(after_stat(level)),
                   ),
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
                    label.placer = label_placer_flattest())+
  scale_colour_manual(values = cols)+
  #scale_colour_viridis_d(begin = 0, end = 1, option = "B")+
  labs(colour = NULL,
       #colour = expression(paste("Cd [ ", ng~m^-3, " ]")), 
       title = title,
       x = NULL, y = NULL)


######################à

# file lapmod avg period
fl <- './data_input/grd_f3_sc_2/Average_Period.grd '

# lapost from raster to dataframe average
lp_avg <- raster(fl) %>%
  as.data.frame(., xy=TRUE) %>%
  rename(z=3)

# define title
title <- "Scenario 2 - media anno" 

lp_avg %>%
  ggplot() +
  geom_contour(aes(x = x, y = y, z = z, 
                   colour = as.factor(after_stat(level)),
  ),
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
                    label.placer = label_placer_flattest())+
  #scale_colour_viridis_d(begin = 0, end = 1, option = "B")+
  scale_colour_manual(values = cols)+
  labs(colour = NULL,
       #colour = expression(paste("Cd [ ", ng~m^-3, " ]")), 
       title = title,
       x = NULL, y = NULL)


