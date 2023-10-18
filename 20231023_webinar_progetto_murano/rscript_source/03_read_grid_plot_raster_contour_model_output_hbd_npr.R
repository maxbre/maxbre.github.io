## ---- read-map-grid-output-hbd-npr ----

library(raster)
library(tidyverse)
library(metR)

# raster for average period
rdf_avg <- raster('./data_input/lapost_grid_hbd_npr/annual.grd') %>% 
  as.data.frame(.,xy=TRUE) %>% 
  rename(z=3)

max_avg <- rdf_avg %>% 
  summarise(round(max(z),1)) %>% 
  pull()

# raster for max 24 h period
rdf_max <- raster('././data_input/lapost_grid_hbd_npr/max24h.grd') %>% 
  as.data.frame(.,xy=TRUE) %>% 
  rename(z=3)

max_max <- rdf_max %>% 
  summarise(round(max(z),1)) %>% 
  pull()

# raster for p98 24 h period
rdf_p98 <- raster('././data_input/lapost_grid_hbd_npr/pct24h98.grd') %>% 
  as.data.frame(.,xy=TRUE) %>% 
  rename(z=3)

max_p98 <- rdf_p98 %>% 
  summarise(round(max(z),1)) %>% 
  pull()

#expression(paste("PM10 [ ", mu*g~m^-3, " ]"))
#expression(paste("Cd [ ", ng~m^-3, " ]"))

## ---- ggplot-raster-contour-hbd-npr-avg ----

# average period

avg_breaks <- c(0.1, 0.3, 0.5, 1, 3, 5)

rdf_avg %>%
  ggplot() +
  geom_raster(aes(x=x, y=y, fill=z), alpha=0.8)+
  geom_contour(aes(x=x, y=y, z=z),
               colour="white",
               linewidth=0.2,
               breaks= avg_breaks)+
  geom_text_contour(aes(x=x, y=y, z = z),
                    stroke = 0.2, size=2.5, skip = 0,
                    breaks= avg_breaks) +
  labs(title = paste0("LAPMOD, media periodo [max = ", max_avg, "]"))+
  scale_fill_viridis_c(begin=0,
                       name=expression(paste("Cd [ ", ng~m^-3, " ]")), 
                       alpha = 0.8, option="D")+
  coord_equal()+
  #theme_minimal()+
  theme_void()+
  theme(legend.position="bottom")


## ---- ggplot-raster-contour-hbd-npr-p98-m24h ----

# p98 m24h

p98_breaks <- c(0.5, 1, 3, 5, 10, 30) 

rdf_p98 %>%
  ggplot() +
  geom_raster(aes(x=x, y=y, fill=z), alpha=0.8)+
  geom_contour(aes(x=x, y=y, z=z),
               colour="white",
               linewidth=0.2,
               breaks= p98_breaks)+
  geom_text_contour(aes(x=x, y=y, z = z),
                    stroke = 0.2, size=2.5, skip = 0,
                    breaks = p98_breaks) +
  labs(title = paste0("LAPMOD, 98° percentile 24h [max = ", max_p98, "]"))+
  scale_fill_viridis_c(begin=0.1,
                       name=expression(paste("Cd [ ", ng~m^-3, " ]")), 
                       alpha = 0.8, option="D")+
  coord_equal()+
  #theme_minimal()+
  theme_void()+
  theme(legend.position="bottom")

## ---- ggplot-raster-contour-hbd-npr-max-m24h ----

# max m24h 

max_breaks <- c(0.5, 1, 3, 5, 10, 30) 

rdf_max %>%
  ggplot() +
  geom_raster(aes(x=x, y=y, fill=z), alpha=0.8)+
  geom_contour(aes(x=x, y=y, z=z),
               colour="white",
               linewidth=0.2,
               breaks= max_breaks)+
  geom_text_contour(aes(x=x, y=y, z = z),
                    stroke = 0.2, size=2.5, skip = 0,
                    breaks = max_breaks) +
  labs(title = paste0("LAPMOD, valore massimo 24h [max = ", max_max, "]"))+
  scale_fill_viridis_c(begin=0.1,
                       name=expression(paste("Cd [ ", ng~m^-3, " ]")), 
                       alpha = 0.8, option="D")+
  coord_equal()+
  #theme_minimal()+
  theme_void()+
  theme(legend.position="bottom")

