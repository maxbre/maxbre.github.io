## ---- read-map-grid-output ----

library(sf)
library(raster)
library(tidyverse)
library(ggspatial)
library(scales)

# read background shapefile
shp_murano <- read_sf('./data_input/shp_murano/murano.shp')

# read grid calpuff - period average
cp_r_avg <- raster('./data_input/calpost_grid/rank(0)_cd_2952hr_conc_cd.grd') 

# remember the CALPUFF extent in km
new_extent <- raster::extent(raster::xmin(cp_r_avg) * 1000, raster::xmax(cp_r_avg) * 1000, 
                             raster::ymin(cp_r_avg) * 1000, raster::ymax(cp_r_avg) * 1000)
# transform in meters
cp_r_avg <- raster::setExtent(cp_r_avg, new_extent)

# set the appropriate crs
raster::crs(cp_r_avg) <- 32632

# read grid lapmod - period average
lp_r_avg <- raster('./data_input/lapost_grid/annual.grd', 
                   crs="+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs")

# transform to df, it is a necessary preliminary step to later ggplot
cp_avg_df <- cp_r_avg %>% 
  as.data.frame(., xy=TRUE) %>% 
  rename('z' = 3)

lp_avg_df <- lp_r_avg %>%
  as.data.frame(., xy=TRUE) %>% 
  rename('z' = 3)

## ---- ggplot-map-grid-calpuff-avg ----

# calpuff plot map average
shp_murano %>% 
  ggplot() + 
  geom_sf(colour="gray",  alpha=0.5) +
  geom_raster(data = cp_avg_df, mapping = aes(x = x, y = y, fill = z),
              alpha = 0.5)+
  scale_fill_viridis_c(#name = "Cd\n[ng/m3]",
                       name = expression(paste("Cd [ ", ng~m^-3, " ]")),
                       breaks=breaks_pretty(5),
                       #breaks=breaks_width(0.5),
                       option="viridis", 
                       alpha = 0.5)+
  annotation_scale(location = "br", width_hint=0.4, line_width = 1,
                   height = unit(0.2, "cm"),
                   pad_x = unit(0.65, "cm"),
                   pad_y = unit(0.65, "cm"),) +
  annotation_north_arrow(location = "br", which_north = "true",
                         height = unit(0.9, "cm"),
                         width = unit(0.9, "cm"),
                         pad_x = unit(0.55, "cm"),
                         pad_y = unit(0.85, "cm"),
                         style = north_arrow_fancy_orienteering) +
  #coord_sf(datum=32632)+ # this is to set CRS that provides datum to use when generating graticules
  coord_sf(datum = st_crs(shp_murano))+ # this is the very same result as above but programmatically taking crs from the specified layer
  #labs(x=NULL, y=NULL)+
  theme_void()+
  #theme_minimal()
  theme(legend.position = "bottom")

## ---- ggplot-map-grid-lapmod-avg ----

# lapmod plot map average
shp_murano %>% 
  ggplot() + 
  geom_sf(colour="gray",  alpha=0.5) +
  geom_raster(data = lp_avg_df, mapping = aes(x = x, y = y, fill = z),
              alpha = 0.5)+
  scale_fill_viridis_c(#name = "Cd\n[ng/m3]",
                       name = expression(paste("Cd [ ", ng~m^-3, " ]")),
                       breaks=breaks_pretty(5),
                       #breaks=breaks_width(0.5),
                       option="viridis", 
                       alpha = 0.5)+
  annotation_scale(location = "br", width_hint=0.4, line_width = 1,
                   height = unit(0.2, "cm"),
                   pad_x = unit(0.65, "cm"),
                   pad_y = unit(0.65, "cm"),) +
  annotation_north_arrow(location = "br", which_north = "true",
                         height = unit(0.9, "cm"),
                         width = unit(0.9, "cm"),
                         pad_x = unit(0.55, "cm"),
                         pad_y = unit(0.85, "cm"),
                         style = north_arrow_fancy_orienteering) +
  #coord_sf(datum=32632)+ # this is to set CRS that provides datum to use when generating graticules
  coord_sf(datum = st_crs(shp_murano))+ # this is the very same result as above but programmatically taking crs from the specified layer
  #labs(x=NULL, y=NULL)+
  coord_sf(datum=32632) +
  theme_void()+
  #theme_minimal()
  theme(legend.position = "bottom")
