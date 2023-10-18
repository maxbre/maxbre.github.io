## ---- input-monitor-qa-points ----

library(tidyverse)
library(terra)
library(leaflet)
library(mapview)

# read data for monitoring points
p <- read_csv('./data_input/murano_siti_monitor_cd.csv')

# spatial vector
p <- vect(p, geom=c("x", "y"), crs="epsg:32632", keepgeom=FALSE)

# reproject to lon lat
p <- project(p, "epsg:4326")

# here working again with plain df
p <- as.data.frame(p, geom="XY")

## ---- map-3siti-qa ----

p %>%
  leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
  
  # add basemap
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
  
  # focus map in a certain area / zoom level
  setView(lng=12.35, lat=45.455, zoom = 16) %>%
  
  ### Add scale bar
  addScaleBar()%>%
  
  # add points
  addCircleMarkers(lng = ~x,
                   lat = ~y,
                   #label = mydf$sito,
                   weight = 0.5,
                   #color = 'white',
                   fillColor = 'red',
                   radius = 3,
                   fillOpacity = 0.9,
                   stroke = FALSE) %>% 
  
  # add labels
  addLabelOnlyMarkers(lng = ~x+0.0002,
                      lat = ~y+0.0003,
                      label = ~sito,
                      labelOptions = labelOptions(#offset = c(10, -18),
                      textOnly=TRUE, noHide = TRUE, 
                      textsize = "12px",
                      style = list("color" = "red"))
                      )%>% 
  mapshot(., file = "./figure/map_3siti_qa.png")

# this is including the map as image in rmarkdown output
include_graphics("./figure/map_3siti_qa.png")

