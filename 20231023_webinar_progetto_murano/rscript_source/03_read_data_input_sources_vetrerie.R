## ---- input-25vetr ----

library(tidyverse)
library(lubridate)
library(terra)
library(leaflet)
library(mapview)


# read table vetrerie
vetr25 <- read_csv('./data_input/lista_25_vetrerie.csv')

# cache.extra = tools::md5sum("./data_input/lista_25_vetrerie.csv")

# spatial vector
vetr25<-vect(vetr25, geom=c("x", "y"), crs="epsg:32632", keepgeom=FALSE)

#reproject lon lat
vetr25<-project(vetr25, "epsg:4326")

# dataframe
vetr25<-as.data.frame(vetr25, geom="XY")

## ---- map-25vetr ----

vetr25 %>% 
  
  leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
  
  # add basemap
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
  
  # focus map in a certain area / zoom level
  setView(lng = 12.352, lat = 45.4565, zoom = 16.2) %>%
  
  ### Add scale bar
  addScaleBar(position = "bottomright")%>%
  
  # add points
  addCircleMarkers(lng = ~x,
                   lat = ~y,
                   #label = mydf$sito,
                   weight = 1,
                   color = 'black',
                   fill=FALSE,
                   #fillColor = 'red',
                   radius = 0.8,
                   opacity = 0.7,
                   #fillOpacity = 1,
                   stroke = TRUE #,
                   #labelOptions = labelOptions(noHide = TRUE, textOnly=TRUE, offset=c(0.1, 0))
  ) %>% 
  
  # add labels
  addLabelOnlyMarkers(lng = ~x+0.000085,
                      lat = ~y-0.00008,
                      label = ~abbr,
                      labelOptions = labelOptions(#offset = c(10, -18),
                        textOnly=TRUE, noHide = TRUE,
                        textsize = "10px",
                        style = list("color" = "red"))
                      )%>% 
  mapshot(., file = "./figure/map_25_vetrerie.png")

# this is including the map as image in rmarkdown output
include_graphics("./figure/map_25_vetrerie.png")



