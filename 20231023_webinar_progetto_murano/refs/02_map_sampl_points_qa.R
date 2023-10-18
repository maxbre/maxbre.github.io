# library(leaflet)
# 
# myicon <- awesomeIcons(
#   icon = 'home',
#   library = 'glyphicon',
#   iconColor = 'white',
#   markerColor = 'orange')
# 
# leaflet() %>%
#   addTiles() %>% 
#   setView(lat = 45.455609, lng = 12.353883, zoom = 12) %>%
#   addAwesomeMarkers(lat = 45.455609, lng = 12.353883, popup="campionatore PM10", icon=myicon)
# 

library(leaflet)
library(mapview)

ptlat <- c(45.455609, 45.4285)
ptlon <- c(12.353883, 12.313)

# start basemap (note the argument to hide the zoom buttons)
leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
  
  # add basemap
  #addProviderTiles(providers$Stamen.TonerLite) %>%
  #addProviderTiles(providers$Stamen.Terrain) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # focus map in a certain area / zoom level
  #setView(lng = ptlon, lat = ptlat, zoom = 18) %>%
  setView(lng= 12.339, lat = 45.4435, zoom = 13.5) %>%
   
  # add inset map
  addMiniMap(
    #tiles = providers$Stamen.TonerLite,
    #tiles = providers$Stamen.Terrain,
    tiles = providers$CartoDB.Positron,
    position = 'bottomright', 
    width = 100, height = 100,
    toggleDisplay = FALSE) %>%
  
  ### Add scale bar
  addScaleBar()%>%
  
  # add points (as circle markers)
  addCircleMarkers(lng = ptlon,
                   lat = ptlat,
                   weight = 0.5,
                   col = 'white', 
                   fillColor = c('red', 'blue'),
                   radius = 5, 
                   fillOpacity = 0.9, 
                   stroke = T) %>% 
  mapshot(., file = "./figure/map_sampl_points_qa.png")

# this is including the map as image in rmarkdown output
include_graphics("./figure/map_sampl_points_qa.png")
