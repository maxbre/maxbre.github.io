## ---- read-data-input ----

library(tidyverse)
library(terra)
library(openair)
library(patchwork)
library(leaflet)
library(lubridate)

# read data for monitoring points
p <- read_csv('./data_input/murano_siti_monitor_cd.csv')

# read table vetrerie
vetr25 <- read_csv('./data_input/lista_25_vetrerie.csv')

# read prtmet dat
# read headings, first row, i.e. col names
col_names <- read_table('./data_input/prtmet.dat', skip= 1, n_max = 0) %>% names() %>% tolower()

# read data, skip row and specify col names
met<-read_table('./data_input/prtmet.dat', 
                col_names= col_names, 
                skip=4, 
                na='*********' # pay attention to this!
                )%>%
  select(!c('year', 'month', 'day', 'hour', 'sec'))%>%
  rename_with(~gsub("_1", "", .x, fixed = TRUE))%>% # here fixed is to match the string literally, no regexpr
  rename_with(~gsub("*", "_star", .x, fixed = TRUE))%>%
  rename_with(~gsub(".", "_", .x, fixed = TRUE)) # gsub dot "." with "_", note the use of fixed

# create and format date column with lubridate
met$date<-ymd_h(paste0(met$year, '-', met$month, '-', met$day, ' ', met$hour))

## ---- map-25vetr ----

# cache.extra = tools::md5sum("./data_input/lista_25_vetrerie.csv")

# spatial vector
vetr25<-vect(vetr25, geom=c("x", "y"), crs="epsg:32632", keepgeom=FALSE)

#reproject lon lat
vetr25<-project(vetr25, "epsg:4326")

# dataframe
vetr25<-as.data.frame(vetr25, geom="XY")

# library(leaflet)
# library(leafem)

vetr25 %>% 
  
  leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
  
  # add basemap
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # focus map in a certain area / zoom level
  setView(lng = 12.353, lat = 45.45506, zoom = 15) %>%
  
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
                   radius = 1,
                   opacity = 0.4,
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
                        textsize = "8px",
                        style = list("color" = "red"))
                      )


## ---- wind-rose ----

# windrose by month
windRose(met,
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

## ---- wind-rose-day-night ----

# windrose by daylight nigthtime

windRose(met,
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

## ---- wind-barplot ----

# factor cut wind direction by user input intervals
wsf<-cut(met$ws,
         c(0, 0.5, 1, 2, 3, 6, max(met$ws)),
         labels=c('< 0.5','[ 0.5, 1 )','[ 1, 2 )','[ 2, 3 )','[ 3, 6 )','>= 6'),
         include.lowest=TRUE, right=FALSE)

# frequency distribution of wind classes
wsfd<-met %>%
  mutate(recs_all=n())%>%
  group_by(ws_class=wsf) %>%
  summarise(recs=n(),
            perc=recs/first(recs_all)*100)

#barplot of wind classes, colour
ggplot(wsfd, aes(x=ws_class, y=perc, fill=ws_class))+
  geom_col(show.legend=FALSE)+
  geom_text(aes(label=round(perc,1)), vjust=-0.3, size=3.5)+
  ylim(0,100)+
  #scale_y_continuous('%', limits=c(0,100))+
  labs(#title=mysite, 
    y='frequenza %',
    x=expression(paste("[ ", m~s^-1, " ]"))
    )+
  theme_bw()

## ---- map-3siti-qa ----

# cache.extra = tools::md5sum("./data_input/murano_siti_monitor_cd.csv")

# library(sf)
# 
# # set original crs
# p_sf <- st_as_sf(p, coords = c("x", "y"), crs = 32632)
# # transformed crs
# p_sf <- st_transform(p_sf, 4326)
# 
# library(leafem)
# 
# p_sf %>%
#   # start basemap
#   leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
#   
#   # add basemap
#    addProviderTiles(providers$CartoDB.Positron) %>%
#   
#   # focus map in a certain area / zoom level
#   setView(lng = 12.35, lat = 45.455, zoom = 15) %>%
#  
#   ### Add scale bar
#   addScaleBar()%>%
#   
#   # add points
#   addCircleMarkers(weight = 0.5,
#                    col = 'white', 
#                    fillColor = 'red',
#                    radius = 3, 
#                    fillOpacity = 0.9, 
#                    stroke = T)  %>% 
#   # add labels
#   addStaticLabels(.,label=p_sf$sito, offset = c(0, 0.0001), style = list("color" = "red")) 

# spatial vector
p <- vect(p, geom=c("x", "y"), crs="epsg:32632", keepgeom=FALSE)

# reproject to lon lat
p <- project(p, "epsg:4326")

# here working again with plain df
p <- as.data.frame(p, geom="XY")

p %>%
  leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
  
  # add basemap
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # focus map in a certain area / zoom level
  setView(lng=12.35, lat=45.456, zoom = 15) %>%
  
  ### Add scale bar
  addScaleBar()%>%
  
  # add points
  addCircleMarkers(lng = ~x,
                   lat = ~y,
                   #label = mydf$sito,
                   weight = 0.5,
                   #color = 'white',
                   fillColor = 'red',
                   radius = 6,
                   fillOpacity = 0.9,
                   stroke = FALSE) %>% 
  
  # add labels
  addLabelOnlyMarkers(lng = ~x+0.0003,
                      lat = ~y+0.0005,
                      label = ~sito,
                      labelOptions = labelOptions(#offset = c(10, -18),
                        textOnly=TRUE, noHide = TRUE, 
                        textsize = "12px",
                        style = list("color" = "red"))
                      )
