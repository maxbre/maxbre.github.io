# function to save mapview in html

save_mapview_html<-function(my_mapview, string_filename_html){
  
  # string date
  my_date<-format(Sys.Date(), "%Y%m%d")
  
  ## compose filename html
  my_filename<-paste0(my_date, '_', string_filename_html, '.html')
  
  #create both html and png, and remove some controls in png
  mapview::mapshot(my_mapview,
                   url = my_filename,
                   file=paste0(my_date, '_', string_filename_html, '.png'),
                   remove_controls = c("zoomControl", "layersControl", "homeButton", "scaleBar"))
  
  my_mapview
  
}

# function to plot the map and save the html file

plot_aermod_map<-function(input_filename,
                          rows_to_skip, 
                          name_of_map_layer,
                          string_filename_html)  {
  
  library(magrittr)
  
  plt<-readr::read_table2(input_filename, col_names=FALSE, skip=rows_to_skip) %>%
    dplyr::select(x=1, y=2, z=3)
  
  # coordinates to define a spatial point data frame
  sp::coordinates(plt)<- ~x+y
  # string WGS 84 / UTM zone 32N cartesian
  epsg_in<-'+init=epsg:32632'
  # set original coordinates system
  sp::proj4string(plt) <- sp::CRS(epsg_in)
  # set it as a grid
  sp::gridded(plt)<-TRUE
  # convert to a raster
  pltr <- raster::raster(plt)
  
  
  
  # define the binning
  #my_bins<-c(0, 1, 2, 3, 4, 5, 10, 100, round(max(raster::values(pltr)),0))
  #my_bins<-pretty(range(raster::values(pltr)))
  
  
  map_r<-mapview::mapview(pltr,
                          #here define the palette
                          col.regions = grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(8, 'RdYlBu'))),
                          #at = my_bins,
                          na.color ='transparent',
                          alpha.region= 0.5,
                          legend.opacity=0.5,
                          #query.digits=1,
                          query.position = 'bottomright',
                          #query.prefix = NULL,
                          layer.name = name_of_map_layer,
                          map.types = c('CartoDB.Positron','OpenStreetMap','Esri.WorldImagery')
  )
  
  save_mapview_html(map_r, string_filename_html)

}

n_rec<-0

# this is for the interactive mode
# comment uncomment accordingly

# mytxt<-"\n**** HOW MANY DISCRETE RECEPTORS? ****\n\n please enter number of discrete receptors, 0 (zero) if none\n"
# cat(mytxt)
# 
# n_rec<- as.numeric(readLines("stdin", n=1))
# cat("\n")
# 
# mymsg<-paste0("\n**** you set ", n_rec, " discrete receptors ****\n")
# cat(mymsg)
# cat("\n")



rows_to_skip<-8+n_rec

map_avg<-plot_aermod_map('./PLT_PERIOD_ALL_AVE.PLT',
                         rows_to_skip,
                         'AERMOD<br>SO2 [ug*m^-3]<br>AVG period',
                         'AERMOD_AVG')

map_avg

map_p098<-plot_aermod_map('./PLT_PERIOD_ALL_176_P98.PLT',
                         rows_to_skip,
                         'AERMOD<br>SO2 [ug*m^-3]<br>P98 1h',
                         'AERMOD_P098')

map_p098

map_p099<-plot_aermod_map('./PLT_PERIOD_ALL_88_P99.PLT',
                         rows_to_skip,
                         'AERMOD<br>SO2 [ug*m^-3]<br>P99 1h',
                         'AERMOD_P099')

map_p099

map_p100<-plot_aermod_map('./PLT_PERIOD_ALL_1_MAX.PLT',
                          rows_to_skip,
                          'AERMOD<br>SO2 [ug*m^-3]<br>P100 1h',
                          'AERMOD_P100')

map_p100

library(leafsync)

map_all<-sync(map_avg, map_p098, map_p099, map_p100)

map_all

