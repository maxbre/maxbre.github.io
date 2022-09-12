library(raster)
library(sp)
library(mapview)

grd<-raster('./rank(0)_cd_2952hr_conc_cd.grd')

# change coordinates units from km to m
# pay attention to the order, change first max x and y values
xmax(grd) <- xmax(grd)*1000
xmin(grd) <- xmin(grd)*1000

ymax(grd) <- ymax(grd)*1000
ymin(grd) <- ymin(grd)*1000

plot(grd)


# epsg 32632
mycrs<-"+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"
crs(grd) <- mycrs 

# sp approach for setting crs, same as above
#proj4string(grd)<- mycrs
# string WGS 84 / UTM zone 32N cartesian
#epsg_in<-'+init=epsg:32632'
# set original coordinates system
#proj4string(grd) <- CRS(epsg_in)

mapview(grd, alpha.regions=0.5)

#############################

grd2<-raster('./rank(1)_cd_1hr_conc_cd.grd')

# change coordinates units from km to m
# pay attention to the order, change first max x and y values
xmax(grd2) <- xmax(grd2)*1000
xmin(grd2) <- xmin(grd2)*1000

ymax(grd2) <- ymax(grd2)*1000
ymin(grd2) <- ymin(grd2)*1000

plot(grd2)


# epsg 32632
mycrs<-"+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"
crs(grd2) <- mycrs 

# sp approach for setting crs, same as above
#proj4string(grd2)<- mycrs
# string WGS 84 / UTM zone 32N cartesian
#epsg_in<-'+init=epsg:32632' 
# set original coordinates system
#proj4string(grd2) <- CRS(epsg_in)

mapview(grd2, alpha.regions=0.5)

################################################################################

grd3<-raster('./rank(1)_cd_24hr_conc_cd.grd')

# change coordinates units from km to m
# pay attention to the order, change first max x and y values
xmax(grd3) <- xmax(grd3)*1000
xmin(grd3) <- xmin(grd3)*1000

ymax(grd3) <- ymax(grd3)*1000
ymin(grd3) <- ymin(grd3)*1000

plot(grd3)


# epsg 32632
mycrs<-"+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"
crs(grd3) <- mycrs 

# sp approach for setting crs, same as above
#proj4string(grd3)<- mycrs
# string WGS 84 / UTM zone 32N cartesian
#epsg_in<-'+init=epsg:32632' 
# set original coordinates system
#proj4string(grd3) <- CRS(epsg_in)

mapview(grd3, alpha.regions=0.5)


