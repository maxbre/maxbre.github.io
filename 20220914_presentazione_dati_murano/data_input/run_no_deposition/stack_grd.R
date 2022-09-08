library(raster)

rList <- list.files(path="./", pattern = "\\.grd$")

rasterList <- lapply(1:length(rList), function(x) raster(rList[x]))

rs <- stack(rasterList)                     

xmax(rs) <- xmax(rs)*1000
xmin(rs) <- xmin(rs)*1000

ymax(rs) <- ymax(rs)*1000
ymin(rs) <- ymin(rs)*1000

area(rs)

plot(rs)
#animate(rs)
#spplot(rs)

#rasterBrick <- brick(rasterList)
#plot(rasterBrick)

#https://www.youtube.com/watch?v=NQZNpyEgVss
rs[[1]]
names(rs)[[1]]

plot(subset(rs, "rank.0._cd_2952hr_conc_cd"))

r1<-subset(rs, "rank.0._cd_2952hr_conc_cd")

plot(r1)
contour(r1, col="red",levels=c(0.05, 0.1, 0.2), method="edge", lty="dashed", add=TRUE)

image(r1, col=terrain.colors(6), axes=FALSE)

r1_df<-as.data.frame(r1, xy=TRUE)

library(tidyverse)

r1_df<-r1_df%>%
  mutate(x=x*1000, y=y*1000)%>%
  rename(z=last_col())

#names(r1_df)[3]<-"z"

r1_df%>%
  ggplot()+
  geom_raster(aes(x = x, y = y, fill = z)) + 
  #scale_fill_gradientn(name = "cd", colors = terrain.colors(10)) + 
  scale_fill_viridis_c(name = "Cd", alpha=0.5, option="D")+
  coord_quickmap()+
  cowplot::theme_map()
