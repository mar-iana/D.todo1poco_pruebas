### mapa de los charcos
library(sf)
library(raster)
library(tidyverse)
library(dismo)

getData("ISO3")
#245  URY Uruguay
Uruguay<-getData("GADM", country = "URY", level = 1) %>% 
  st_as_sf()
Uruguay; plot(Uruguay)
#Rocha<-Uruguay %>% dplyr::filter(NAME_1 == "Rocha") %>% rename(Region = "NAME_2")
#plot(Rocha)
#ggplot() + geom_sf(data= Rocha, aes(fill = Region)) + theme_bw()

charcos1<-Uruguay %>% st_crop(xmin = -53.995  ,ymin = -34.262 ,xmax =-53.97, ymax= -34.2425 )
#coord_55<-kk %>% st_as_sf(coords = c(3, 2), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
coord_55<-kk %>% st_as_sf(coords = c(3, 2), crs = "+proj=longlat +datum=WGS84 +no_defs")
#ggplot() + geom_sf(data= Rocha, aes(fill = Region)) + geom_sf(data = coord_55) + theme_bw()
ggplot() + geom_sf(data= charcos1, aes(fill=NAME_1)) + geom_sf(data = coord_55, ) + theme_bw() + theme(legend.position="none")
Area<-amb.vieja[,c(1,11)]
Area<-Area[(Area$charco %in% coord_55$Charco),]
#coord_53<-coord_55[(Area$charco %in%coord_55$Charco),]
coord_55<-coord_55[-c(5,37),]
coord_53_area<-cbind(coord_55,"Area"=Area$Area)
ggplot() + geom_sf(data= charcos1, aes(fill="NAME_1")) + geom_sf(data = coord_53_area, aes(size= Area), color= "red") + 
  theme_bw() + scale_fill_manual(values = "palegreen", guide = "none") #+ theme(legend.position="none")
ggplot() + geom_sf(data= charcos1, xlab="") + geom_sf_label(data = coord_53_area, aes(label=Charco ,size= Area)) + theme_bw() +
  theme(axis.title.x=element_blank(), axis.title.y = element_blank(), legend.position = "none")

library(ggmap)
sbbox <- make_bbox(lon = kk$X, lat = kk$Y, f = .1)
sbbox
sq_map <- get_map(location = sbbox, maptype = "satellite", source = "google")
ggmap(sq_map)# + geom_point(data = kk, mapping = aes(x = X, y = Y), color = "red")
qmap('Liverpool')
qmap('CH1 1AA')
map <- get_map(location = 'Australia', zoom = 4)

get_googlemap("waco texas", zoom = 12) %>% ggmap()

devtools::install_github("dkahle/ggmap", ref = "tidyup")
library(ggmap)
chicago <- get_stamenmap(bbox = c(left = -88.0225, bottom = 41.5949, 
                                  right = -87.2713, top = 42.0677), 
                         zoom = 11)
ggmap(chicago)
jj <- get_stamenmap(bbox = c(left = -53.995, bottom = -34.262, 
                                  right = -53.97, top = -34.2425), 
                         zoom = 11)
ggmap(jj)

pacman::p_load(ggmap, osmdata)
ggmap(get_map(location = getbb("texas"), zoom = 6, source = "stamen"))
ggmap(get_map(location = sbbox, source = "stamen"))
?get_stamenmap
ggmap(get_stamenmap(sbbox, maptype = "terrain", zoom = 13))

### openstreetmap

coords <- matrix(c(-34.262,-34.2425,-53.995,-53.97), byrow = TRUE, nrow = 2, ncol = 2, dimnames = list(c('x','y'),c('min','max'))) 
location <- coords %>% opq()
ggmap(get_map(location = sbbox, source = "stamen"))


