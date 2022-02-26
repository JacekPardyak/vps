library(tidyverse)
library(sf)

heart <- st_read("./ua/heart.dxf") %>% select(geometry) %>% 
  st_union()  %>% st_polygonize() %>% first() %>% first() %>% st_geometry() %>%
  st_sfc() %>% st_set_crs(3857) %>% st_transform(4326)
heart

heart %>% st_bbox()

heart %>% plot()
heart %>% ggplot() + 
  geom_sf() +
  theme_void()

st_is_valid(heart)

heart <- st_make_valid(heart)

st_centroid(heart)


heart %>% st_write("./ua/Wikimedia_Community_Logo-WOSP.shp")


library(svglite)
library(ggplot2)
# SVG sizes are in inches, not pixels
res <- 144
svglite("./ua/mtcars.svg", width = 1080/res, height = 720/res)
plot(image)
dev.off()

heart <- st_read("./ua/mtcars.svg")


# ------------------------

lon = c(756065.70, 757428.78)
lat = c(4074435.19,4075144.12)


Poly_Coord_df = data.frame(lon, lat)
Poly_Coord_df %>% plot()

poly <- Poly_Coord_df %>% 
  st_as_sf(coords = c("lon", "lat"), 
           crs = 32611) %>% 
  st_bbox() %>% 
  st_as_sfc()

library(maps); library(ggplot2); library(mapproj)
states <- map_data("state")
usamap <- ggplot(states, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill="white", colour="black")
usamap + coord_map("mercator")

usamap + coord_map("azequalarea") 


sfc = st_sfc(st_point(c(0,0)), st_point(c(1,181)))
sf = st_sf(a = 1:2, geom = sfc)
st_crs(sf) = 4326
st_geometry(sf)

sf %>% st_bbox()

