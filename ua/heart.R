library(tidyverse)
library(sf)

heart <- st_read("./ua/heart.dxf") %>% select(geometry) %>% 
  st_union()  %>% st_polygonize() %>% first() %>% first() %>% st_geometry() %>%
  st_sfc()
heart

heart %>% st_bbox()

heart %>% ggplot() + 
  geom_sf() +
  theme_void()

st_is_valid(heart)

heart <- st_make_valid(heart)

st_centroid(heart)


heart %>% st_write("./ua/Wikimedia_Community_Logo-WOSP.shp")




# ------------------------
# victory
victory <- st_read("./ua/victory.dxf") 
d1 <- victory %>% slice(17:178) %>% st_union() %>% st_polygonize() %>%
  first() %>% first() %>% st_geometry() %>%
  st_sfc()
victory %>%  
  ggplot() + 
    geom_sf() +
    theme_void()
  
d2 <- victory %>% slice(179:308) %>% st_union() %>% st_polygonize() %>%
  first() %>% first() %>% st_geometry() %>%
  st_sfc()
d2 %>%  
  ggplot() + 
  geom_sf() +
  theme_void()

d3 <- victory %>% slice(309:337) %>% st_union() %>% st_polygonize() %>%
  first() %>% first() %>% st_geometry() %>%
  st_sfc()
d3 %>%  
  ggplot() + 
  geom_sf() +
  theme_void()

d4 <- victory %>% slice(338:366) %>% st_union() %>% st_polygonize() %>%
  first() %>% first() %>% st_geometry() %>%
  st_sfc()
d4 %>%  
  ggplot() + 
  geom_sf() +
  theme_void()
