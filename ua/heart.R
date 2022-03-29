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
  st_sfc() %>% st_as_sf() %>%
  mutate(facet = 0) %>% mutate(colour = "#000000")
  
d2 <- victory %>% slice(179:308) %>% st_union() %>% st_polygonize() %>%
  first() %>% first() %>% st_geometry() %>%
  st_sfc() %>% st_as_sf() %>%
  mutate(facet = 0) %>% mutate(colour = "#000000")

d3 <- victory %>% slice(309:337) %>% st_union() %>% st_polygonize() %>%
  first() %>% first() %>% st_geometry() %>%
  st_sfc() %>% st_as_sf() %>%
  mutate(facet = 0) %>% mutate(colour = "#005BBB")

d4 <- victory %>% slice(338:366) %>% st_union() %>% st_polygonize() %>%
  first() %>% first() %>% st_geometry() %>%
  st_sfc() %>% st_as_sf() %>%
  mutate(facet = 0) %>% mutate(colour = "#FFD500")

data <- d1 %>% bind_rows(d2) %>% bind_rows(d3) %>% bind_rows(d4)

data %>%
  ggplot() + 
  aes(colour = colour, fill = colour) +
  geom_sf(aes(group = 1L)) +
  scale_colour_identity() +
  scale_fill_identity() +
  theme_void() + 
  theme(panel.background = element_rect(fill = 'white'))

ggsave("./ua/victory.png")
