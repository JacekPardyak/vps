library(tidyverse)
library(sf)
library(s2)
library(av)
library(gganimate)
theme_set(theme_bw())

#fit <- function(x)  (x * 0.1 + c(30, 40))  
#drawing <- st_read('./ua/land.dxf', as_tibble = FALSE, stringsAsFactors = TRUE) %>%
#  select(geometry) %>% # st_union() 
#  fit() %>% 
#  st_sf(geometry = .) %>%
#  mutate(colour = "#d4213d") %>% mutate(facet = 0)
#drawing
audio = "./ua/Shouse-Love-Tonight.mp3"
ua = s2_data_countries("Ukraine") %>% first() %>% first() %>% st_as_sf() #%>% 
# mutate(colour = "#000000")  %>% mutate(facet = 0)
ua
heart <- st_read("/tmp/heart.dxf") %>% select(geometry) %>% 
  st_union()  %>% st_polygonize() %>% first() %>% first()
heart


cloud_1 <- st_sample(ua, 1000) %>% st_sf(geometry = .) %>% st_as_sf() %>% 
  mutate(colour = ifelse(st_coordinates(geometry)[,'Y'] >
                           mean(c(st_bbox(ua)['ymin'], st_bbox(ua)['ymax'])) , "#FFD500", "#005BBB"))  %>%
    mutate(facet = 1)
cloud_2 <- st_sample(ua, 1000) %>% st_sf(geometry = .) %>% st_as_sf() %>% 
  mutate(colour = ifelse(st_coordinates(geometry)[,'Y'] >
                           mean(c(st_bbox(ua)['ymin'], st_bbox(ua)['ymax'])) , "#FFD500", "#005BBB"))  %>%
  mutate(facet = 2)

cloud_3 <- st_sample(ua, 1000) %>% st_sf(geometry = .) %>% st_as_sf() %>% 
  mutate(colour = ifelse(st_coordinates(geometry)[,'Y'] >
                           mean(c(st_bbox(ua)['ymin'], st_bbox(ua)['ymax'])) , "#FFD500", "#005BBB"))  %>%
  mutate(facet = 3)



cloud <- cloud_1  %>% bind_rows(cloud_2) %>% bind_rows(cloud_3)
p <- cloud %>% 
  ggplot() +
  aes(colour = colour, fill = colour) +
  geom_sf(aes(group = 1L)) +
#  geom_sf(data = cloud, aes(group = 1L), size = 5) +  #shape = "\u2665", 
  scale_colour_identity() +
  scale_fill_identity() +
  theme_void() + 
  transition_time(facet) +
  ease_aes("linear") 
#p

p %>%
  animate(renderer = av_renderer(audio = audio))
anim_save('./ua/output.mp4')
