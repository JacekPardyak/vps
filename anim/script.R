library(tidyverse)
library(sf)
library(gganimate)

data <- st_read("https://raw.githubusercontent.com/JacekPardyak/vps/master/data/logo/logo.dxf")
data
layers <- data %>% data.frame() %>% distinct(Layer) %>% pull()
layers

# Layer 1
layer <- layers[1]
layer_1 <- data %>% filter(Layer == layer) %>% st_union() %>% st_polygonize() %>% first()  %>% first()
layer_1 <- layer_1  %>%
  st_sfc() %>% st_sf(geometry = .) %>% mutate(Layer = layer)
layer_1
layer_1 %>% plot()

# Layer 2
layer_in  <- layers[2]
layer_out <- layers[3]

layer_2_in  <- data %>% filter(Layer == layer_in) %>% st_union() %>% st_polygonize() 
layer_2_out <- data %>% filter(Layer == layer_out) %>% st_union() %>% st_polygonize() 

layer_2 <- st_difference(layer_2_out, layer_2_in)
layer_2 <- layer_2  %>%
  st_sfc() %>% st_sf(geometry = .) %>% mutate(Layer = layer)
layer_2 %>% plot()

layer_2

# Layer 3
layer = "Layer 3"
layer_in  <- layers[4]
layer_out <- layers[5]

layer_in  <- data %>% filter(Layer == layer_in) %>% st_union() %>% st_polygonize() 
layer_out <- data %>% filter(Layer == layer_out) %>% st_union() %>% st_polygonize() 

layer_3 <- st_difference(layer_out, layer_in)
layer_3 <- layer_3  %>%
  st_sfc() %>% st_sf(geometry = .) %>% mutate(Layer = layer)
layer_3 %>% plot()


## data
data <- (layer_2 %>% st_sample(size = 1000) %>%
           st_sfc() %>% st_sf(geometry = .) %>% mutate(Facet = 1) %>% mutate(Colour = "#d51125") ) %>% 
  bind_rows(layer_2 %>% st_sample(size = 1000) %>%
              st_sfc() %>% st_sf(geometry = .) %>% mutate(Facet = 2)  %>% mutate(Colour = "#cc1100") )

anim <- data %>% ggplot() +
  aes(colour = Colour, fill = Colour) +
  geom_sf(aes(group = 1L)) +
  scale_colour_identity() +
  scale_fill_identity() +
  theme_void() +
  transition_states(Facet, wrap = FALSE) #+  shadow_mark() 

movie <- animate(anim, duration = 10, fps = 10, 
                 options(gganimate.dev_args = 
                           list(width = 1080, height = 1920)), #
                 renderer = av_renderer(audio = "./anim/Blue Dot Sessions - Tangle.mp3"))

anim_save("./anim//output.mp4", movie)

