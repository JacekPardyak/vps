library(tidyverse)
library(sf)
library(gganimate)

data <- st_read("https://raw.githubusercontent.com/JacekPardyak/vps/master/anim/logo/logo.dxf") %>%
  mutate(Layer = make.names(Layer))
data
layers <- data %>% data.frame() %>% distinct(Layer) %>% pull()
layers

for (i in c(1, 4, 5, 7, 8)) {
  layer <- paste("Layer", i, sep = ".")
  assign(layer, data %>% filter(Layer == layer) %>% st_union() %>% 
    st_polygonize() %>% first()  %>% first() %>%
    st_sfc() %>% st_sf(geometry = .) %>% mutate(Layer = layer))
}

Layer.1 %>% plot()
Layer.4 %>% plot()
Layer.5 %>% plot()
Layer.7 %>% plot()
Layer.8 %>% plot()


for (i in c(2, 3, 6, 9)) {
  layer <- paste("Layer", i, sep = ".")
  for (suf in c("in", "out")) {
    assign(paste("tmp", suf, sep = "."),  
           data %>% filter(Layer == paste(layer, suf, sep = ".")) %>% 
             st_union() %>% st_polygonize() )
  }
  assign(layer, st_difference(tmp.out, tmp.in)  %>%
           st_sfc() %>% st_sf(geometry = .) %>% mutate(Layer = layer))
}

Layer.2 %>% plot()
Layer.3 %>% plot()
Layer.6 %>% plot()
Layer.9 %>% plot()

data <- st_sfc() %>% st_sf(geometry = .)

for (i in c(1:9)) {
  data <- data %>% bind_rows(get(paste("Layer", i, sep = "."))) 
}

data <- data %>% mutate(Colour = if_else(Layer == "Layer.5", "#17ace3ff", "#1c3f6eff"))

data %>% ggplot() +
  aes(colour = Colour, fill = Colour) +
  geom_sf(aes(group = 1L)) +
  scale_colour_identity() +
  scale_fill_identity() +
  theme_void() +
  transition_states(Layer, wrap = FALSE) #+  shadow_mark() 


data.cloud <- data %>% mutate(geometry = geometry %>% st_sample(size = 1000))  %>%
  st_sfc() %>% st_sf(geometry = .)
data.cloud


data.cloud  <- st_sfc() %>% st_sf(geometry = .)
for (i in c(1:9)) {
  #i = 1
  area <- get(paste("Layer", i, sep = ".")) %>% st_area() %>% round()
  tmp <- get(paste("Layer", i, sep = ".")) %>% st_sample(size = area) %>%
    st_sfc() %>% st_sf(geometry = .) %>% mutate(Layer = paste("Layer", i, sep = "."))
  data.cloud <- data.cloud %>% bind_rows(tmp) 
}

data.cloud <- data.cloud %>% mutate(Colour = if_else(Layer == "Layer.5", "#17ace3ff", "#1c3f6eff"))


anim <- data.cloud %>% ggplot() +
  aes(colour = Colour, fill = Colour) +
  geom_sf(aes(group = 1L)) +
  scale_colour_identity() +
  scale_fill_identity() +
  theme_void() +
  transition_states(Layer, wrap = FALSE) +
  shadow_mark() 

movie <- animate(anim, duration = 35, fps = 10, 
                 options(gganimate.dev_args = 
                           list(width = 1080, height = 1920)), #
                 renderer = av_renderer(audio = "./anim/Blue Dot Sessions - Tangle.mp3"))

anim_save("./anim//output.mp4", movie)

