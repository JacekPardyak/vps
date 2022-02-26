library(tidyverse)
library(sf)

heart <- st_read("./ua/Wikimedia_Community_Logo-WOSP.dxf") #%>%
#  select(geometry) %>% 
#  mutate(facet = 0) #%>%  mutate(geometry =  geometry * 0.3) #%>% 
#mutate(geometry =  1 + geometry)
heart

heart %>% ggplot() + 
  geom_sf() +
  theme_void()