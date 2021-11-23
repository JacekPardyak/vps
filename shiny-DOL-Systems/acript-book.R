library(tidyverse)
library(gsubfn)

# definitions of the curves
curves = list(
  list(
    name = "Quadratic Koch island 1",
    axiom = "F-F-F-F",
    rules = list("F" = "F-F+F+FF-F-F+F"),
    angle = 90,
    depth = 3,
    alfa0 = 90
  ),
  list(
    name = "Quadratic Koch island 2",
    axiom = "F-F-F-F",
    rules = list("F" = "F+FF-FF-F-F+F+FF-F-F+F+FF+FF-F"),
    angle = 90,
    depth = 2,
    alfa0 = 90
  ),
  list(
    name = "Quadratic Koch island 3",
    axiom = "-F",
    rules = list("F" = "F+F-F-F+F"),
    angle = 90,
    depth = 4,
    alfa0 = 90
  ),
  list(
    name = "Curve 1",
    axiom = "F-F-F-F-F",
    rules = list("F" = "FF-F-F-F-F-F+F"),
    angle = 90,
    depth = 4,
    alfa0 = 90
  ),
  list(
    name = "Curve 2", # like it the most
    axiom = "F-F-F-F",
    rules = list("F" = "FF-F--F-F"),
    angle = 90,
    depth = 4,
    alfa0 = 90
  ),
  list(
    name = "Curve 3",
    axiom = "R",
    rules = list(
      "L" = "R+L+R",
      "R" = "L-R-L"),
    angle = 60,
    depth = 7,
    alfa0 = 0
  ),
  list(
    name = "Curve 4",
    axiom = "F+F+F+F",
    rules = list(
      "F" = "F+f-FF+F+FF+Ff+FF-f+FF-F-FF-Ff-FFF",
      "f" = "ffffff"),
    angle = 90,
    depth = 3,
    alfa0 = 90
  ),
  list(
    name = "Curve 5",
    axiom = "F-F-F-F",
    rules = list(
      "F" = "F-f+FF-F-FF-Ff-FF+f-FF+F+FF+Ff+FFF",
      "f" = "ffffff"),
    angle = 90,
    depth = 2,
    alfa0 = 90
  ),
  list(
    name = "Curve 6",
    axiom = "F+XF+F+XF",
    rules = list(
      "X" = "XF−F+F−XF+F+XF−F+F−X"),
    angle = 90,
    depth = 4,
    alfa0 = 90
  ),
  list(
    name = "Curve 7",
    axiom = "XF",
    rules = list(
      "X" = "X+YF++YF−FX−−FXFX−YF+",
      "Y" = "−FX+YFYF++YF+FX−−FX−Y"),
    angle = 60,
    depth = 6,
    alfa0 = 90
  )
)


title = "Curve 7"

curve = list.filter(curves, name==title)
  axiom = list.select(curve, axiom) %>% unlist
  rules = list.select(curve, rules)[[1]]$rules
  rules
  alfa0 = list.select(curve, alfa0) %>% unlist
  depth = list.select(curve, depth) %>% unlist
  angle = list.select(curve, angle) %>% unlist
  
  if(depth > 0) {
    for (i in 1:depth) {
      axiom = gsubfn(".", rules, axiom)
    }
  }
  
  actions = str_extract_all(axiom, "\\d*\\+|\\d*\\-|F|L|R|\\[|\\]|\\|") %>% unlist
  
  points=data.frame(x = 0, y = 0, alfa = alfa0)
  for (i in 1:length(actions))
  {
    if (actions[i] == "F" | actions[i] == "L" | actions[i] == "R")
    {
      row = list(
        x = points[nrow(points), "x"] + cos(points[nrow(points), "alfa"] * (pi/180)),
        y = points[nrow(points), "y"] + sin(points[nrow(points), "alfa"] * (pi/180)),
        alfa = points[nrow(points), "alfa"]
      )
      
      points <- points %>% bind_rows(row)
    }
    else{
      alfa = points[nrow(points), "alfa"]
      points[nrow(points), "alfa"] = eval(parse(text = paste0("alfa", actions[i], angle)))
    }
  }
  
points

# plot path
points %>% ggplot( aes(x, y)) + 
  geom_path() + 
  coord_fixed(ratio = 1) +
  theme_void()

# plot Simple Features
library(sf)
points %>% select(c("x", "y")) %>% as.matrix() %>% list() %>%
  st_multilinestring() %>%
  st_sfc() %>% st_sf(geometry = .) %>% mutate(facet = 0) %>% 
  mutate(Colour = "#000000") %>% ggplot() +
  aes(colour = Colour, fill = Colour)+
  scale_colour_identity() +
  scale_fill_identity() +
  geom_sf(aes(group = 1L))  +
  theme_void() 

## tests

axiom = "X"
rules = list("X" = "F−[[X]+X]+F[+FX]−X",
               "F" = "FF")

axiom = gsubfn(".", rules, axiom)
