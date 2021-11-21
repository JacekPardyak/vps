
if (!require("units")) install.packages("units")
if (!require("geojsonio")) install.packages("geojsonio")
if (!require("sf")) install.packages("sf")
if (!require("gganimate")) install.packages("gganimate")
if (!require("transformr")) install.packages("transformr")
if (!require("plotly")) install.packages("plotly")
if (!require("av")) install.packages("av")
if (!require("reshape2")) install.packages("reshape2")
library(tidyverse)
library(gganimate)
library(reshape2)

# Kronecker power of a matrix, where: m - initial matrix, n - power.
matkronpow <- function(m, n) {
  if (n<2) {return (m)};
  r = m; n = n-1;
  for(i in 1:n) {r = r%x%m};
  return (r);
}

M <- matrix(c(1,1,1,1,1,1,1, 1,0,0,0,0,0,1, 1,0,1,1,1,0,1, 1,0,1,1,1,0,1,
 1,0,1,1,1,0,1, 1,0,0,0,0,0,1, 1,1,1,1,1,1,1), ncol=7, nrow=7, byrow=TRUE);

M %>% melt() %>% filter(value != 0) %>% ggplot() +
  aes(x = Var2, y = Var1) + 
  geom_raster(aes(fill=value))  +
  theme_void() + 
  scale_fill_gradient(low="gray", high="black") + 
  theme(legend.position = "none")

matkronpow(M, n = 3) %>% melt() %>% filter(value != 0) %>% ggplot() +
  aes(x = Var2, y = Var1) + 
  geom_raster(aes(fill=value))  +
  theme_void() + 
  scale_fill_gradient(low="gray", high="black") + 
  theme(legend.position = "none")

brownianTree <- function(m, n)
{
  M <- matrix(c(0), ncol=m, nrow=m, byrow=TRUE);
  # Seed in center
  x <- m%/%2; y <- m%/%2;
  M[x,y]=1;

  # Main loops
  for (i in 1:n) {
    if(i>1) {
      x <- sample(1:m, 1, replace=FALSE)
      y <- sample(1:m, 1, replace=FALSE)}
    while(1) {
      ox = x; oy = y;
      x <- x + sample(-1:1, 1, replace=FALSE);
      y <- y + sample(-1:1, 1, replace=FALSE);
      if(x<=m && y<=m && x>0 && y>0 && M[x,y]) 
        {if(ox<=m && oy<=m && ox>0 && oy>0) {M[ox,oy]=1; break}}
      if(!(x<=m && y<=m && x>0 && y>0)) {break}
    }
  }
  return(M)
}

polygon <- function(x, y) {
       matrix(c(x-0.5, y-0.5,
                x-0.5, y+0.5,
                x+0.5, y+0.5,
                x+0.5, y-0.5,
                x-0.5, y-0.5), ncol = 2, byrow = T) %>%
      list() %>% st_polygon()
}

B <- brownianTree(m = 400, n  = 100000)  %>% melt() %>% #100000
  filter(value != 0) 

data <- st_sfc() %>% st_sf(geometry = .)
for(i in c(1:nrow(B))){
  data <- data %>% bind_rows(polygon(B[i, 'Var1'], B[i, 'Var2']) %>%
                         st_sfc() %>% st_sf(geometry = .))
}

data <- data %>% mutate(Colour = "#17ace3ff") %>% 
  mutate(Facet = row_number())
data %>% save(file = "./fractal/output.RData")

# define animation
load(file = "./fractal/output.RData")
anim <- 
  data %>% ggplot() +
  aes(colour = Colour, fill = Colour) +
  geom_sf(aes(group = 1L)) +
  scale_colour_identity() +
  scale_fill_identity() +
  theme_void() +
  transition_states(Facet, wrap = FALSE) +
  shadow_mark()

# download audio
#url = "https://github.com/JacekPardyak/vps/raw/master/anim/Blue%20Dot%20Sessions%20-%20Tangle.mp3"
#audio = tempfile(tmpdir = tempdir(), fileext = ".mp3")
#download.file(url, audio)

audio = "C://Users//jacek//Documents//rebus//morawiecki//Rudy Trubitt - fort point.mp3"
movie <- animate(anim, duration = 35, fps = 10, 
                 options(gganimate.dev_args = 
                           list(width = 1080 , height = 1920)),
                 renderer = av_renderer(audio = audio))
anim_save("./fractal/output.mp4", movie)

