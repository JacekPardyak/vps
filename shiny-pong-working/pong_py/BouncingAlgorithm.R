#Bouncing Algorithm - www.101computing.net/bouncing-algorithm/
#from processing_py import *
#  from random import randint
library(tidyverse)
delay = .5

## Playing field boundaries (depends on cex)
field = I("x1, y1, x2, y2
                  1, 1, 30, 1
                  30, 1, 30, 30
            30, 30, 1, 30") %>% read_csv()



## initial position
t <- data.frame(x = sample(5:25, 1), y = sample(5:25, 1))

moveBall = function(df){
    df = tbl
    #df$x <- 25
    #df$y <- 14
  # initiation
  xmin <- 0.5
  xmax <- 29.4
  ymin <- 0.5
  ymax <- 29.4
  x <- df$x 
  y <- df$y

  dx <- runif(1, .5, 1)
  dy <- runif(1, .5, 1)
  
  # Move ball
  x <- x + dx
  y <- y + dy
  # Collision detection
  if (x > xmax) {
    dx <- -dx * runif(1, .9, 1.1) # Bounce
    if (x > xmin) x <- xmax # Boundary
    sound <- 10 # Set sound
  }
  if (y < ymin | y > ymax) {
    if (y < ymin) y <- ymin
    if (y > ymax) y <- ymax
    dy <- -dy * runif(1, .9, 1.1)
    sound <- 10
  }
  
  tbl <- data.frame(x = x, y = y)
  p <- tbl %>% ggplot() + aes(x = x, y = y) +
    geom_point(colour  = "#FFFFFF", size = 10, pch = 15) +
    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, colour = "#FFFFFF", group = 1, size = 5),
                 data = field) +
    scale_colour_identity() +
    scale_fill_identity() + 
   # theme_void() + 
    theme(legend.position = "none",
          panel.background = element_rect(fill = "#000000",
                                            colour  = "#FFFFFF",
                                            size = 10, 
                                            linetype = 1,
                                            color  = "#FFFFFF"))  + coord_fixed()
  print(p)
  return(tbl)
}


  
for (i in c(1:30)) {
  Sys.sleep(delay)
  t = moveBall(t)
}


#Draw ball  
ellipse(x,y,30,30)


draw = moveBall
run()
#This function from the processing library will call the setup() function
#once and then the moveBall() function every x ms to create the frame based animation.
