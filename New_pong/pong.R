## Simulating the Legendary Pong Game in R

## Sound library
library(beepr) 

## Game parameters
skill <- 0.87 # Skill (0-1)
score <- 0
high.score <- 0

## Define playing field
#x11()
new_field <- function() {
  par(mar = rep(1, 4), bg = "black")
  plot.new()
  plot.window(xlim = c(0, 30), ylim = c(0, 30))
  # draw fence
  lines(
    x = c(1, 30, 30, 1),
    y = c(0, 0, 30, 30),
    type = "l",
    lwd = 5,
    col = "yellow"
  )
}
new_field()

## Draw initial ball position
new_ball <- function() {
  x_ball <- sample(5:25, 1)
  y_ball <- sample(5:25, 1)
  points(x_ball,
         y_ball,
         pch = 15,
         col = "white",
         cex = 2)
}

new_ball()

## Draw initial paddle position
new_paddle <- function(){
  psize = 4
  y_paddle <- sample(5:25, 1)
  lines(c(0, 0), c(y_paddle - (psize / 2), y_paddle + (psize / 2)), type = "l", lwd = 8, col = "white")
  }

new_paddle()


move_ball <- 



#library(ggplot2)
#ggplot() +
#  geom_line()


## Playing field boundaries (depends on cex)
xmin <- 0.5
xmax <- 29.4
ymin <- 0.5
ymax <- 29.4



## Paddle control
psize <- 4


## Set direction
dx <- runif(1, .5, 1)
dy <- runif(1, .5, 1)

## Game play

# inital paddle

# move paddle








#while (x > xmin - 1) {
#    sound <- 0 # Silence
#    Sys.sleep(.05) # Pause screen
# cover ball
    points(x, y, pch = 15, col = "red", cex = 2) # Erase ball
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
    # Caught by paddle?
    if (x < xmin & (y > ypaddle - (psize / 2)) & y < ypaddle + (psize / 2)) {
        if (x < xmin) x <- xmin
        dx <- -dx * runif(1, .9, 1.1)
        sound <- 2
        score <- score + 1
        }
# Draw ball
    points(x, y, pch = 15, col = "white", cex = 2)
   # if (sound !=0) beep(sound)
    # Move paddle
    if (runif(1, 0, 1) < skill) ypaddle <- ypaddle + dy # Imperfect follow
    # Draw paddle
    # Erase back line
    lines(c(0, 0), c(0, 30), type = "l", lwd = 8, col = "black")
    # Keep paddle in window
    if (ypaddle < (psize / 2)) ypaddle <- (psize / 2)
    if (ypaddle > 30 - (psize / 2)) ypaddle <- 30 - (psize / 2)
    # Draw paddle
    lines(c(0, 0), c(ypaddle - (psize / 2), ypaddle + (psize / 2)), type = "l", lwd = 8, col = "white")
#}
#beep(8)
#text(15,15, "GAME OVER", cex=5, col = "white")
#s <- ifelse(score == 1, "", "s")
#text(15,5, paste0(score, " Point", s), cex=3, col = "white")
